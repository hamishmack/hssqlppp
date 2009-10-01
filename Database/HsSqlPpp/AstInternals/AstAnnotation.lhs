Copyright 2009 Jake Wheat

The annotation data types and utilities for working with them.

Annotations are used to store source positions, types, errors,
warnings, environment deltas, information, and other stuff a client might
want to use when looking at an ast. Internal annotations which are
used in the type-checking/ annotation process use the attribute
grammar code and aren't exposed.

> {-# LANGUAGE ExistentialQuantification, DeriveDataTypeable,ScopedTypeVariables,
>   RankNTypes,FlexibleContexts #-}
> {-# OPTIONS_HADDOCK hide #-}

> module Database.HsSqlPpp.AstInternals.AstAnnotation
>     (
>      Annotation
>     ,AnnotationElement(..)
>     --,stripAnnotations
>     ,getTopLevelTypes
>     ,getTopLevelInfos
>     ,getTopLevelEnvUpdates
>     ,getTypeAnnotation
>     ,getTypeErrors
>     ,stripAnnotations
>     ,updateAnnotation
>     ,getAnnotation
>     ,getAnnotations
>     --,getTypeErrors
>     --,pack
>     ,StatementInfo(..)
>     ,getSIAnnotation
>     ) where

> import Data.Generics
> import Data.Maybe
> import Control.Arrow

> import Database.HsSqlPpp.AstInternals.TypeType
> import Database.HsSqlPpp.AstInternals.Environment.EnvironmentInternal

> -- | Annotation type - one of these is attached to most of the
> -- data types used in the ast.
> type Annotation = [AnnotationElement]

> -- | the elements of an annotation. Source positions are generated by
> -- the parser, the rest come from the separate ast annotation process.
> data AnnotationElement = SourcePos String Int Int
>                        | TypeAnnotation Type
>                        | TypeErrorA TypeError
>                        | StatementInfoA StatementInfo
>                        | EnvUpdates [EnvironmentUpdate]
>                          deriving (Eq, Show,Typeable,Data)

Use syb to pull annotation values from an ast.

I like to cut and paste code from the internet which I don't
understand, then keep changing it till it compiles and passes the tests.


> -- | run through the ast, and pull the type annotation from each
> -- of the top level items.
> getTopLevelTypes :: Data a => [a] -> [Type]
> getTopLevelTypes st =
>     getTopLevelXs typeAnnot st
>     where
>       typeAnnot :: Annotation -> [Type]
>       typeAnnot (x:xs) = case x of
>                                 TypeAnnotation t -> [t]
>                                 _ -> typeAnnot xs
>       typeAnnot [] = [TypeCheckFailed] -- error "couldn't find type annotation"

> getTopLevelXs :: forall a b a1.
>                  (Data a1, Typeable b) =>
>                 (b -> [a]) -> a1 -> [a]
> getTopLevelXs st = everythingOne (++) $ mkQ [] st


> getTypeAnnotation :: Data a => a -> Type
> getTypeAnnotation st =
>     case getTopLevelX typeAnnot st of
>       x:_ -> x
>       [] -> TypeCheckFailed
>     where
>       typeAnnot :: Annotation -> [Type]
>       typeAnnot (x:xs) = case x of
>                                 TypeAnnotation t -> [t]
>                                 _ -> typeAnnot xs
>       typeAnnot [] = [TypeCheckFailed]

> getTopLevelX :: forall a b a1.
>                 (Data a1, Typeable b) =>
>                (b -> [a]) -> a1 -> [a]
> getTopLevelX p = everythingOne (++) (mkQ [] p)


 > everythingTwo :: (r -> r -> r) -> GenericQ r -> GenericQ r
 > everythingTwo k f x
 >  = foldl k (f x) (gmapQ (everythingOne k f) x)

> everythingZero :: (r -> r -> r) -> GenericQ r -> GenericQ r
> everythingZero k f x
>  = foldl k (f x) (gmapQ f x)

> everythingOne :: (r -> r -> r) -> GenericQ r -> GenericQ r
> everythingOne k f x
>  = foldl k (f x) (gmapQ (everythingZero k f) x)

> getSIAnnotation :: Annotation -> [Maybe StatementInfo]
> getSIAnnotation (x:xs) = case x of
>                                 StatementInfoA t -> [Just t]
>                                 _ -> getSIAnnotation xs
> getSIAnnotation []  = [Nothing]

> getEuAnnotation :: Annotation -> [[EnvironmentUpdate]]
> getEuAnnotation (x:xs) = case x of
>                                 EnvUpdates t -> t:getEuAnnotation xs
>                                 _ -> getEuAnnotation xs
> getEuAnnotation [] = []


> -- | Run through the ast given and return a list of statementinfos
> -- from the top level items.
> getTopLevelInfos :: Data a => [a] -> [Maybe StatementInfo]
> getTopLevelInfos = getTopLevelXs getSIAnnotation

> getTopLevelEnvUpdates ::  Data a => [a] -> [[EnvironmentUpdate]]
> getTopLevelEnvUpdates = getTopLevelXs getEuAnnotation

> data StatementInfo = DefaultStatementInfo Type
>                    | SelectInfo Type
>                    | InsertInfo String [(String,Type)]
>                    | UpdateInfo String [(String,Type)]
>                    | DeleteInfo String
>                      deriving (Eq,Show,Typeable,Data)

todo:
add environment deltas to statementinfo

question:
if a node has no source position e.g. the all in select all or select
   distinct may correspond to a token or may be synthesized as the
   default if neither all or distinct is present. Should this have the
   source position of where the token would have appeared, should it
   inherit it from its parent, should there be a separate ctor to
   represent a fake node with no source position?


hack job, often not interested in the source positions when testing
the asts produced, so this function will reset all the source
positions to empty ("", 0, 0) so we can compare them for equality, etc.
without having to get the positions correct.

> -- | strip all the annotations from a tree. E.g. can be used to compare
> -- two asts are the same, ignoring any source position annotation differences.
> stripAnnotations :: (Data a) => a -> a
> stripAnnotations = everywhere (mkT stripAn)
>                    where
>                      stripAn :: [AnnotationElement] -> [AnnotationElement]
>                      stripAn _ = []


> -- | runs through the ast given and returns a list of all the type errors
> -- in the ast. Recurses into all ast nodes to find type errors.
> -- This is the function to use to see if an ast has passed the type checking process.
> -- Returns a Maybe SourcePos and the list of type errors for each node which has one or
> -- more type errors.
> getTypeErrors :: (Data a) => a -> [(Maybe AnnotationElement,[TypeError])]
> getTypeErrors sts =
>     filter (\(_,te) -> not $ null te) $ map (gtsp &&& gte) $ getAnnotations sts
>     where
>       gte (a:as) = case a of
>                     TypeErrorA e -> e:gte as
>                     _ -> gte as
>       gte _ = []

>       gtsp (a:as) = case a of
>                     s@(SourcePos _ _ _) -> Just s
>                     _ -> gtsp as
>       gtsp _ = Nothing



> updateAnnotation :: forall a.(Data a) =>
>                   (Annotation -> Annotation) -> a -> a
> updateAnnotation f = oneLevel (mkT f)

> oneLevel :: (forall a.Data a => a -> a)
>          -> (forall a.Data a => a -> a)
> oneLevel f = gmapT f

> getAnnotation :: forall a.(Data a) => a -> Annotation
> getAnnotation a =
>   case oneLevelQ (mkQ [] f) a of
>     an:_ -> an
>     [] -> []
>   where
>     f :: Annotation -> Annotation
>     f = id

> oneLevelQ :: forall a.Data a => forall u. (forall d. (Data d) => d -> u) -> a -> [u]
> oneLevelQ = gmapQ


> getAnnotations :: forall a.(Data a) =>
>                   a -> [Annotation]
> getAnnotations = listifyWholeLists (\(_::Annotation) -> True)

> listifyWholeLists :: Typeable b => ([b] -> Bool) -> GenericQ [[b]]
> listifyWholeLists blp = flip (synthesize id (.) (mkQ id (\bl _ -> if blp bl then (bl:) else id))) []