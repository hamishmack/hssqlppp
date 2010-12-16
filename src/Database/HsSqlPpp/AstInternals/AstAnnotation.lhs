
The annotation data types and utilities for working with them.

Annotations are used to store source positions, types, errors,
warnings, catalog deltas, information, and other stuff a client might
want to use when looking at an ast. Internal annotations which are
used in the type-checking/ annotation process use the attribute
grammar code and aren't exposed.

> {-# LANGUAGE ExistentialQuantification, DeriveDataTypeable,ScopedTypeVariables,
>   RankNTypes,FlexibleContexts #-}
>
> module Database.HsSqlPpp.AstInternals.AstAnnotation
>     (
>      Annotation(..)
>     ,SourcePosition
>     ,StatementType
>     ,getAnnotation
>     ,updateAnnotations
>     ,getAnnotations
>     ,emptyAnnotation
>     ,getTypeAnnotation
>     ,updateAnnotation
>     ) where
>
> import Data.Data
> ---import Control.Arrow
> import Data.Generics.Uniplate.Data
> --import Debug.Trace
>
> import Database.HsSqlPpp.AstInternals.TypeType
> import Database.HsSqlPpp.AstInternals.Catalog.CatalogInternal
> --import Database.HsSqlPpp.Utils.Utils
>
> type SourcePosition = (String,Int,Int)
> type StatementType = ([Type],[(String,Type)])

> -- | Annotation type - one of these is attached to most of the
> -- data types used in the ast.
> data Annotation = Annotation {asrc :: Maybe SourcePosition
>                              ,atype :: Maybe Type -- type of the node, nothing if type error prevents determining type
>                              ,errs :: [TypeError] -- any type errors
>                              ,stType :: Maybe StatementType -- 'statement type' - used for getting the in and out types of a parameterized statement
>                              ,catUpd :: [CatalogUpdate] -- any catalog updates that a ddl statement produces
>                              ,fnProt :: Maybe FunctionPrototype -- the matched function prototype for a funcall
>                              ,infType :: Maybe Type} -- 'inferred' type - fake type inference used for getting the type of ? placeholders in parameterized statements
>                   deriving (Eq, Show,Typeable,Data)
>
> emptyAnnotation :: Annotation
> emptyAnnotation = Annotation Nothing Nothing [] Nothing [] Nothing Nothing

> -- | get the annotation for the root element of the tree passed
> getAnnotation :: Data a => a -> Annotation
> getAnnotation = head . childrenBi

> getAnnotations :: Data a => a -> [Annotation]
> getAnnotations st = [x | x <- universeBi st]

> updateAnnotations :: Data a => (Annotation -> Annotation) -> a -> a
> updateAnnotations = transformBi

> getTypeAnnotation :: Data a => a -> Maybe Type
> getTypeAnnotation = atype . getAnnotation





Use syb/uniplate to pull annotation values from an ast.

I like to cut and paste code from the internet which I don't
understand, then keep changing it till it compiles and passes the tests.

> {- -- | run through the ast, and pull the type annotation from each
> -- of the top level items.
> getTypeAnnotation :: (Show a, Data a) => a -> Type
> getTypeAnnotation st =
>     typeAnnot $ getTopLevelAnnotation st
>     where
>       typeAnnot :: Annotation -> Type
>       typeAnnot (x:xs) = case x of
>                                 TypeAnnotation t -> t
>                                 _ -> typeAnnot xs
>       typeAnnot [] = TypeCheckFailed -- error "couldn't find type annotation"
>
> -- | Run through the ast given and return a list of statementtypes
> -- from the top level items.
> getTopLevelInfos :: Data a => [a] -> [Maybe StatementType]
> getTopLevelInfos = map (getSIAnnotation . getTopLevelAnnotation)
>
> getTopLevelCatUpdates ::  Data a => [a] -> [[CatalogUpdate]]
> getTopLevelCatUpdates = map (getEuAnnotation . getTopLevelAnnotation)
>

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

~~~~
question:
if a node has no source position e.g. the all in select all or select
   distinct may correspond to a token or may be synthesized as the
   default if neither all or distinct is present. Should this have the
   source position of where the token would have appeared, should it
   inherit it from its parent, should there be a separate ctor to
   represent a fake node with no source position?

~~~~

hack job, often not interested in the source positions when testing
the asts produced, so this function will reset all the source
positions to empty ("", 0, 0) so we can compare them for equality, etc.
without having to get the positions correct.

> -}

 > -- | strip all the annotations from a tree. E.g. can be used to compare
 > -- two asts are the same, ignoring any source position annotation differences.
 > stripAnnotations :: Data a => a -> a
 > stripAnnotations = filterAnnotations (const False)
 >

 > filterAnnotations :: Data a => (Annotation -> Bool) -> a -> a
 > filterAnnotations f = transformBi (filter f)

>
> -- | Update all the annotations in a tree using the function supplied
> updateAnnotation :: Data a => (Annotation -> Annotation) -> a -> a
> updateAnnotation f = transformBi f --gmapT (mkT f)
>


 >   case gmapQ (mkQ [] f) a of
 >     an:_ -> an
 >     [] -> []
 >   where
 >     f :: Annotation -> Annotation
 >     f = id
 >

 >
 > getAnnotations :: Data a => a -> [Annotation]
 > getAnnotations = listifyWholeLists (\(_::Annotation) -> True)
 >

> {-getSIAnnotation :: Annotation -> Maybe StatementType
> getSIAnnotation (x:xs) = case x of
>                                 StatementTypeA t -> Just t
>                                 _ -> getSIAnnotation xs
> getSIAnnotation []  = Nothing
>
> getEuAnnotation :: Annotation -> [CatalogUpdate]
> getEuAnnotation (x:xs) = case x of
>                                 CatUpdates t -> t
>                                 _ -> getEuAnnotation xs
> getEuAnnotation [] = []-}

-------------------------------------------------------------------------------

utils

 > listifyWholeLists :: Typeable b => ([b] -> Bool) -> GenericQ [[b]]
 > listifyWholeLists blp = flip (synthesize id (.) (mkQ id (\bl _ -> if blp bl then (bl:) else id))) []

this might need to be maybe and change head?

 > getTopLevelAnnotation :: Data a => a -> Annotation
 > getTopLevelAnnotation st = head $ childrenBi st
