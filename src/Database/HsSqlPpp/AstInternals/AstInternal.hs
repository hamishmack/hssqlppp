

-- UUAGC 0.9.29 (/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/)
module Database.HsSqlPpp.AstInternals.AstInternal(
    -- {-# LANGUAGE DeriveDataTypeable,RankNTypes,ScopedTypeVariables #-}
    QueryExpression(..)
   ,WithQueryList
   ,WithQuery(..)
   ,CombineType(..)
   ,Direction(..)
   ,Distinct(..)

   ,SelectList(..)
   ,SelectItem(..)

   ,TableRef(..)
   ,TableAlias(..)
   ,JoinExpression(..)
   ,JoinType(..)
   ,Natural(..)

   ,ScalarExpression(..)
   ,LiftFlavour(..)
   ,InList(..)
   ,FrameClause(..)
   ,TypeName(..)

   ,SelectItemList
   ,TableRefList
   ,MaybeExpression
   ,MaybeBoolExpression
   ,OnExpr
   ,ExpressionList
   ,ExpressionListList
   ,ExpressionDirectionPair
   ,ExpressionDirectionPairList
   ,CaseExpressionListExpressionPair
   ,CaseExpressionListExpressionPairList

   ,typeCheckScalarExpr
   ,typeCheckQueryExpr
   ,typeCheckQueryExprs

   ,canonicaliseIdentifiers
) where

import Data.Maybe
import Data.List
import Control.Applicative
import Data.Generics
import Data.Char
import Control.Monad.State

import Data.Generics.PlateData
import Debug.Trace

import Database.HsSqlPpp.AstInternals.TypeType
import Database.HsSqlPpp.AstInternals.TypeChecking.TypeConversion
import Database.HsSqlPpp.AstInternals.AstAnnotation
import Database.HsSqlPpp.AstInternals.Catalog.CatalogInternal
--import Database.HsSqlPpp.AstInternals.TypeChecking.LocalBindings
import Database.HsSqlPpp.Utils.Utils
--import Database.HsSqlPpp.AstInternals.TypeChecking.ErrorUtils


{-# LINE 145 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.ag" #-}

data CombineType = Except | Union | Intersect | UnionAll
                   deriving (Show,Eq,Typeable,Data)

data Direction = Asc | Desc
                 deriving (Show,Eq,Typeable,Data)

data Distinct = Distinct | Dupes
                deriving (Show,Eq,Typeable,Data)
{-# LINE 77 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-}

{-# LINE 188 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.ag" #-}

data TableAlias = NoAlias
                | TableAlias String --alias:String
                | FullAlias String [String] -- alias:String cols:{[String]}
                  deriving (Show,Eq,Typeable,Data)
{-# LINE 85 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-}

{-# LINE 198 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.ag" #-}


data JoinType = Inner | LeftOuter| RightOuter | FullOuter | Cross
                deriving (Show,Eq,Typeable,Data)

data Natural = Natural | Unnatural
               deriving (Show,Eq,Typeable,Data)

{-# LINE 96 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-}

{-# LINE 257 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.ag" #-}

data LiftFlavour = LiftAny | LiftAll
                   deriving (Show,Eq,Typeable,Data)
{-# LINE 102 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-}

{-# LINE 266 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.ag" #-}

data FrameClause = FrameUnboundedPreceding
                 | FrameUnboundedFull
                 | FrameRowsUnboundedPreceding
                   deriving (Show,Eq,Typeable,Data)
{-# LINE 110 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-}

{-# LINE 285 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.ag" #-}


typeCheckScalarExpr :: Catalog -> ScalarExpression SourcePosition -> ScalarExpression Annotation
typeCheckScalarExpr = undefined

typeCheckQueryExpr :: Catalog -> QueryExpression SourcePosition -> QueryExpression Annotation
typeCheckQueryExpr = undefined

typeCheckQueryExprs :: Catalog -> [QueryExpression SourcePosition] -> [QueryExpression Annotation]
typeCheckQueryExprs = undefined

{-# LINE 124 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-}

{-# LINE 56 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}


data Environment = Environment Int

qualifyID :: Environment -> String -> (Maybe String,String)
qualifyID = undefined

makeEnvironment :: String -- range qualifier
                -> [String] -- attribute names
                -> Environment
makeEnvironment = undefined

emptyEnvironment :: Environment
emptyEnvironment = undefined

unimplementedEnvironment :: Environment
unimplementedEnvironment = undefined


joinEnvironments :: Environment -> Environment -> Environment
joinEnvironments = undefined

aliasEnvironmentRangeName :: Environment -> String -> Environment
aliasEnvironmentRangeName = undefined

expandStar :: Environment -> Maybe String --qualifier
           -> [String]
expandStar = undefined

{-# LINE 156 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-}

{-# LINE 184 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}




{-# LINE 163 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-}

{-# LINE 195 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}

canonicaliseIdentifiers :: Catalog -> [QueryExpression a] -> [QueryExpression a]
canonicaliseIdentifiers cat sts =
    let t = sem_Root (Root sts)
        ta = wrap_Root t Inh_Root {cat_Inh_Root = cat}
        tl = canonicalisedIdentifiersTree_Syn_Root ta
    in case tl of
         Root r -> r

{-# LINE 175 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-}
-- CaseExpressionListExpressionPair ----------------------------
{-
   visit 0:
      inherited attribute:
         cat                  : Catalog
      synthesized attribute:
         canonicalisedIdentifiersTree : SELF 
   alternatives:
      alternative Tuple:
         child x1             : ExpressionList a
         child x2             : ScalarExpression a
         visit 0:
            local canonicalisedIdentifiersTree : _
-}
type CaseExpressionListExpressionPair a  = ( ExpressionList a ,ScalarExpression a )
-- cata
sem_CaseExpressionListExpressionPair :: (CaseExpressionListExpressionPair) (a)  ->
                                        (T_CaseExpressionListExpressionPair) (a) 
sem_CaseExpressionListExpressionPair ( x1,x2)  =
    (sem_CaseExpressionListExpressionPair_Tuple (sem_ExpressionList x1 ) (sem_ScalarExpression x2 ) )
-- semantic domain
type T_CaseExpressionListExpressionPair a  = (Catalog) ->
                                             ( CaseExpressionListExpressionPair a )
data Inh_CaseExpressionListExpressionPair a  = Inh_CaseExpressionListExpressionPair {cat_Inh_CaseExpressionListExpressionPair :: (Catalog)}
data Syn_CaseExpressionListExpressionPair a  = Syn_CaseExpressionListExpressionPair {canonicalisedIdentifiersTree_Syn_CaseExpressionListExpressionPair :: CaseExpressionListExpressionPair a }
wrap_CaseExpressionListExpressionPair :: (T_CaseExpressionListExpressionPair) (a)  ->
                                         (Inh_CaseExpressionListExpressionPair) (a)  ->
                                         (Syn_CaseExpressionListExpressionPair) (a) 
wrap_CaseExpressionListExpressionPair sem (Inh_CaseExpressionListExpressionPair _lhsIcat )  =
    (let ( _lhsOcanonicalisedIdentifiersTree) =
             (sem _lhsIcat )
     in  (Syn_CaseExpressionListExpressionPair _lhsOcanonicalisedIdentifiersTree ))
sem_CaseExpressionListExpressionPair_Tuple :: (T_ExpressionList) (a)  ->
                                              (T_ScalarExpression) (a)  ->
                                              (T_CaseExpressionListExpressionPair) (a) 
sem_CaseExpressionListExpressionPair_Tuple (x1_ :: (Catalog) ->
                                                   ( ExpressionList a )) (x2_ :: (Catalog) ->
                                                                                 (Environment) ->
                                                                                 ( ScalarExpression a ))  =
    (\ (_lhsIcat :: (Catalog)) ->
         (let _lhsOcanonicalisedIdentifiersTree :: CaseExpressionListExpressionPair a 
              _x1Ocat :: (Catalog)
              _x2Ocat :: (Catalog)
              _x2Oenv :: (Environment)
              _x1IcanonicalisedIdentifiersTree :: ExpressionList a 
              _x2IcanonicalisedIdentifiersTree :: ScalarExpression a 
              -- self rule
              _canonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   (_x1IcanonicalisedIdentifiersTree,_x2IcanonicalisedIdentifiersTree)
                   {-# LINE 226 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- self rule
              _lhsOcanonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _canonicalisedIdentifiersTree
                   {-# LINE 231 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- copy rule (down)
              _x1Ocat =
                  ({-# LINE 190 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _lhsIcat
                   {-# LINE 236 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- copy rule (down)
              _x2Ocat =
                  ({-# LINE 190 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _lhsIcat
                   {-# LINE 241 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- copy rule (chain)
              _x2Oenv =
                  ({-# LINE 146 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   error "missing rule: CaseExpressionListExpressionPair.Tuple.x2.env"
                   {-# LINE 246 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              ( _x1IcanonicalisedIdentifiersTree) =
                  (x1_ _x1Ocat )
              ( _x2IcanonicalisedIdentifiersTree) =
                  (x2_ _x2Ocat _x2Oenv )
          in  ( _lhsOcanonicalisedIdentifiersTree)))
-- CaseExpressionListExpressionPairList ------------------------
{-
   visit 0:
      inherited attribute:
         cat                  : Catalog
      synthesized attribute:
         canonicalisedIdentifiersTree : SELF 
   alternatives:
      alternative Cons:
         child hd             : CaseExpressionListExpressionPair a
         child tl             : CaseExpressionListExpressionPairList a
         visit 0:
            local canonicalisedIdentifiersTree : _
      alternative Nil:
         visit 0:
            local canonicalisedIdentifiersTree : _
-}
type CaseExpressionListExpressionPairList a  = [CaseExpressionListExpressionPair a ]
-- cata
sem_CaseExpressionListExpressionPairList :: (CaseExpressionListExpressionPairList) (a)  ->
                                            (T_CaseExpressionListExpressionPairList) (a) 
sem_CaseExpressionListExpressionPairList list  =
    (Prelude.foldr sem_CaseExpressionListExpressionPairList_Cons sem_CaseExpressionListExpressionPairList_Nil (Prelude.map sem_CaseExpressionListExpressionPair list) )
-- semantic domain
type T_CaseExpressionListExpressionPairList a  = (Catalog) ->
                                                 ( CaseExpressionListExpressionPairList a )
data Inh_CaseExpressionListExpressionPairList a  = Inh_CaseExpressionListExpressionPairList {cat_Inh_CaseExpressionListExpressionPairList :: (Catalog)}
data Syn_CaseExpressionListExpressionPairList a  = Syn_CaseExpressionListExpressionPairList {canonicalisedIdentifiersTree_Syn_CaseExpressionListExpressionPairList :: CaseExpressionListExpressionPairList a }
wrap_CaseExpressionListExpressionPairList :: (T_CaseExpressionListExpressionPairList) (a)  ->
                                             (Inh_CaseExpressionListExpressionPairList) (a)  ->
                                             (Syn_CaseExpressionListExpressionPairList) (a) 
wrap_CaseExpressionListExpressionPairList sem (Inh_CaseExpressionListExpressionPairList _lhsIcat )  =
    (let ( _lhsOcanonicalisedIdentifiersTree) =
             (sem _lhsIcat )
     in  (Syn_CaseExpressionListExpressionPairList _lhsOcanonicalisedIdentifiersTree ))
sem_CaseExpressionListExpressionPairList_Cons :: (T_CaseExpressionListExpressionPair) (a)  ->
                                                 (T_CaseExpressionListExpressionPairList) (a)  ->
                                                 (T_CaseExpressionListExpressionPairList) (a) 
sem_CaseExpressionListExpressionPairList_Cons (hd_ :: (Catalog) ->
                                                      ( CaseExpressionListExpressionPair a )) (tl_ :: (Catalog) ->
                                                                                                      ( CaseExpressionListExpressionPairList a ))  =
    (\ (_lhsIcat :: (Catalog)) ->
         (let _lhsOcanonicalisedIdentifiersTree :: CaseExpressionListExpressionPairList a 
              _hdOcat :: (Catalog)
              _tlOcat :: (Catalog)
              _hdIcanonicalisedIdentifiersTree :: CaseExpressionListExpressionPair a 
              _tlIcanonicalisedIdentifiersTree :: CaseExpressionListExpressionPairList a 
              -- self rule
              _canonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   (:) _hdIcanonicalisedIdentifiersTree _tlIcanonicalisedIdentifiersTree
                   {-# LINE 303 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- self rule
              _lhsOcanonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _canonicalisedIdentifiersTree
                   {-# LINE 308 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- copy rule (down)
              _hdOcat =
                  ({-# LINE 190 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _lhsIcat
                   {-# LINE 313 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- copy rule (down)
              _tlOcat =
                  ({-# LINE 190 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _lhsIcat
                   {-# LINE 318 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              ( _hdIcanonicalisedIdentifiersTree) =
                  (hd_ _hdOcat )
              ( _tlIcanonicalisedIdentifiersTree) =
                  (tl_ _tlOcat )
          in  ( _lhsOcanonicalisedIdentifiersTree)))
sem_CaseExpressionListExpressionPairList_Nil :: (T_CaseExpressionListExpressionPairList) (a) 
sem_CaseExpressionListExpressionPairList_Nil  =
    (\ (_lhsIcat :: (Catalog)) ->
         (let _lhsOcanonicalisedIdentifiersTree :: CaseExpressionListExpressionPairList a 
              -- self rule
              _canonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   []
                   {-# LINE 332 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- self rule
              _lhsOcanonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _canonicalisedIdentifiersTree
                   {-# LINE 337 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
          in  ( _lhsOcanonicalisedIdentifiersTree)))
-- ExpressionDirectionPair -------------------------------------
{-
   visit 0:
      inherited attribute:
         cat                  : Catalog
      synthesized attribute:
         canonicalisedIdentifiersTree : SELF 
   alternatives:
      alternative Tuple:
         child x1             : ScalarExpression a
         child x2             : {Direction}
         visit 0:
            local canonicalisedIdentifiersTree : _
-}
type ExpressionDirectionPair a  = ( ScalarExpression a ,(Direction))
-- cata
sem_ExpressionDirectionPair :: (ExpressionDirectionPair) (a)  ->
                               (T_ExpressionDirectionPair) (a) 
sem_ExpressionDirectionPair ( x1,x2)  =
    (sem_ExpressionDirectionPair_Tuple (sem_ScalarExpression x1 ) x2 )
-- semantic domain
type T_ExpressionDirectionPair a  = (Catalog) ->
                                    ( ExpressionDirectionPair a )
data Inh_ExpressionDirectionPair a  = Inh_ExpressionDirectionPair {cat_Inh_ExpressionDirectionPair :: (Catalog)}
data Syn_ExpressionDirectionPair a  = Syn_ExpressionDirectionPair {canonicalisedIdentifiersTree_Syn_ExpressionDirectionPair :: ExpressionDirectionPair a }
wrap_ExpressionDirectionPair :: (T_ExpressionDirectionPair) (a)  ->
                                (Inh_ExpressionDirectionPair) (a)  ->
                                (Syn_ExpressionDirectionPair) (a) 
wrap_ExpressionDirectionPair sem (Inh_ExpressionDirectionPair _lhsIcat )  =
    (let ( _lhsOcanonicalisedIdentifiersTree) =
             (sem _lhsIcat )
     in  (Syn_ExpressionDirectionPair _lhsOcanonicalisedIdentifiersTree ))
sem_ExpressionDirectionPair_Tuple :: (T_ScalarExpression) (a)  ->
                                     (Direction) ->
                                     (T_ExpressionDirectionPair) (a) 
sem_ExpressionDirectionPair_Tuple (x1_ :: (Catalog) ->
                                          (Environment) ->
                                          ( ScalarExpression a )) (x2_ :: (Direction))  =
    (\ (_lhsIcat :: (Catalog)) ->
         (let _lhsOcanonicalisedIdentifiersTree :: ExpressionDirectionPair a 
              _x1Ocat :: (Catalog)
              _x1Oenv :: (Environment)
              _x1IcanonicalisedIdentifiersTree :: ScalarExpression a 
              -- self rule
              _canonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   (_x1IcanonicalisedIdentifiersTree,x2_)
                   {-# LINE 386 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- self rule
              _lhsOcanonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _canonicalisedIdentifiersTree
                   {-# LINE 391 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- copy rule (down)
              _x1Ocat =
                  ({-# LINE 190 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _lhsIcat
                   {-# LINE 396 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- copy rule (chain)
              _x1Oenv =
                  ({-# LINE 146 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   error "missing rule: ExpressionDirectionPair.Tuple.x1.env"
                   {-# LINE 401 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              ( _x1IcanonicalisedIdentifiersTree) =
                  (x1_ _x1Ocat _x1Oenv )
          in  ( _lhsOcanonicalisedIdentifiersTree)))
-- ExpressionDirectionPairList ---------------------------------
{-
   visit 0:
      inherited attribute:
         cat                  : Catalog
      synthesized attribute:
         canonicalisedIdentifiersTree : SELF 
   alternatives:
      alternative Cons:
         child hd             : ExpressionDirectionPair a
         child tl             : ExpressionDirectionPairList a
         visit 0:
            local canonicalisedIdentifiersTree : _
      alternative Nil:
         visit 0:
            local canonicalisedIdentifiersTree : _
-}
type ExpressionDirectionPairList a  = [ExpressionDirectionPair a ]
-- cata
sem_ExpressionDirectionPairList :: (ExpressionDirectionPairList) (a)  ->
                                   (T_ExpressionDirectionPairList) (a) 
sem_ExpressionDirectionPairList list  =
    (Prelude.foldr sem_ExpressionDirectionPairList_Cons sem_ExpressionDirectionPairList_Nil (Prelude.map sem_ExpressionDirectionPair list) )
-- semantic domain
type T_ExpressionDirectionPairList a  = (Catalog) ->
                                        ( ExpressionDirectionPairList a )
data Inh_ExpressionDirectionPairList a  = Inh_ExpressionDirectionPairList {cat_Inh_ExpressionDirectionPairList :: (Catalog)}
data Syn_ExpressionDirectionPairList a  = Syn_ExpressionDirectionPairList {canonicalisedIdentifiersTree_Syn_ExpressionDirectionPairList :: ExpressionDirectionPairList a }
wrap_ExpressionDirectionPairList :: (T_ExpressionDirectionPairList) (a)  ->
                                    (Inh_ExpressionDirectionPairList) (a)  ->
                                    (Syn_ExpressionDirectionPairList) (a) 
wrap_ExpressionDirectionPairList sem (Inh_ExpressionDirectionPairList _lhsIcat )  =
    (let ( _lhsOcanonicalisedIdentifiersTree) =
             (sem _lhsIcat )
     in  (Syn_ExpressionDirectionPairList _lhsOcanonicalisedIdentifiersTree ))
sem_ExpressionDirectionPairList_Cons :: (T_ExpressionDirectionPair) (a)  ->
                                        (T_ExpressionDirectionPairList) (a)  ->
                                        (T_ExpressionDirectionPairList) (a) 
sem_ExpressionDirectionPairList_Cons (hd_ :: (Catalog) ->
                                             ( ExpressionDirectionPair a )) (tl_ :: (Catalog) ->
                                                                                    ( ExpressionDirectionPairList a ))  =
    (\ (_lhsIcat :: (Catalog)) ->
         (let _lhsOcanonicalisedIdentifiersTree :: ExpressionDirectionPairList a 
              _hdOcat :: (Catalog)
              _tlOcat :: (Catalog)
              _hdIcanonicalisedIdentifiersTree :: ExpressionDirectionPair a 
              _tlIcanonicalisedIdentifiersTree :: ExpressionDirectionPairList a 
              -- self rule
              _canonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   (:) _hdIcanonicalisedIdentifiersTree _tlIcanonicalisedIdentifiersTree
                   {-# LINE 456 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- self rule
              _lhsOcanonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _canonicalisedIdentifiersTree
                   {-# LINE 461 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- copy rule (down)
              _hdOcat =
                  ({-# LINE 190 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _lhsIcat
                   {-# LINE 466 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- copy rule (down)
              _tlOcat =
                  ({-# LINE 190 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _lhsIcat
                   {-# LINE 471 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              ( _hdIcanonicalisedIdentifiersTree) =
                  (hd_ _hdOcat )
              ( _tlIcanonicalisedIdentifiersTree) =
                  (tl_ _tlOcat )
          in  ( _lhsOcanonicalisedIdentifiersTree)))
sem_ExpressionDirectionPairList_Nil :: (T_ExpressionDirectionPairList) (a) 
sem_ExpressionDirectionPairList_Nil  =
    (\ (_lhsIcat :: (Catalog)) ->
         (let _lhsOcanonicalisedIdentifiersTree :: ExpressionDirectionPairList a 
              -- self rule
              _canonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   []
                   {-# LINE 485 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- self rule
              _lhsOcanonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _canonicalisedIdentifiersTree
                   {-# LINE 490 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
          in  ( _lhsOcanonicalisedIdentifiersTree)))
-- ExpressionList ----------------------------------------------
{-
   visit 0:
      inherited attribute:
         cat                  : Catalog
      synthesized attribute:
         canonicalisedIdentifiersTree : SELF 
   alternatives:
      alternative Cons:
         child hd             : ScalarExpression a
         child tl             : ExpressionList a
         visit 0:
            local canonicalisedIdentifiersTree : _
      alternative Nil:
         visit 0:
            local canonicalisedIdentifiersTree : _
-}
type ExpressionList a  = [ScalarExpression a ]
-- cata
sem_ExpressionList :: (ExpressionList) (a)  ->
                      (T_ExpressionList) (a) 
sem_ExpressionList list  =
    (Prelude.foldr sem_ExpressionList_Cons sem_ExpressionList_Nil (Prelude.map sem_ScalarExpression list) )
-- semantic domain
type T_ExpressionList a  = (Catalog) ->
                           ( ExpressionList a )
data Inh_ExpressionList a  = Inh_ExpressionList {cat_Inh_ExpressionList :: (Catalog)}
data Syn_ExpressionList a  = Syn_ExpressionList {canonicalisedIdentifiersTree_Syn_ExpressionList :: ExpressionList a }
wrap_ExpressionList :: (T_ExpressionList) (a)  ->
                       (Inh_ExpressionList) (a)  ->
                       (Syn_ExpressionList) (a) 
wrap_ExpressionList sem (Inh_ExpressionList _lhsIcat )  =
    (let ( _lhsOcanonicalisedIdentifiersTree) =
             (sem _lhsIcat )
     in  (Syn_ExpressionList _lhsOcanonicalisedIdentifiersTree ))
sem_ExpressionList_Cons :: (T_ScalarExpression) (a)  ->
                           (T_ExpressionList) (a)  ->
                           (T_ExpressionList) (a) 
sem_ExpressionList_Cons (hd_ :: (Catalog) ->
                                (Environment) ->
                                ( ScalarExpression a )) (tl_ :: (Catalog) ->
                                                                ( ExpressionList a ))  =
    (\ (_lhsIcat :: (Catalog)) ->
         (let _lhsOcanonicalisedIdentifiersTree :: ExpressionList a 
              _hdOcat :: (Catalog)
              _hdOenv :: (Environment)
              _tlOcat :: (Catalog)
              _hdIcanonicalisedIdentifiersTree :: ScalarExpression a 
              _tlIcanonicalisedIdentifiersTree :: ExpressionList a 
              -- self rule
              _canonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   (:) _hdIcanonicalisedIdentifiersTree _tlIcanonicalisedIdentifiersTree
                   {-# LINE 545 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- self rule
              _lhsOcanonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _canonicalisedIdentifiersTree
                   {-# LINE 550 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- copy rule (down)
              _hdOcat =
                  ({-# LINE 190 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _lhsIcat
                   {-# LINE 555 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- copy rule (chain)
              _hdOenv =
                  ({-# LINE 146 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   error "missing rule: ExpressionList.Cons.hd.env"
                   {-# LINE 560 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- copy rule (down)
              _tlOcat =
                  ({-# LINE 190 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _lhsIcat
                   {-# LINE 565 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              ( _hdIcanonicalisedIdentifiersTree) =
                  (hd_ _hdOcat _hdOenv )
              ( _tlIcanonicalisedIdentifiersTree) =
                  (tl_ _tlOcat )
          in  ( _lhsOcanonicalisedIdentifiersTree)))
sem_ExpressionList_Nil :: (T_ExpressionList) (a) 
sem_ExpressionList_Nil  =
    (\ (_lhsIcat :: (Catalog)) ->
         (let _lhsOcanonicalisedIdentifiersTree :: ExpressionList a 
              -- self rule
              _canonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   []
                   {-# LINE 579 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- self rule
              _lhsOcanonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _canonicalisedIdentifiersTree
                   {-# LINE 584 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
          in  ( _lhsOcanonicalisedIdentifiersTree)))
-- ExpressionListList ------------------------------------------
{-
   visit 0:
      inherited attribute:
         cat                  : Catalog
      synthesized attribute:
         canonicalisedIdentifiersTree : SELF 
   alternatives:
      alternative Cons:
         child hd             : ExpressionList a
         child tl             : ExpressionListList a
         visit 0:
            local canonicalisedIdentifiersTree : _
      alternative Nil:
         visit 0:
            local canonicalisedIdentifiersTree : _
-}
type ExpressionListList a  = [ExpressionList a ]
-- cata
sem_ExpressionListList :: (ExpressionListList) (a)  ->
                          (T_ExpressionListList) (a) 
sem_ExpressionListList list  =
    (Prelude.foldr sem_ExpressionListList_Cons sem_ExpressionListList_Nil (Prelude.map sem_ExpressionList list) )
-- semantic domain
type T_ExpressionListList a  = (Catalog) ->
                               ( ExpressionListList a )
data Inh_ExpressionListList a  = Inh_ExpressionListList {cat_Inh_ExpressionListList :: (Catalog)}
data Syn_ExpressionListList a  = Syn_ExpressionListList {canonicalisedIdentifiersTree_Syn_ExpressionListList :: ExpressionListList a }
wrap_ExpressionListList :: (T_ExpressionListList) (a)  ->
                           (Inh_ExpressionListList) (a)  ->
                           (Syn_ExpressionListList) (a) 
wrap_ExpressionListList sem (Inh_ExpressionListList _lhsIcat )  =
    (let ( _lhsOcanonicalisedIdentifiersTree) =
             (sem _lhsIcat )
     in  (Syn_ExpressionListList _lhsOcanonicalisedIdentifiersTree ))
sem_ExpressionListList_Cons :: (T_ExpressionList) (a)  ->
                               (T_ExpressionListList) (a)  ->
                               (T_ExpressionListList) (a) 
sem_ExpressionListList_Cons (hd_ :: (Catalog) ->
                                    ( ExpressionList a )) (tl_ :: (Catalog) ->
                                                                  ( ExpressionListList a ))  =
    (\ (_lhsIcat :: (Catalog)) ->
         (let _lhsOcanonicalisedIdentifiersTree :: ExpressionListList a 
              _hdOcat :: (Catalog)
              _tlOcat :: (Catalog)
              _hdIcanonicalisedIdentifiersTree :: ExpressionList a 
              _tlIcanonicalisedIdentifiersTree :: ExpressionListList a 
              -- self rule
              _canonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   (:) _hdIcanonicalisedIdentifiersTree _tlIcanonicalisedIdentifiersTree
                   {-# LINE 637 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- self rule
              _lhsOcanonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _canonicalisedIdentifiersTree
                   {-# LINE 642 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- copy rule (down)
              _hdOcat =
                  ({-# LINE 190 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _lhsIcat
                   {-# LINE 647 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- copy rule (down)
              _tlOcat =
                  ({-# LINE 190 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _lhsIcat
                   {-# LINE 652 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              ( _hdIcanonicalisedIdentifiersTree) =
                  (hd_ _hdOcat )
              ( _tlIcanonicalisedIdentifiersTree) =
                  (tl_ _tlOcat )
          in  ( _lhsOcanonicalisedIdentifiersTree)))
sem_ExpressionListList_Nil :: (T_ExpressionListList) (a) 
sem_ExpressionListList_Nil  =
    (\ (_lhsIcat :: (Catalog)) ->
         (let _lhsOcanonicalisedIdentifiersTree :: ExpressionListList a 
              -- self rule
              _canonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   []
                   {-# LINE 666 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- self rule
              _lhsOcanonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _canonicalisedIdentifiersTree
                   {-# LINE 671 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
          in  ( _lhsOcanonicalisedIdentifiersTree)))
-- InList ------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         cat                  : Catalog
      synthesized attribute:
         canonicalisedIdentifiersTree : SELF 
   alternatives:
      alternative InList:
         child ann            : {a}
         child exprs          : ExpressionList a
         visit 0:
            local canonicalisedIdentifiersTree : _
      alternative InSelect:
         child ann            : {a}
         child sel            : QueryExpression a
         visit 0:
            local canonicalisedIdentifiersTree : _
-}
data InList a  = InList (a) (ExpressionList a ) 
               | InSelect (a) (QueryExpression a ) 
               deriving ( Data,Eq,Show,Typeable)
-- cata
sem_InList :: (InList) (a)  ->
              (T_InList) (a) 
sem_InList (InList _ann _exprs )  =
    (sem_InList_InList _ann (sem_ExpressionList _exprs ) )
sem_InList (InSelect _ann _sel )  =
    (sem_InList_InSelect _ann (sem_QueryExpression _sel ) )
-- semantic domain
type T_InList a  = (Catalog) ->
                   ( InList a )
data Inh_InList a  = Inh_InList {cat_Inh_InList :: (Catalog)}
data Syn_InList a  = Syn_InList {canonicalisedIdentifiersTree_Syn_InList :: InList a }
wrap_InList :: (T_InList) (a)  ->
               (Inh_InList) (a)  ->
               (Syn_InList) (a) 
wrap_InList sem (Inh_InList _lhsIcat )  =
    (let ( _lhsOcanonicalisedIdentifiersTree) =
             (sem _lhsIcat )
     in  (Syn_InList _lhsOcanonicalisedIdentifiersTree ))
sem_InList_InList :: (a) ->
                     (T_ExpressionList) (a)  ->
                     (T_InList) (a) 
sem_InList_InList (ann_ :: (a)) (exprs_ :: (Catalog) ->
                                           ( ExpressionList a ))  =
    (\ (_lhsIcat :: (Catalog)) ->
         (let _lhsOcanonicalisedIdentifiersTree :: InList a 
              _exprsOcat :: (Catalog)
              _exprsIcanonicalisedIdentifiersTree :: ExpressionList a 
              -- self rule
              _canonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   InList ann_ _exprsIcanonicalisedIdentifiersTree
                   {-# LINE 727 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- self rule
              _lhsOcanonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _canonicalisedIdentifiersTree
                   {-# LINE 732 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- copy rule (down)
              _exprsOcat =
                  ({-# LINE 190 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _lhsIcat
                   {-# LINE 737 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              ( _exprsIcanonicalisedIdentifiersTree) =
                  (exprs_ _exprsOcat )
          in  ( _lhsOcanonicalisedIdentifiersTree)))
sem_InList_InSelect :: (a) ->
                       (T_QueryExpression) (a)  ->
                       (T_InList) (a) 
sem_InList_InSelect (ann_ :: (a)) (sel_ :: (Catalog) ->
                                           ( QueryExpression a ,(Environment)))  =
    (\ (_lhsIcat :: (Catalog)) ->
         (let _lhsOcanonicalisedIdentifiersTree :: InList a 
              _selOcat :: (Catalog)
              _selIcanonicalisedIdentifiersTree :: QueryExpression a 
              _selIcenv :: (Environment)
              -- self rule
              _canonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   InSelect ann_ _selIcanonicalisedIdentifiersTree
                   {-# LINE 755 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- self rule
              _lhsOcanonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _canonicalisedIdentifiersTree
                   {-# LINE 760 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- copy rule (down)
              _selOcat =
                  ({-# LINE 190 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _lhsIcat
                   {-# LINE 765 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              ( _selIcanonicalisedIdentifiersTree,_selIcenv) =
                  (sel_ _selOcat )
          in  ( _lhsOcanonicalisedIdentifiersTree)))
-- JoinExpression ----------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         env                  : Environment
      synthesized attribute:
         canonicalisedIdentifiersTree : SELF 
   alternatives:
      alternative JoinOn:
         child ann            : {a}
         child expr           : ScalarExpression a
         visit 0:
            local canonicalisedIdentifiersTree : _
      alternative JoinUsing:
         child ann            : {a}
         child x              : {[String]}
         visit 0:
            local canonicalisedIdentifiersTree : _
-}
data JoinExpression a  = JoinOn (a) (ScalarExpression a ) 
                       | JoinUsing (a) (([String])) 
                       deriving ( Data,Eq,Show,Typeable)
-- cata
sem_JoinExpression :: (JoinExpression) (a)  ->
                      (T_JoinExpression) (a) 
sem_JoinExpression (JoinOn _ann _expr )  =
    (sem_JoinExpression_JoinOn _ann (sem_ScalarExpression _expr ) )
sem_JoinExpression (JoinUsing _ann _x )  =
    (sem_JoinExpression_JoinUsing _ann _x )
-- semantic domain
type T_JoinExpression a  = (Catalog) ->
                           (Environment) ->
                           ( JoinExpression a )
data Inh_JoinExpression a  = Inh_JoinExpression {cat_Inh_JoinExpression :: (Catalog),env_Inh_JoinExpression :: (Environment)}
data Syn_JoinExpression a  = Syn_JoinExpression {canonicalisedIdentifiersTree_Syn_JoinExpression :: JoinExpression a }
wrap_JoinExpression :: (T_JoinExpression) (a)  ->
                       (Inh_JoinExpression) (a)  ->
                       (Syn_JoinExpression) (a) 
wrap_JoinExpression sem (Inh_JoinExpression _lhsIcat _lhsIenv )  =
    (let ( _lhsOcanonicalisedIdentifiersTree) =
             (sem _lhsIcat _lhsIenv )
     in  (Syn_JoinExpression _lhsOcanonicalisedIdentifiersTree ))
sem_JoinExpression_JoinOn :: (a) ->
                             (T_ScalarExpression) (a)  ->
                             (T_JoinExpression) (a) 
sem_JoinExpression_JoinOn (ann_ :: (a)) (expr_ :: (Catalog) ->
                                                  (Environment) ->
                                                  ( ScalarExpression a ))  =
    (\ (_lhsIcat :: (Catalog))
       (_lhsIenv :: (Environment)) ->
         (let _lhsOcanonicalisedIdentifiersTree :: JoinExpression a 
              _exprOcat :: (Catalog)
              _exprOenv :: (Environment)
              _exprIcanonicalisedIdentifiersTree :: ScalarExpression a 
              -- self rule
              _canonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   JoinOn ann_ _exprIcanonicalisedIdentifiersTree
                   {-# LINE 828 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- self rule
              _lhsOcanonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _canonicalisedIdentifiersTree
                   {-# LINE 833 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- copy rule (down)
              _exprOcat =
                  ({-# LINE 190 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _lhsIcat
                   {-# LINE 838 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- copy rule (down)
              _exprOenv =
                  ({-# LINE 146 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _lhsIenv
                   {-# LINE 843 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              ( _exprIcanonicalisedIdentifiersTree) =
                  (expr_ _exprOcat _exprOenv )
          in  ( _lhsOcanonicalisedIdentifiersTree)))
sem_JoinExpression_JoinUsing :: (a) ->
                                ([String]) ->
                                (T_JoinExpression) (a) 
sem_JoinExpression_JoinUsing (ann_ :: (a)) (x_ :: ([String]))  =
    (\ (_lhsIcat :: (Catalog))
       (_lhsIenv :: (Environment)) ->
         (let _lhsOcanonicalisedIdentifiersTree :: JoinExpression a 
              -- self rule
              _canonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   JoinUsing ann_ x_
                   {-# LINE 858 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- self rule
              _lhsOcanonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _canonicalisedIdentifiersTree
                   {-# LINE 863 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
          in  ( _lhsOcanonicalisedIdentifiersTree)))
-- MaybeBoolExpression -----------------------------------------
{-
   visit 0:
      inherited attribute:
         cat                  : Catalog
      synthesized attribute:
         canonicalisedIdentifiersTree : SELF 
   alternatives:
      alternative Just:
         child just           : {ScalarExpression a}
         visit 0:
            local canonicalisedIdentifiersTree : _
      alternative Nothing:
         visit 0:
            local canonicalisedIdentifiersTree : _
-}
type MaybeBoolExpression a  = Maybe (ScalarExpression a)
-- cata
sem_MaybeBoolExpression :: (MaybeBoolExpression) (a)  ->
                           (T_MaybeBoolExpression) (a) 
sem_MaybeBoolExpression (Prelude.Just x )  =
    (sem_MaybeBoolExpression_Just x )
sem_MaybeBoolExpression Prelude.Nothing  =
    sem_MaybeBoolExpression_Nothing
-- semantic domain
type T_MaybeBoolExpression a  = (Catalog) ->
                                ( MaybeBoolExpression a )
data Inh_MaybeBoolExpression a  = Inh_MaybeBoolExpression {cat_Inh_MaybeBoolExpression :: (Catalog)}
data Syn_MaybeBoolExpression a  = Syn_MaybeBoolExpression {canonicalisedIdentifiersTree_Syn_MaybeBoolExpression :: MaybeBoolExpression a }
wrap_MaybeBoolExpression :: (T_MaybeBoolExpression) (a)  ->
                            (Inh_MaybeBoolExpression) (a)  ->
                            (Syn_MaybeBoolExpression) (a) 
wrap_MaybeBoolExpression sem (Inh_MaybeBoolExpression _lhsIcat )  =
    (let ( _lhsOcanonicalisedIdentifiersTree) =
             (sem _lhsIcat )
     in  (Syn_MaybeBoolExpression _lhsOcanonicalisedIdentifiersTree ))
sem_MaybeBoolExpression_Just :: (ScalarExpression a) ->
                                (T_MaybeBoolExpression) (a) 
sem_MaybeBoolExpression_Just (just_ :: (ScalarExpression a))  =
    (\ (_lhsIcat :: (Catalog)) ->
         (let _lhsOcanonicalisedIdentifiersTree :: MaybeBoolExpression a 
              -- self rule
              _canonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   Just just_
                   {-# LINE 910 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- self rule
              _lhsOcanonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _canonicalisedIdentifiersTree
                   {-# LINE 915 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
          in  ( _lhsOcanonicalisedIdentifiersTree)))
sem_MaybeBoolExpression_Nothing :: (T_MaybeBoolExpression) (a) 
sem_MaybeBoolExpression_Nothing  =
    (\ (_lhsIcat :: (Catalog)) ->
         (let _lhsOcanonicalisedIdentifiersTree :: MaybeBoolExpression a 
              -- self rule
              _canonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   Nothing
                   {-# LINE 925 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- self rule
              _lhsOcanonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _canonicalisedIdentifiersTree
                   {-# LINE 930 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
          in  ( _lhsOcanonicalisedIdentifiersTree)))
-- MaybeExpression ---------------------------------------------
{-
   visit 0:
      inherited attribute:
         cat                  : Catalog
      synthesized attribute:
         canonicalisedIdentifiersTree : SELF 
   alternatives:
      alternative Just:
         child just           : {ScalarExpression a}
         visit 0:
            local canonicalisedIdentifiersTree : _
      alternative Nothing:
         visit 0:
            local canonicalisedIdentifiersTree : _
-}
type MaybeExpression a  = Maybe (ScalarExpression a)
-- cata
sem_MaybeExpression :: (MaybeExpression) (a)  ->
                       (T_MaybeExpression) (a) 
sem_MaybeExpression (Prelude.Just x )  =
    (sem_MaybeExpression_Just x )
sem_MaybeExpression Prelude.Nothing  =
    sem_MaybeExpression_Nothing
-- semantic domain
type T_MaybeExpression a  = (Catalog) ->
                            ( MaybeExpression a )
data Inh_MaybeExpression a  = Inh_MaybeExpression {cat_Inh_MaybeExpression :: (Catalog)}
data Syn_MaybeExpression a  = Syn_MaybeExpression {canonicalisedIdentifiersTree_Syn_MaybeExpression :: MaybeExpression a }
wrap_MaybeExpression :: (T_MaybeExpression) (a)  ->
                        (Inh_MaybeExpression) (a)  ->
                        (Syn_MaybeExpression) (a) 
wrap_MaybeExpression sem (Inh_MaybeExpression _lhsIcat )  =
    (let ( _lhsOcanonicalisedIdentifiersTree) =
             (sem _lhsIcat )
     in  (Syn_MaybeExpression _lhsOcanonicalisedIdentifiersTree ))
sem_MaybeExpression_Just :: (ScalarExpression a) ->
                            (T_MaybeExpression) (a) 
sem_MaybeExpression_Just (just_ :: (ScalarExpression a))  =
    (\ (_lhsIcat :: (Catalog)) ->
         (let _lhsOcanonicalisedIdentifiersTree :: MaybeExpression a 
              -- self rule
              _canonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   Just just_
                   {-# LINE 977 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- self rule
              _lhsOcanonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _canonicalisedIdentifiersTree
                   {-# LINE 982 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
          in  ( _lhsOcanonicalisedIdentifiersTree)))
sem_MaybeExpression_Nothing :: (T_MaybeExpression) (a) 
sem_MaybeExpression_Nothing  =
    (\ (_lhsIcat :: (Catalog)) ->
         (let _lhsOcanonicalisedIdentifiersTree :: MaybeExpression a 
              -- self rule
              _canonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   Nothing
                   {-# LINE 992 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- self rule
              _lhsOcanonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _canonicalisedIdentifiersTree
                   {-# LINE 997 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
          in  ( _lhsOcanonicalisedIdentifiersTree)))
-- OnExpr ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         env                  : Environment
      synthesized attribute:
         canonicalisedIdentifiersTree : SELF 
   alternatives:
      alternative Just:
         child just           : {JoinExpression a}
         visit 0:
            local canonicalisedIdentifiersTree : _
      alternative Nothing:
         visit 0:
            local canonicalisedIdentifiersTree : _
-}
type OnExpr a  = Maybe (JoinExpression a)
-- cata
sem_OnExpr :: (OnExpr) (a)  ->
              (T_OnExpr) (a) 
sem_OnExpr (Prelude.Just x )  =
    (sem_OnExpr_Just x )
sem_OnExpr Prelude.Nothing  =
    sem_OnExpr_Nothing
-- semantic domain
type T_OnExpr a  = (Catalog) ->
                   (Environment) ->
                   ( OnExpr a )
data Inh_OnExpr a  = Inh_OnExpr {cat_Inh_OnExpr :: (Catalog),env_Inh_OnExpr :: (Environment)}
data Syn_OnExpr a  = Syn_OnExpr {canonicalisedIdentifiersTree_Syn_OnExpr :: OnExpr a }
wrap_OnExpr :: (T_OnExpr) (a)  ->
               (Inh_OnExpr) (a)  ->
               (Syn_OnExpr) (a) 
wrap_OnExpr sem (Inh_OnExpr _lhsIcat _lhsIenv )  =
    (let ( _lhsOcanonicalisedIdentifiersTree) =
             (sem _lhsIcat _lhsIenv )
     in  (Syn_OnExpr _lhsOcanonicalisedIdentifiersTree ))
sem_OnExpr_Just :: (JoinExpression a) ->
                   (T_OnExpr) (a) 
sem_OnExpr_Just (just_ :: (JoinExpression a))  =
    (\ (_lhsIcat :: (Catalog))
       (_lhsIenv :: (Environment)) ->
         (let _lhsOcanonicalisedIdentifiersTree :: OnExpr a 
              -- self rule
              _canonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   Just just_
                   {-# LINE 1047 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- self rule
              _lhsOcanonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _canonicalisedIdentifiersTree
                   {-# LINE 1052 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
          in  ( _lhsOcanonicalisedIdentifiersTree)))
sem_OnExpr_Nothing :: (T_OnExpr) (a) 
sem_OnExpr_Nothing  =
    (\ (_lhsIcat :: (Catalog))
       (_lhsIenv :: (Environment)) ->
         (let _lhsOcanonicalisedIdentifiersTree :: OnExpr a 
              -- self rule
              _canonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   Nothing
                   {-# LINE 1063 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- self rule
              _lhsOcanonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _canonicalisedIdentifiersTree
                   {-# LINE 1068 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
          in  ( _lhsOcanonicalisedIdentifiersTree)))
-- QueryExpression ---------------------------------------------
{-
   visit 0:
      inherited attribute:
         cat                  : Catalog
      synthesized attributes:
         canonicalisedIdentifiersTree : SELF 
         cenv                 : Environment
   alternatives:
      alternative CombineSelect:
         child ann            : {a}
         child ctype          : {CombineType}
         child sel1           : QueryExpression a
         child sel2           : QueryExpression a
         visit 0:
            local canonicalisedIdentifiersTree : _
      alternative Select:
         child ann            : {a}
         child selDistinct    : {Distinct}
         child selSelectList  : SelectList a
         child selTref        : TableRefList a
         child selWhere       : MaybeBoolExpression a
         child selGroupBy     : ExpressionList a
         child selHaving      : MaybeBoolExpression a
         child selOrderBy     : ExpressionDirectionPairList a
         child selLimit       : MaybeExpression a
         child selOffset      : MaybeExpression a
         visit 0:
            local canonicalisedIdentifiersTree : _
      alternative Values:
         child ann            : {a}
         child vll            : ExpressionListList a
         visit 0:
            local canonicalisedIdentifiersTree : _
      alternative WithSelect:
         child ann            : {a}
         child withs          : WithQueryList a
         child ex             : QueryExpression a
         visit 0:
            local canonicalisedIdentifiersTree : _
-}
data QueryExpression a  = CombineSelect (a) (CombineType) (QueryExpression a ) (QueryExpression a ) 
                        | Select (a) (Distinct) (SelectList a ) (TableRefList a ) (MaybeBoolExpression a ) (ExpressionList a ) (MaybeBoolExpression a ) (ExpressionDirectionPairList a ) (MaybeExpression a ) (MaybeExpression a ) 
                        | Values (a) (ExpressionListList a ) 
                        | WithSelect (a) (WithQueryList a ) (QueryExpression a ) 
                        deriving ( Data,Eq,Show,Typeable)
-- cata
sem_QueryExpression :: (QueryExpression) (a)  ->
                       (T_QueryExpression) (a) 
sem_QueryExpression (CombineSelect _ann _ctype _sel1 _sel2 )  =
    (sem_QueryExpression_CombineSelect _ann _ctype (sem_QueryExpression _sel1 ) (sem_QueryExpression _sel2 ) )
sem_QueryExpression (Select _ann _selDistinct _selSelectList _selTref _selWhere _selGroupBy _selHaving _selOrderBy _selLimit _selOffset )  =
    (sem_QueryExpression_Select _ann _selDistinct (sem_SelectList _selSelectList ) (sem_TableRefList _selTref ) (sem_MaybeBoolExpression _selWhere ) (sem_ExpressionList _selGroupBy ) (sem_MaybeBoolExpression _selHaving ) (sem_ExpressionDirectionPairList _selOrderBy ) (sem_MaybeExpression _selLimit ) (sem_MaybeExpression _selOffset ) )
sem_QueryExpression (Values _ann _vll )  =
    (sem_QueryExpression_Values _ann (sem_ExpressionListList _vll ) )
sem_QueryExpression (WithSelect _ann _withs _ex )  =
    (sem_QueryExpression_WithSelect _ann (sem_WithQueryList _withs ) (sem_QueryExpression _ex ) )
-- semantic domain
type T_QueryExpression a  = (Catalog) ->
                            ( QueryExpression a ,(Environment))
data Inh_QueryExpression a  = Inh_QueryExpression {cat_Inh_QueryExpression :: (Catalog)}
data Syn_QueryExpression a  = Syn_QueryExpression {canonicalisedIdentifiersTree_Syn_QueryExpression :: QueryExpression a ,cenv_Syn_QueryExpression :: (Environment)}
wrap_QueryExpression :: (T_QueryExpression) (a)  ->
                        (Inh_QueryExpression) (a)  ->
                        (Syn_QueryExpression) (a) 
wrap_QueryExpression sem (Inh_QueryExpression _lhsIcat )  =
    (let ( _lhsOcanonicalisedIdentifiersTree,_lhsOcenv) =
             (sem _lhsIcat )
     in  (Syn_QueryExpression _lhsOcanonicalisedIdentifiersTree _lhsOcenv ))
sem_QueryExpression_CombineSelect :: (a) ->
                                     (CombineType) ->
                                     (T_QueryExpression) (a)  ->
                                     (T_QueryExpression) (a)  ->
                                     (T_QueryExpression) (a) 
sem_QueryExpression_CombineSelect (ann_ :: (a)) (ctype_ :: (CombineType)) (sel1_ :: (Catalog) ->
                                                                                    ( QueryExpression a ,(Environment))) (sel2_ :: (Catalog) ->
                                                                                                                                   ( QueryExpression a ,(Environment)))  =
    (\ (_lhsIcat :: (Catalog)) ->
         (let _lhsOcenv :: (Environment)
              _lhsOcanonicalisedIdentifiersTree :: QueryExpression a 
              _sel1Ocat :: (Catalog)
              _sel2Ocat :: (Catalog)
              _sel1IcanonicalisedIdentifiersTree :: QueryExpression a 
              _sel1Icenv :: (Environment)
              _sel2IcanonicalisedIdentifiersTree :: QueryExpression a 
              _sel2Icenv :: (Environment)
              -- "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag"(line 130, column 21)
              _lhsOcenv =
                  ({-# LINE 130 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _sel1Icenv
                   {-# LINE 1160 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- self rule
              _canonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   CombineSelect ann_ ctype_ _sel1IcanonicalisedIdentifiersTree _sel2IcanonicalisedIdentifiersTree
                   {-# LINE 1165 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- self rule
              _lhsOcanonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _canonicalisedIdentifiersTree
                   {-# LINE 1170 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- copy rule (down)
              _sel1Ocat =
                  ({-# LINE 190 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _lhsIcat
                   {-# LINE 1175 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- copy rule (down)
              _sel2Ocat =
                  ({-# LINE 190 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _lhsIcat
                   {-# LINE 1180 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              ( _sel1IcanonicalisedIdentifiersTree,_sel1Icenv) =
                  (sel1_ _sel1Ocat )
              ( _sel2IcanonicalisedIdentifiersTree,_sel2Icenv) =
                  (sel2_ _sel2Ocat )
          in  ( _lhsOcanonicalisedIdentifiersTree,_lhsOcenv)))
sem_QueryExpression_Select :: (a) ->
                              (Distinct) ->
                              (T_SelectList) (a)  ->
                              (T_TableRefList) (a)  ->
                              (T_MaybeBoolExpression) (a)  ->
                              (T_ExpressionList) (a)  ->
                              (T_MaybeBoolExpression) (a)  ->
                              (T_ExpressionDirectionPairList) (a)  ->
                              (T_MaybeExpression) (a)  ->
                              (T_MaybeExpression) (a)  ->
                              (T_QueryExpression) (a) 
sem_QueryExpression_Select (ann_ :: (a)) (selDistinct_ :: (Distinct)) (selSelectList_ :: (Catalog) ->
                                                                                         ( SelectList a ,(Environment))) (selTref_ :: (Catalog) ->
                                                                                                                                      ( TableRefList a ,(Environment))) (selWhere_ :: (Catalog) ->
                                                                                                                                                                                      ( MaybeBoolExpression a )) (selGroupBy_ :: (Catalog) ->
                                                                                                                                                                                                                                 ( ExpressionList a )) (selHaving_ :: (Catalog) ->
                                                                                                                                                                                                                                                                      ( MaybeBoolExpression a )) (selOrderBy_ :: (Catalog) ->
                                                                                                                                                                                                                                                                                                                 ( ExpressionDirectionPairList a )) (selLimit_ :: (Catalog) ->
                                                                                                                                                                                                                                                                                                                                                                  ( MaybeExpression a )) (selOffset_ :: (Catalog) ->
                                                                                                                                                                                                                                                                                                                                                                                                        ( MaybeExpression a ))  =
    (\ (_lhsIcat :: (Catalog)) ->
         (let _lhsOcenv :: (Environment)
              _lhsOcanonicalisedIdentifiersTree :: QueryExpression a 
              _selSelectListOcat :: (Catalog)
              _selTrefOcat :: (Catalog)
              _selWhereOcat :: (Catalog)
              _selGroupByOcat :: (Catalog)
              _selHavingOcat :: (Catalog)
              _selOrderByOcat :: (Catalog)
              _selLimitOcat :: (Catalog)
              _selOffsetOcat :: (Catalog)
              _selSelectListIcanonicalisedIdentifiersTree :: SelectList a 
              _selSelectListIcenv :: (Environment)
              _selTrefIcanonicalisedIdentifiersTree :: TableRefList a 
              _selTrefIcenv :: (Environment)
              _selWhereIcanonicalisedIdentifiersTree :: MaybeBoolExpression a 
              _selGroupByIcanonicalisedIdentifiersTree :: ExpressionList a 
              _selHavingIcanonicalisedIdentifiersTree :: MaybeBoolExpression a 
              _selOrderByIcanonicalisedIdentifiersTree :: ExpressionDirectionPairList a 
              _selLimitIcanonicalisedIdentifiersTree :: MaybeExpression a 
              _selOffsetIcanonicalisedIdentifiersTree :: MaybeExpression a 
              -- "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag"(line 129, column 14)
              _lhsOcenv =
                  ({-# LINE 129 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _selSelectListIcenv
                   {-# LINE 1231 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- self rule
              _canonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   Select ann_ selDistinct_ _selSelectListIcanonicalisedIdentifiersTree _selTrefIcanonicalisedIdentifiersTree _selWhereIcanonicalisedIdentifiersTree _selGroupByIcanonicalisedIdentifiersTree _selHavingIcanonicalisedIdentifiersTree _selOrderByIcanonicalisedIdentifiersTree _selLimitIcanonicalisedIdentifiersTree _selOffsetIcanonicalisedIdentifiersTree
                   {-# LINE 1236 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- self rule
              _lhsOcanonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _canonicalisedIdentifiersTree
                   {-# LINE 1241 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- copy rule (down)
              _selSelectListOcat =
                  ({-# LINE 190 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _lhsIcat
                   {-# LINE 1246 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- copy rule (down)
              _selTrefOcat =
                  ({-# LINE 190 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _lhsIcat
                   {-# LINE 1251 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- copy rule (down)
              _selWhereOcat =
                  ({-# LINE 190 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _lhsIcat
                   {-# LINE 1256 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- copy rule (down)
              _selGroupByOcat =
                  ({-# LINE 190 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _lhsIcat
                   {-# LINE 1261 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- copy rule (down)
              _selHavingOcat =
                  ({-# LINE 190 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _lhsIcat
                   {-# LINE 1266 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- copy rule (down)
              _selOrderByOcat =
                  ({-# LINE 190 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _lhsIcat
                   {-# LINE 1271 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- copy rule (down)
              _selLimitOcat =
                  ({-# LINE 190 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _lhsIcat
                   {-# LINE 1276 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- copy rule (down)
              _selOffsetOcat =
                  ({-# LINE 190 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _lhsIcat
                   {-# LINE 1281 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              ( _selSelectListIcanonicalisedIdentifiersTree,_selSelectListIcenv) =
                  (selSelectList_ _selSelectListOcat )
              ( _selTrefIcanonicalisedIdentifiersTree,_selTrefIcenv) =
                  (selTref_ _selTrefOcat )
              ( _selWhereIcanonicalisedIdentifiersTree) =
                  (selWhere_ _selWhereOcat )
              ( _selGroupByIcanonicalisedIdentifiersTree) =
                  (selGroupBy_ _selGroupByOcat )
              ( _selHavingIcanonicalisedIdentifiersTree) =
                  (selHaving_ _selHavingOcat )
              ( _selOrderByIcanonicalisedIdentifiersTree) =
                  (selOrderBy_ _selOrderByOcat )
              ( _selLimitIcanonicalisedIdentifiersTree) =
                  (selLimit_ _selLimitOcat )
              ( _selOffsetIcanonicalisedIdentifiersTree) =
                  (selOffset_ _selOffsetOcat )
          in  ( _lhsOcanonicalisedIdentifiersTree,_lhsOcenv)))
sem_QueryExpression_Values :: (a) ->
                              (T_ExpressionListList) (a)  ->
                              (T_QueryExpression) (a) 
sem_QueryExpression_Values (ann_ :: (a)) (vll_ :: (Catalog) ->
                                                  ( ExpressionListList a ))  =
    (\ (_lhsIcat :: (Catalog)) ->
         (let _lhsOcenv :: (Environment)
              _lhsOcanonicalisedIdentifiersTree :: QueryExpression a 
              _vllOcat :: (Catalog)
              _vllIcanonicalisedIdentifiersTree :: ExpressionListList a 
              -- "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag"(line 131, column 14)
              _lhsOcenv =
                  ({-# LINE 131 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   unimplementedEnvironment
                   {-# LINE 1313 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- self rule
              _canonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   Values ann_ _vllIcanonicalisedIdentifiersTree
                   {-# LINE 1318 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- self rule
              _lhsOcanonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _canonicalisedIdentifiersTree
                   {-# LINE 1323 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- copy rule (down)
              _vllOcat =
                  ({-# LINE 190 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _lhsIcat
                   {-# LINE 1328 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              ( _vllIcanonicalisedIdentifiersTree) =
                  (vll_ _vllOcat )
          in  ( _lhsOcanonicalisedIdentifiersTree,_lhsOcenv)))
sem_QueryExpression_WithSelect :: (a) ->
                                  (T_WithQueryList) (a)  ->
                                  (T_QueryExpression) (a)  ->
                                  (T_QueryExpression) (a) 
sem_QueryExpression_WithSelect (ann_ :: (a)) (withs_ :: (Catalog) ->
                                                        ( WithQueryList a )) (ex_ :: (Catalog) ->
                                                                                     ( QueryExpression a ,(Environment)))  =
    (\ (_lhsIcat :: (Catalog)) ->
         (let _lhsOcenv :: (Environment)
              _lhsOcanonicalisedIdentifiersTree :: QueryExpression a 
              _withsOcat :: (Catalog)
              _exOcat :: (Catalog)
              _withsIcanonicalisedIdentifiersTree :: WithQueryList a 
              _exIcanonicalisedIdentifiersTree :: QueryExpression a 
              _exIcenv :: (Environment)
              -- "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag"(line 132, column 18)
              _lhsOcenv =
                  ({-# LINE 132 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _exIcenv
                   {-# LINE 1351 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- self rule
              _canonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   WithSelect ann_ _withsIcanonicalisedIdentifiersTree _exIcanonicalisedIdentifiersTree
                   {-# LINE 1356 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- self rule
              _lhsOcanonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _canonicalisedIdentifiersTree
                   {-# LINE 1361 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- copy rule (down)
              _withsOcat =
                  ({-# LINE 190 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _lhsIcat
                   {-# LINE 1366 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- copy rule (down)
              _exOcat =
                  ({-# LINE 190 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _lhsIcat
                   {-# LINE 1371 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              ( _withsIcanonicalisedIdentifiersTree) =
                  (withs_ _withsOcat )
              ( _exIcanonicalisedIdentifiersTree,_exIcenv) =
                  (ex_ _exOcat )
          in  ( _lhsOcanonicalisedIdentifiersTree,_lhsOcenv)))
-- Root --------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         cat                  : Catalog
      synthesized attribute:
         canonicalisedIdentifiersTree : SELF 
   alternatives:
      alternative Root:
         child expr           : {[QueryExpression a]}
         visit 0:
            local canonicalisedIdentifiersTree : _
-}
data Root a  = Root (([QueryExpression a])) 
             deriving ( Show)
-- cata
sem_Root :: (Root) (a)  ->
            (T_Root) (a) 
sem_Root (Root _expr )  =
    (sem_Root_Root _expr )
-- semantic domain
type T_Root a  = (Catalog) ->
                 ( Root a )
data Inh_Root a  = Inh_Root {cat_Inh_Root :: (Catalog)}
data Syn_Root a  = Syn_Root {canonicalisedIdentifiersTree_Syn_Root :: Root a }
wrap_Root :: (T_Root) (a)  ->
             (Inh_Root) (a)  ->
             (Syn_Root) (a) 
wrap_Root sem (Inh_Root _lhsIcat )  =
    (let ( _lhsOcanonicalisedIdentifiersTree) =
             (sem _lhsIcat )
     in  (Syn_Root _lhsOcanonicalisedIdentifiersTree ))
sem_Root_Root :: ([QueryExpression a]) ->
                 (T_Root) (a) 
sem_Root_Root (expr_ :: ([QueryExpression a]))  =
    (\ (_lhsIcat :: (Catalog)) ->
         (let _lhsOcanonicalisedIdentifiersTree :: Root a 
              -- self rule
              _canonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   Root expr_
                   {-# LINE 1418 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- self rule
              _lhsOcanonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _canonicalisedIdentifiersTree
                   {-# LINE 1423 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
          in  ( _lhsOcanonicalisedIdentifiersTree)))
-- ScalarExpression --------------------------------------------
{-
   visit 0:
      inherited attributes:
         cat                  : Catalog
         env                  : Environment
      synthesized attribute:
         canonicalisedIdentifiersTree : SELF 
   alternatives:
      alternative BooleanLit:
         child ann            : {a}
         child b              : {Bool}
         visit 0:
            local canonicalisedIdentifiersTree : _
      alternative Case:
         child ann            : {a}
         child cases          : CaseExpressionListExpressionPairList a
         child els            : MaybeExpression a
         visit 0:
            local canonicalisedIdentifiersTree : _
      alternative CaseSimple:
         child ann            : {a}
         child value          : ScalarExpression a
         child cases          : CaseExpressionListExpressionPairList a
         child els            : MaybeExpression a
         visit 0:
            local canonicalisedIdentifiersTree : _
      alternative Cast:
         child ann            : {a}
         child expr           : ScalarExpression a
         child tn             : TypeName a
         visit 0:
            local canonicalisedIdentifiersTree : _
      alternative Exists:
         child ann            : {a}
         child sel            : QueryExpression a
         visit 0:
            local canonicalisedIdentifiersTree : _
      alternative FloatLit:
         child ann            : {a}
         child d              : {Double}
         visit 0:
            local canonicalisedIdentifiersTree : _
      alternative FunCall:
         child ann            : {a}
         child funName        : {String}
         child args           : ExpressionList a
         visit 0:
            local canonicalisedIdentifiersTree : _
      alternative Identifier:
         child ann            : {a}
         child i              : {String}
         visit 0:
            local canonicalisedIdentifiersTree : _
      alternative InPredicate:
         child ann            : {a}
         child expr           : ScalarExpression a
         child i              : {Bool}
         child list           : InList a
         visit 0:
            local canonicalisedIdentifiersTree : _
      alternative IntegerLit:
         child ann            : {a}
         child i              : {Integer}
         visit 0:
            local canonicalisedIdentifiersTree : _
      alternative LiftOperator:
         child ann            : {a}
         child oper           : {String}
         child flav           : {LiftFlavour}
         child args           : ExpressionList a
         visit 0:
            local canonicalisedIdentifiersTree : _
      alternative NullLit:
         child ann            : {a}
         visit 0:
            local canonicalisedIdentifiersTree : _
      alternative Placeholder:
         child ann            : {a}
         visit 0:
            local canonicalisedIdentifiersTree : _
      alternative PositionalArg:
         child ann            : {a}
         child p              : {Integer}
         visit 0:
            local canonicalisedIdentifiersTree : _
      alternative QIdentifier:
         child ann            : {a}
         child qual           : ScalarExpression a
         child i              : {String}
         visit 0:
            local canonicalisedIdentifiersTree : _
      alternative ScalarSubQuery:
         child ann            : {a}
         child sel            : QueryExpression a
         visit 0:
            local canonicalisedIdentifiersTree : _
      alternative StringLit:
         child ann            : {a}
         child value          : {String}
         visit 0:
            local canonicalisedIdentifiersTree : _
      alternative WindowFn:
         child ann            : {a}
         child fn             : ScalarExpression a
         child partitionBy    : ExpressionList a
         child orderBy        : ExpressionList a
         child dir            : {Direction}
         child frm            : {FrameClause}
         visit 0:
            local canonicalisedIdentifiersTree : _
-}
data ScalarExpression a  = BooleanLit (a) (Bool) 
                         | Case (a) (CaseExpressionListExpressionPairList a ) (MaybeExpression a ) 
                         | CaseSimple (a) (ScalarExpression a ) (CaseExpressionListExpressionPairList a ) (MaybeExpression a ) 
                         | Cast (a) (ScalarExpression a ) (TypeName a ) 
                         | Exists (a) (QueryExpression a ) 
                         | FloatLit (a) (Double) 
                         | FunCall (a) (String) (ExpressionList a ) 
                         | Identifier (a) (String) 
                         | InPredicate (a) (ScalarExpression a ) (Bool) (InList a ) 
                         | IntegerLit (a) (Integer) 
                         | LiftOperator (a) (String) (LiftFlavour) (ExpressionList a ) 
                         | NullLit (a) 
                         | Placeholder (a) 
                         | PositionalArg (a) (Integer) 
                         | QIdentifier (a) (ScalarExpression a ) (String) 
                         | ScalarSubQuery (a) (QueryExpression a ) 
                         | StringLit (a) (String) 
                         | WindowFn (a) (ScalarExpression a ) (ExpressionList a ) (ExpressionList a ) (Direction) (FrameClause) 
                         deriving ( Data,Eq,Show,Typeable)
-- cata
sem_ScalarExpression :: (ScalarExpression) (a)  ->
                        (T_ScalarExpression) (a) 
sem_ScalarExpression (BooleanLit _ann _b )  =
    (sem_ScalarExpression_BooleanLit _ann _b )
sem_ScalarExpression (Case _ann _cases _els )  =
    (sem_ScalarExpression_Case _ann (sem_CaseExpressionListExpressionPairList _cases ) (sem_MaybeExpression _els ) )
sem_ScalarExpression (CaseSimple _ann _value _cases _els )  =
    (sem_ScalarExpression_CaseSimple _ann (sem_ScalarExpression _value ) (sem_CaseExpressionListExpressionPairList _cases ) (sem_MaybeExpression _els ) )
sem_ScalarExpression (Cast _ann _expr _tn )  =
    (sem_ScalarExpression_Cast _ann (sem_ScalarExpression _expr ) (sem_TypeName _tn ) )
sem_ScalarExpression (Exists _ann _sel )  =
    (sem_ScalarExpression_Exists _ann (sem_QueryExpression _sel ) )
sem_ScalarExpression (FloatLit _ann _d )  =
    (sem_ScalarExpression_FloatLit _ann _d )
sem_ScalarExpression (FunCall _ann _funName _args )  =
    (sem_ScalarExpression_FunCall _ann _funName (sem_ExpressionList _args ) )
sem_ScalarExpression (Identifier _ann _i )  =
    (sem_ScalarExpression_Identifier _ann _i )
sem_ScalarExpression (InPredicate _ann _expr _i _list )  =
    (sem_ScalarExpression_InPredicate _ann (sem_ScalarExpression _expr ) _i (sem_InList _list ) )
sem_ScalarExpression (IntegerLit _ann _i )  =
    (sem_ScalarExpression_IntegerLit _ann _i )
sem_ScalarExpression (LiftOperator _ann _oper _flav _args )  =
    (sem_ScalarExpression_LiftOperator _ann _oper _flav (sem_ExpressionList _args ) )
sem_ScalarExpression (NullLit _ann )  =
    (sem_ScalarExpression_NullLit _ann )
sem_ScalarExpression (Placeholder _ann )  =
    (sem_ScalarExpression_Placeholder _ann )
sem_ScalarExpression (PositionalArg _ann _p )  =
    (sem_ScalarExpression_PositionalArg _ann _p )
sem_ScalarExpression (QIdentifier _ann _qual _i )  =
    (sem_ScalarExpression_QIdentifier _ann (sem_ScalarExpression _qual ) _i )
sem_ScalarExpression (ScalarSubQuery _ann _sel )  =
    (sem_ScalarExpression_ScalarSubQuery _ann (sem_QueryExpression _sel ) )
sem_ScalarExpression (StringLit _ann _value )  =
    (sem_ScalarExpression_StringLit _ann _value )
sem_ScalarExpression (WindowFn _ann _fn _partitionBy _orderBy _dir _frm )  =
    (sem_ScalarExpression_WindowFn _ann (sem_ScalarExpression _fn ) (sem_ExpressionList _partitionBy ) (sem_ExpressionList _orderBy ) _dir _frm )
-- semantic domain
type T_ScalarExpression a  = (Catalog) ->
                             (Environment) ->
                             ( ScalarExpression a )
data Inh_ScalarExpression a  = Inh_ScalarExpression {cat_Inh_ScalarExpression :: (Catalog),env_Inh_ScalarExpression :: (Environment)}
data Syn_ScalarExpression a  = Syn_ScalarExpression {canonicalisedIdentifiersTree_Syn_ScalarExpression :: ScalarExpression a }
wrap_ScalarExpression :: (T_ScalarExpression) (a)  ->
                         (Inh_ScalarExpression) (a)  ->
                         (Syn_ScalarExpression) (a) 
wrap_ScalarExpression sem (Inh_ScalarExpression _lhsIcat _lhsIenv )  =
    (let ( _lhsOcanonicalisedIdentifiersTree) =
             (sem _lhsIcat _lhsIenv )
     in  (Syn_ScalarExpression _lhsOcanonicalisedIdentifiersTree ))
sem_ScalarExpression_BooleanLit :: (a) ->
                                   (Bool) ->
                                   (T_ScalarExpression) (a) 
sem_ScalarExpression_BooleanLit (ann_ :: (a)) (b_ :: (Bool))  =
    (\ (_lhsIcat :: (Catalog))
       (_lhsIenv :: (Environment)) ->
         (let _lhsOcanonicalisedIdentifiersTree :: ScalarExpression a 
              -- self rule
              _canonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   BooleanLit ann_ b_
                   {-# LINE 1619 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- self rule
              _lhsOcanonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _canonicalisedIdentifiersTree
                   {-# LINE 1624 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
          in  ( _lhsOcanonicalisedIdentifiersTree)))
sem_ScalarExpression_Case :: (a) ->
                             (T_CaseExpressionListExpressionPairList) (a)  ->
                             (T_MaybeExpression) (a)  ->
                             (T_ScalarExpression) (a) 
sem_ScalarExpression_Case (ann_ :: (a)) (cases_ :: (Catalog) ->
                                                   ( CaseExpressionListExpressionPairList a )) (els_ :: (Catalog) ->
                                                                                                        ( MaybeExpression a ))  =
    (\ (_lhsIcat :: (Catalog))
       (_lhsIenv :: (Environment)) ->
         (let _lhsOcanonicalisedIdentifiersTree :: ScalarExpression a 
              _casesOcat :: (Catalog)
              _elsOcat :: (Catalog)
              _casesIcanonicalisedIdentifiersTree :: CaseExpressionListExpressionPairList a 
              _elsIcanonicalisedIdentifiersTree :: MaybeExpression a 
              -- self rule
              _canonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   Case ann_ _casesIcanonicalisedIdentifiersTree _elsIcanonicalisedIdentifiersTree
                   {-# LINE 1644 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- self rule
              _lhsOcanonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _canonicalisedIdentifiersTree
                   {-# LINE 1649 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- copy rule (down)
              _casesOcat =
                  ({-# LINE 190 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _lhsIcat
                   {-# LINE 1654 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- copy rule (down)
              _elsOcat =
                  ({-# LINE 190 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _lhsIcat
                   {-# LINE 1659 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              ( _casesIcanonicalisedIdentifiersTree) =
                  (cases_ _casesOcat )
              ( _elsIcanonicalisedIdentifiersTree) =
                  (els_ _elsOcat )
          in  ( _lhsOcanonicalisedIdentifiersTree)))
sem_ScalarExpression_CaseSimple :: (a) ->
                                   (T_ScalarExpression) (a)  ->
                                   (T_CaseExpressionListExpressionPairList) (a)  ->
                                   (T_MaybeExpression) (a)  ->
                                   (T_ScalarExpression) (a) 
sem_ScalarExpression_CaseSimple (ann_ :: (a)) (value_ :: (Catalog) ->
                                                         (Environment) ->
                                                         ( ScalarExpression a )) (cases_ :: (Catalog) ->
                                                                                            ( CaseExpressionListExpressionPairList a )) (els_ :: (Catalog) ->
                                                                                                                                                 ( MaybeExpression a ))  =
    (\ (_lhsIcat :: (Catalog))
       (_lhsIenv :: (Environment)) ->
         (let _lhsOcanonicalisedIdentifiersTree :: ScalarExpression a 
              _valueOcat :: (Catalog)
              _valueOenv :: (Environment)
              _casesOcat :: (Catalog)
              _elsOcat :: (Catalog)
              _valueIcanonicalisedIdentifiersTree :: ScalarExpression a 
              _casesIcanonicalisedIdentifiersTree :: CaseExpressionListExpressionPairList a 
              _elsIcanonicalisedIdentifiersTree :: MaybeExpression a 
              -- self rule
              _canonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   CaseSimple ann_ _valueIcanonicalisedIdentifiersTree _casesIcanonicalisedIdentifiersTree _elsIcanonicalisedIdentifiersTree
                   {-# LINE 1689 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- self rule
              _lhsOcanonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _canonicalisedIdentifiersTree
                   {-# LINE 1694 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- copy rule (down)
              _valueOcat =
                  ({-# LINE 190 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _lhsIcat
                   {-# LINE 1699 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- copy rule (down)
              _valueOenv =
                  ({-# LINE 146 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _lhsIenv
                   {-# LINE 1704 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- copy rule (down)
              _casesOcat =
                  ({-# LINE 190 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _lhsIcat
                   {-# LINE 1709 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- copy rule (down)
              _elsOcat =
                  ({-# LINE 190 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _lhsIcat
                   {-# LINE 1714 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              ( _valueIcanonicalisedIdentifiersTree) =
                  (value_ _valueOcat _valueOenv )
              ( _casesIcanonicalisedIdentifiersTree) =
                  (cases_ _casesOcat )
              ( _elsIcanonicalisedIdentifiersTree) =
                  (els_ _elsOcat )
          in  ( _lhsOcanonicalisedIdentifiersTree)))
sem_ScalarExpression_Cast :: (a) ->
                             (T_ScalarExpression) (a)  ->
                             (T_TypeName) (a)  ->
                             (T_ScalarExpression) (a) 
sem_ScalarExpression_Cast (ann_ :: (a)) (expr_ :: (Catalog) ->
                                                  (Environment) ->
                                                  ( ScalarExpression a )) (tn_ :: (Catalog) ->
                                                                                  ( TypeName a ))  =
    (\ (_lhsIcat :: (Catalog))
       (_lhsIenv :: (Environment)) ->
         (let _lhsOcanonicalisedIdentifiersTree :: ScalarExpression a 
              _exprOcat :: (Catalog)
              _exprOenv :: (Environment)
              _tnOcat :: (Catalog)
              _exprIcanonicalisedIdentifiersTree :: ScalarExpression a 
              _tnIcanonicalisedIdentifiersTree :: TypeName a 
              -- self rule
              _canonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   Cast ann_ _exprIcanonicalisedIdentifiersTree _tnIcanonicalisedIdentifiersTree
                   {-# LINE 1742 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- self rule
              _lhsOcanonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _canonicalisedIdentifiersTree
                   {-# LINE 1747 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- copy rule (down)
              _exprOcat =
                  ({-# LINE 190 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _lhsIcat
                   {-# LINE 1752 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- copy rule (down)
              _exprOenv =
                  ({-# LINE 146 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _lhsIenv
                   {-# LINE 1757 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- copy rule (down)
              _tnOcat =
                  ({-# LINE 190 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _lhsIcat
                   {-# LINE 1762 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              ( _exprIcanonicalisedIdentifiersTree) =
                  (expr_ _exprOcat _exprOenv )
              ( _tnIcanonicalisedIdentifiersTree) =
                  (tn_ _tnOcat )
          in  ( _lhsOcanonicalisedIdentifiersTree)))
sem_ScalarExpression_Exists :: (a) ->
                               (T_QueryExpression) (a)  ->
                               (T_ScalarExpression) (a) 
sem_ScalarExpression_Exists (ann_ :: (a)) (sel_ :: (Catalog) ->
                                                   ( QueryExpression a ,(Environment)))  =
    (\ (_lhsIcat :: (Catalog))
       (_lhsIenv :: (Environment)) ->
         (let _lhsOcanonicalisedIdentifiersTree :: ScalarExpression a 
              _selOcat :: (Catalog)
              _selIcanonicalisedIdentifiersTree :: QueryExpression a 
              _selIcenv :: (Environment)
              -- self rule
              _canonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   Exists ann_ _selIcanonicalisedIdentifiersTree
                   {-# LINE 1783 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- self rule
              _lhsOcanonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _canonicalisedIdentifiersTree
                   {-# LINE 1788 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- copy rule (down)
              _selOcat =
                  ({-# LINE 190 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _lhsIcat
                   {-# LINE 1793 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              ( _selIcanonicalisedIdentifiersTree,_selIcenv) =
                  (sel_ _selOcat )
          in  ( _lhsOcanonicalisedIdentifiersTree)))
sem_ScalarExpression_FloatLit :: (a) ->
                                 (Double) ->
                                 (T_ScalarExpression) (a) 
sem_ScalarExpression_FloatLit (ann_ :: (a)) (d_ :: (Double))  =
    (\ (_lhsIcat :: (Catalog))
       (_lhsIenv :: (Environment)) ->
         (let _lhsOcanonicalisedIdentifiersTree :: ScalarExpression a 
              -- self rule
              _canonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   FloatLit ann_ d_
                   {-# LINE 1808 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- self rule
              _lhsOcanonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _canonicalisedIdentifiersTree
                   {-# LINE 1813 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
          in  ( _lhsOcanonicalisedIdentifiersTree)))
sem_ScalarExpression_FunCall :: (a) ->
                                (String) ->
                                (T_ExpressionList) (a)  ->
                                (T_ScalarExpression) (a) 
sem_ScalarExpression_FunCall (ann_ :: (a)) (funName_ :: (String)) (args_ :: (Catalog) ->
                                                                            ( ExpressionList a ))  =
    (\ (_lhsIcat :: (Catalog))
       (_lhsIenv :: (Environment)) ->
         (let _lhsOcanonicalisedIdentifiersTree :: ScalarExpression a 
              _argsOcat :: (Catalog)
              _argsIcanonicalisedIdentifiersTree :: ExpressionList a 
              -- self rule
              _canonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   FunCall ann_ funName_ _argsIcanonicalisedIdentifiersTree
                   {-# LINE 1830 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- self rule
              _lhsOcanonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _canonicalisedIdentifiersTree
                   {-# LINE 1835 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- copy rule (down)
              _argsOcat =
                  ({-# LINE 190 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _lhsIcat
                   {-# LINE 1840 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              ( _argsIcanonicalisedIdentifiersTree) =
                  (args_ _argsOcat )
          in  ( _lhsOcanonicalisedIdentifiersTree)))
sem_ScalarExpression_Identifier :: (a) ->
                                   (String) ->
                                   (T_ScalarExpression) (a) 
sem_ScalarExpression_Identifier (ann_ :: (a)) (i_ :: (String))  =
    (\ (_lhsIcat :: (Catalog))
       (_lhsIenv :: (Environment)) ->
         (let _lhsOcanonicalisedIdentifiersTree :: ScalarExpression a 
              -- self rule
              _canonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   Identifier ann_ i_
                   {-# LINE 1855 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- self rule
              _lhsOcanonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _canonicalisedIdentifiersTree
                   {-# LINE 1860 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
          in  ( _lhsOcanonicalisedIdentifiersTree)))
sem_ScalarExpression_InPredicate :: (a) ->
                                    (T_ScalarExpression) (a)  ->
                                    (Bool) ->
                                    (T_InList) (a)  ->
                                    (T_ScalarExpression) (a) 
sem_ScalarExpression_InPredicate (ann_ :: (a)) (expr_ :: (Catalog) ->
                                                         (Environment) ->
                                                         ( ScalarExpression a )) (i_ :: (Bool)) (list_ :: (Catalog) ->
                                                                                                          ( InList a ))  =
    (\ (_lhsIcat :: (Catalog))
       (_lhsIenv :: (Environment)) ->
         (let _lhsOcanonicalisedIdentifiersTree :: ScalarExpression a 
              _exprOcat :: (Catalog)
              _exprOenv :: (Environment)
              _listOcat :: (Catalog)
              _exprIcanonicalisedIdentifiersTree :: ScalarExpression a 
              _listIcanonicalisedIdentifiersTree :: InList a 
              -- self rule
              _canonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   InPredicate ann_ _exprIcanonicalisedIdentifiersTree i_ _listIcanonicalisedIdentifiersTree
                   {-# LINE 1883 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- self rule
              _lhsOcanonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _canonicalisedIdentifiersTree
                   {-# LINE 1888 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- copy rule (down)
              _exprOcat =
                  ({-# LINE 190 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _lhsIcat
                   {-# LINE 1893 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- copy rule (down)
              _exprOenv =
                  ({-# LINE 146 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _lhsIenv
                   {-# LINE 1898 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- copy rule (down)
              _listOcat =
                  ({-# LINE 190 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _lhsIcat
                   {-# LINE 1903 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              ( _exprIcanonicalisedIdentifiersTree) =
                  (expr_ _exprOcat _exprOenv )
              ( _listIcanonicalisedIdentifiersTree) =
                  (list_ _listOcat )
          in  ( _lhsOcanonicalisedIdentifiersTree)))
sem_ScalarExpression_IntegerLit :: (a) ->
                                   (Integer) ->
                                   (T_ScalarExpression) (a) 
sem_ScalarExpression_IntegerLit (ann_ :: (a)) (i_ :: (Integer))  =
    (\ (_lhsIcat :: (Catalog))
       (_lhsIenv :: (Environment)) ->
         (let _lhsOcanonicalisedIdentifiersTree :: ScalarExpression a 
              -- self rule
              _canonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   IntegerLit ann_ i_
                   {-# LINE 1920 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- self rule
              _lhsOcanonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _canonicalisedIdentifiersTree
                   {-# LINE 1925 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
          in  ( _lhsOcanonicalisedIdentifiersTree)))
sem_ScalarExpression_LiftOperator :: (a) ->
                                     (String) ->
                                     (LiftFlavour) ->
                                     (T_ExpressionList) (a)  ->
                                     (T_ScalarExpression) (a) 
sem_ScalarExpression_LiftOperator (ann_ :: (a)) (oper_ :: (String)) (flav_ :: (LiftFlavour)) (args_ :: (Catalog) ->
                                                                                                       ( ExpressionList a ))  =
    (\ (_lhsIcat :: (Catalog))
       (_lhsIenv :: (Environment)) ->
         (let _lhsOcanonicalisedIdentifiersTree :: ScalarExpression a 
              _argsOcat :: (Catalog)
              _argsIcanonicalisedIdentifiersTree :: ExpressionList a 
              -- self rule
              _canonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   LiftOperator ann_ oper_ flav_ _argsIcanonicalisedIdentifiersTree
                   {-# LINE 1943 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- self rule
              _lhsOcanonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _canonicalisedIdentifiersTree
                   {-# LINE 1948 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- copy rule (down)
              _argsOcat =
                  ({-# LINE 190 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _lhsIcat
                   {-# LINE 1953 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              ( _argsIcanonicalisedIdentifiersTree) =
                  (args_ _argsOcat )
          in  ( _lhsOcanonicalisedIdentifiersTree)))
sem_ScalarExpression_NullLit :: (a) ->
                                (T_ScalarExpression) (a) 
sem_ScalarExpression_NullLit (ann_ :: (a))  =
    (\ (_lhsIcat :: (Catalog))
       (_lhsIenv :: (Environment)) ->
         (let _lhsOcanonicalisedIdentifiersTree :: ScalarExpression a 
              -- self rule
              _canonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   NullLit ann_
                   {-# LINE 1967 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- self rule
              _lhsOcanonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _canonicalisedIdentifiersTree
                   {-# LINE 1972 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
          in  ( _lhsOcanonicalisedIdentifiersTree)))
sem_ScalarExpression_Placeholder :: (a) ->
                                    (T_ScalarExpression) (a) 
sem_ScalarExpression_Placeholder (ann_ :: (a))  =
    (\ (_lhsIcat :: (Catalog))
       (_lhsIenv :: (Environment)) ->
         (let _lhsOcanonicalisedIdentifiersTree :: ScalarExpression a 
              -- self rule
              _canonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   Placeholder ann_
                   {-# LINE 1984 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- self rule
              _lhsOcanonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _canonicalisedIdentifiersTree
                   {-# LINE 1989 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
          in  ( _lhsOcanonicalisedIdentifiersTree)))
sem_ScalarExpression_PositionalArg :: (a) ->
                                      (Integer) ->
                                      (T_ScalarExpression) (a) 
sem_ScalarExpression_PositionalArg (ann_ :: (a)) (p_ :: (Integer))  =
    (\ (_lhsIcat :: (Catalog))
       (_lhsIenv :: (Environment)) ->
         (let _lhsOcanonicalisedIdentifiersTree :: ScalarExpression a 
              -- self rule
              _canonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   PositionalArg ann_ p_
                   {-# LINE 2002 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- self rule
              _lhsOcanonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _canonicalisedIdentifiersTree
                   {-# LINE 2007 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
          in  ( _lhsOcanonicalisedIdentifiersTree)))
sem_ScalarExpression_QIdentifier :: (a) ->
                                    (T_ScalarExpression) (a)  ->
                                    (String) ->
                                    (T_ScalarExpression) (a) 
sem_ScalarExpression_QIdentifier (ann_ :: (a)) (qual_ :: (Catalog) ->
                                                         (Environment) ->
                                                         ( ScalarExpression a )) (i_ :: (String))  =
    (\ (_lhsIcat :: (Catalog))
       (_lhsIenv :: (Environment)) ->
         (let _lhsOcanonicalisedIdentifiersTree :: ScalarExpression a 
              _qualOcat :: (Catalog)
              _qualOenv :: (Environment)
              _qualIcanonicalisedIdentifiersTree :: ScalarExpression a 
              -- self rule
              _canonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   QIdentifier ann_ _qualIcanonicalisedIdentifiersTree i_
                   {-# LINE 2026 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- self rule
              _lhsOcanonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _canonicalisedIdentifiersTree
                   {-# LINE 2031 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- copy rule (down)
              _qualOcat =
                  ({-# LINE 190 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _lhsIcat
                   {-# LINE 2036 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- copy rule (down)
              _qualOenv =
                  ({-# LINE 146 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _lhsIenv
                   {-# LINE 2041 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              ( _qualIcanonicalisedIdentifiersTree) =
                  (qual_ _qualOcat _qualOenv )
          in  ( _lhsOcanonicalisedIdentifiersTree)))
sem_ScalarExpression_ScalarSubQuery :: (a) ->
                                       (T_QueryExpression) (a)  ->
                                       (T_ScalarExpression) (a) 
sem_ScalarExpression_ScalarSubQuery (ann_ :: (a)) (sel_ :: (Catalog) ->
                                                           ( QueryExpression a ,(Environment)))  =
    (\ (_lhsIcat :: (Catalog))
       (_lhsIenv :: (Environment)) ->
         (let _lhsOcanonicalisedIdentifiersTree :: ScalarExpression a 
              _selOcat :: (Catalog)
              _selIcanonicalisedIdentifiersTree :: QueryExpression a 
              _selIcenv :: (Environment)
              -- self rule
              _canonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   ScalarSubQuery ann_ _selIcanonicalisedIdentifiersTree
                   {-# LINE 2060 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- self rule
              _lhsOcanonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _canonicalisedIdentifiersTree
                   {-# LINE 2065 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- copy rule (down)
              _selOcat =
                  ({-# LINE 190 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _lhsIcat
                   {-# LINE 2070 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              ( _selIcanonicalisedIdentifiersTree,_selIcenv) =
                  (sel_ _selOcat )
          in  ( _lhsOcanonicalisedIdentifiersTree)))
sem_ScalarExpression_StringLit :: (a) ->
                                  (String) ->
                                  (T_ScalarExpression) (a) 
sem_ScalarExpression_StringLit (ann_ :: (a)) (value_ :: (String))  =
    (\ (_lhsIcat :: (Catalog))
       (_lhsIenv :: (Environment)) ->
         (let _lhsOcanonicalisedIdentifiersTree :: ScalarExpression a 
              -- self rule
              _canonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   StringLit ann_ value_
                   {-# LINE 2085 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- self rule
              _lhsOcanonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _canonicalisedIdentifiersTree
                   {-# LINE 2090 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
          in  ( _lhsOcanonicalisedIdentifiersTree)))
sem_ScalarExpression_WindowFn :: (a) ->
                                 (T_ScalarExpression) (a)  ->
                                 (T_ExpressionList) (a)  ->
                                 (T_ExpressionList) (a)  ->
                                 (Direction) ->
                                 (FrameClause) ->
                                 (T_ScalarExpression) (a) 
sem_ScalarExpression_WindowFn (ann_ :: (a)) (fn_ :: (Catalog) ->
                                                    (Environment) ->
                                                    ( ScalarExpression a )) (partitionBy_ :: (Catalog) ->
                                                                                             ( ExpressionList a )) (orderBy_ :: (Catalog) ->
                                                                                                                                ( ExpressionList a )) (dir_ :: (Direction)) (frm_ :: (FrameClause))  =
    (\ (_lhsIcat :: (Catalog))
       (_lhsIenv :: (Environment)) ->
         (let _lhsOcanonicalisedIdentifiersTree :: ScalarExpression a 
              _fnOcat :: (Catalog)
              _fnOenv :: (Environment)
              _partitionByOcat :: (Catalog)
              _orderByOcat :: (Catalog)
              _fnIcanonicalisedIdentifiersTree :: ScalarExpression a 
              _partitionByIcanonicalisedIdentifiersTree :: ExpressionList a 
              _orderByIcanonicalisedIdentifiersTree :: ExpressionList a 
              -- self rule
              _canonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   WindowFn ann_ _fnIcanonicalisedIdentifiersTree _partitionByIcanonicalisedIdentifiersTree _orderByIcanonicalisedIdentifiersTree dir_ frm_
                   {-# LINE 2118 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- self rule
              _lhsOcanonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _canonicalisedIdentifiersTree
                   {-# LINE 2123 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- copy rule (down)
              _fnOcat =
                  ({-# LINE 190 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _lhsIcat
                   {-# LINE 2128 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- copy rule (down)
              _fnOenv =
                  ({-# LINE 146 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _lhsIenv
                   {-# LINE 2133 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- copy rule (down)
              _partitionByOcat =
                  ({-# LINE 190 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _lhsIcat
                   {-# LINE 2138 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- copy rule (down)
              _orderByOcat =
                  ({-# LINE 190 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _lhsIcat
                   {-# LINE 2143 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              ( _fnIcanonicalisedIdentifiersTree) =
                  (fn_ _fnOcat _fnOenv )
              ( _partitionByIcanonicalisedIdentifiersTree) =
                  (partitionBy_ _partitionByOcat )
              ( _orderByIcanonicalisedIdentifiersTree) =
                  (orderBy_ _orderByOcat )
          in  ( _lhsOcanonicalisedIdentifiersTree)))
-- SelectItem --------------------------------------------------
{-
   visit 0:
      inherited attribute:
         cat                  : Catalog
      synthesized attribute:
         canonicalisedIdentifiersTree : SELF 
   alternatives:
      alternative SelExp:
         child ann            : {a}
         child ex             : ScalarExpression a
         visit 0:
            local canonicalisedIdentifiersTree : _
      alternative SelectItem:
         child ann            : {a}
         child ex             : ScalarExpression a
         child name           : {String}
         visit 0:
            local canonicalisedIdentifiersTree : _
-}
data SelectItem a  = SelExp (a) (ScalarExpression a ) 
                   | SelectItem (a) (ScalarExpression a ) (String) 
                   deriving ( Data,Eq,Show,Typeable)
-- cata
sem_SelectItem :: (SelectItem) (a)  ->
                  (T_SelectItem) (a) 
sem_SelectItem (SelExp _ann _ex )  =
    (sem_SelectItem_SelExp _ann (sem_ScalarExpression _ex ) )
sem_SelectItem (SelectItem _ann _ex _name )  =
    (sem_SelectItem_SelectItem _ann (sem_ScalarExpression _ex ) _name )
-- semantic domain
type T_SelectItem a  = (Catalog) ->
                       ( SelectItem a )
data Inh_SelectItem a  = Inh_SelectItem {cat_Inh_SelectItem :: (Catalog)}
data Syn_SelectItem a  = Syn_SelectItem {canonicalisedIdentifiersTree_Syn_SelectItem :: SelectItem a }
wrap_SelectItem :: (T_SelectItem) (a)  ->
                   (Inh_SelectItem) (a)  ->
                   (Syn_SelectItem) (a) 
wrap_SelectItem sem (Inh_SelectItem _lhsIcat )  =
    (let ( _lhsOcanonicalisedIdentifiersTree) =
             (sem _lhsIcat )
     in  (Syn_SelectItem _lhsOcanonicalisedIdentifiersTree ))
sem_SelectItem_SelExp :: (a) ->
                         (T_ScalarExpression) (a)  ->
                         (T_SelectItem) (a) 
sem_SelectItem_SelExp (ann_ :: (a)) (ex_ :: (Catalog) ->
                                            (Environment) ->
                                            ( ScalarExpression a ))  =
    (\ (_lhsIcat :: (Catalog)) ->
         (let _lhsOcanonicalisedIdentifiersTree :: SelectItem a 
              _exOcat :: (Catalog)
              _exOenv :: (Environment)
              _exIcanonicalisedIdentifiersTree :: ScalarExpression a 
              -- self rule
              _canonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   SelExp ann_ _exIcanonicalisedIdentifiersTree
                   {-# LINE 2208 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- self rule
              _lhsOcanonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _canonicalisedIdentifiersTree
                   {-# LINE 2213 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- copy rule (down)
              _exOcat =
                  ({-# LINE 190 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _lhsIcat
                   {-# LINE 2218 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- copy rule (chain)
              _exOenv =
                  ({-# LINE 146 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   error "missing rule: SelectItem.SelExp.ex.env"
                   {-# LINE 2223 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              ( _exIcanonicalisedIdentifiersTree) =
                  (ex_ _exOcat _exOenv )
          in  ( _lhsOcanonicalisedIdentifiersTree)))
sem_SelectItem_SelectItem :: (a) ->
                             (T_ScalarExpression) (a)  ->
                             (String) ->
                             (T_SelectItem) (a) 
sem_SelectItem_SelectItem (ann_ :: (a)) (ex_ :: (Catalog) ->
                                                (Environment) ->
                                                ( ScalarExpression a )) (name_ :: (String))  =
    (\ (_lhsIcat :: (Catalog)) ->
         (let _lhsOcanonicalisedIdentifiersTree :: SelectItem a 
              _exOcat :: (Catalog)
              _exOenv :: (Environment)
              _exIcanonicalisedIdentifiersTree :: ScalarExpression a 
              -- self rule
              _canonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   SelectItem ann_ _exIcanonicalisedIdentifiersTree name_
                   {-# LINE 2243 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- self rule
              _lhsOcanonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _canonicalisedIdentifiersTree
                   {-# LINE 2248 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- copy rule (down)
              _exOcat =
                  ({-# LINE 190 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _lhsIcat
                   {-# LINE 2253 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- copy rule (chain)
              _exOenv =
                  ({-# LINE 146 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   error "missing rule: SelectItem.SelectItem.ex.env"
                   {-# LINE 2258 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              ( _exIcanonicalisedIdentifiersTree) =
                  (ex_ _exOcat _exOenv )
          in  ( _lhsOcanonicalisedIdentifiersTree)))
-- SelectItemList ----------------------------------------------
{-
   visit 0:
      inherited attribute:
         cat                  : Catalog
      synthesized attribute:
         canonicalisedIdentifiersTree : SELF 
   alternatives:
      alternative Cons:
         child hd             : SelectItem a
         child tl             : SelectItemList a
         visit 0:
            local canonicalisedIdentifiersTree : _
      alternative Nil:
         visit 0:
            local canonicalisedIdentifiersTree : _
-}
type SelectItemList a  = [SelectItem a ]
-- cata
sem_SelectItemList :: (SelectItemList) (a)  ->
                      (T_SelectItemList) (a) 
sem_SelectItemList list  =
    (Prelude.foldr sem_SelectItemList_Cons sem_SelectItemList_Nil (Prelude.map sem_SelectItem list) )
-- semantic domain
type T_SelectItemList a  = (Catalog) ->
                           ( SelectItemList a )
data Inh_SelectItemList a  = Inh_SelectItemList {cat_Inh_SelectItemList :: (Catalog)}
data Syn_SelectItemList a  = Syn_SelectItemList {canonicalisedIdentifiersTree_Syn_SelectItemList :: SelectItemList a }
wrap_SelectItemList :: (T_SelectItemList) (a)  ->
                       (Inh_SelectItemList) (a)  ->
                       (Syn_SelectItemList) (a) 
wrap_SelectItemList sem (Inh_SelectItemList _lhsIcat )  =
    (let ( _lhsOcanonicalisedIdentifiersTree) =
             (sem _lhsIcat )
     in  (Syn_SelectItemList _lhsOcanonicalisedIdentifiersTree ))
sem_SelectItemList_Cons :: (T_SelectItem) (a)  ->
                           (T_SelectItemList) (a)  ->
                           (T_SelectItemList) (a) 
sem_SelectItemList_Cons (hd_ :: (Catalog) ->
                                ( SelectItem a )) (tl_ :: (Catalog) ->
                                                          ( SelectItemList a ))  =
    (\ (_lhsIcat :: (Catalog)) ->
         (let _lhsOcanonicalisedIdentifiersTree :: SelectItemList a 
              _hdOcat :: (Catalog)
              _tlOcat :: (Catalog)
              _hdIcanonicalisedIdentifiersTree :: SelectItem a 
              _tlIcanonicalisedIdentifiersTree :: SelectItemList a 
              -- self rule
              _canonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   (:) _hdIcanonicalisedIdentifiersTree _tlIcanonicalisedIdentifiersTree
                   {-# LINE 2313 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- self rule
              _lhsOcanonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _canonicalisedIdentifiersTree
                   {-# LINE 2318 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- copy rule (down)
              _hdOcat =
                  ({-# LINE 190 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _lhsIcat
                   {-# LINE 2323 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- copy rule (down)
              _tlOcat =
                  ({-# LINE 190 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _lhsIcat
                   {-# LINE 2328 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              ( _hdIcanonicalisedIdentifiersTree) =
                  (hd_ _hdOcat )
              ( _tlIcanonicalisedIdentifiersTree) =
                  (tl_ _tlOcat )
          in  ( _lhsOcanonicalisedIdentifiersTree)))
sem_SelectItemList_Nil :: (T_SelectItemList) (a) 
sem_SelectItemList_Nil  =
    (\ (_lhsIcat :: (Catalog)) ->
         (let _lhsOcanonicalisedIdentifiersTree :: SelectItemList a 
              -- self rule
              _canonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   []
                   {-# LINE 2342 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- self rule
              _lhsOcanonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _canonicalisedIdentifiersTree
                   {-# LINE 2347 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
          in  ( _lhsOcanonicalisedIdentifiersTree)))
-- SelectList --------------------------------------------------
{-
   visit 0:
      inherited attribute:
         cat                  : Catalog
      synthesized attributes:
         canonicalisedIdentifiersTree : SELF 
         cenv                 : Environment
   alternatives:
      alternative SelectList:
         child ann            : {a}
         child items          : SelectItemList a
         visit 0:
            local canonicalisedIdentifiersTree : _
-}
data SelectList a  = SelectList (a) (SelectItemList a ) 
                   deriving ( Data,Eq,Show,Typeable)
-- cata
sem_SelectList :: (SelectList) (a)  ->
                  (T_SelectList) (a) 
sem_SelectList (SelectList _ann _items )  =
    (sem_SelectList_SelectList _ann (sem_SelectItemList _items ) )
-- semantic domain
type T_SelectList a  = (Catalog) ->
                       ( SelectList a ,(Environment))
data Inh_SelectList a  = Inh_SelectList {cat_Inh_SelectList :: (Catalog)}
data Syn_SelectList a  = Syn_SelectList {canonicalisedIdentifiersTree_Syn_SelectList :: SelectList a ,cenv_Syn_SelectList :: (Environment)}
wrap_SelectList :: (T_SelectList) (a)  ->
                   (Inh_SelectList) (a)  ->
                   (Syn_SelectList) (a) 
wrap_SelectList sem (Inh_SelectList _lhsIcat )  =
    (let ( _lhsOcanonicalisedIdentifiersTree,_lhsOcenv) =
             (sem _lhsIcat )
     in  (Syn_SelectList _lhsOcanonicalisedIdentifiersTree _lhsOcenv ))
sem_SelectList_SelectList :: (a) ->
                             (T_SelectItemList) (a)  ->
                             (T_SelectList) (a) 
sem_SelectList_SelectList (ann_ :: (a)) (items_ :: (Catalog) ->
                                                   ( SelectItemList a ))  =
    (\ (_lhsIcat :: (Catalog)) ->
         (let _lhsOcenv :: (Environment)
              _lhsOcanonicalisedIdentifiersTree :: SelectList a 
              _itemsOcat :: (Catalog)
              _itemsIcanonicalisedIdentifiersTree :: SelectItemList a 
              -- "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag"(line 106, column 18)
              _lhsOcenv =
                  ({-# LINE 106 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   unimplementedEnvironment
                   {-# LINE 2397 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- self rule
              _canonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   SelectList ann_ _itemsIcanonicalisedIdentifiersTree
                   {-# LINE 2402 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- self rule
              _lhsOcanonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _canonicalisedIdentifiersTree
                   {-# LINE 2407 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- copy rule (down)
              _itemsOcat =
                  ({-# LINE 190 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _lhsIcat
                   {-# LINE 2412 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              ( _itemsIcanonicalisedIdentifiersTree) =
                  (items_ _itemsOcat )
          in  ( _lhsOcanonicalisedIdentifiersTree,_lhsOcenv)))
-- TableRef ----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         cat                  : Catalog
      synthesized attributes:
         canonicalisedIdentifiersTree : SELF 
         cenv                 : Environment
   alternatives:
      alternative FunTref:
         child ann            : {a}
         child fn             : ScalarExpression a
         child alias          : {TableAlias}
         visit 0:
            local canonicalisedIdentifiersTree : _
      alternative JoinTref:
         child ann            : {a}
         child tbl            : TableRef a
         child nat            : {Natural}
         child joinType       : {JoinType}
         child tbl1           : TableRef a
         child onExpr         : OnExpr a
         child alias          : {TableAlias}
         visit 0:
            local canonicalisedIdentifiersTree : _
      alternative SubTref:
         child ann            : {a}
         child sel            : QueryExpression a
         child alias          : {TableAlias}
         visit 0:
            local canonicalisedIdentifiersTree : _
      alternative Tref:
         child ann            : {a}
         child tbl            : ScalarExpression a
         child alias          : {TableAlias}
         visit 0:
            local canonicalisedIdentifiersTree : _
-}
data TableRef a  = FunTref (a) (ScalarExpression a ) (TableAlias) 
                 | JoinTref (a) (TableRef a ) (Natural) (JoinType) (TableRef a ) (OnExpr a ) (TableAlias) 
                 | SubTref (a) (QueryExpression a ) (TableAlias) 
                 | Tref (a) (ScalarExpression a ) (TableAlias) 
                 deriving ( Data,Eq,Show,Typeable)
-- cata
sem_TableRef :: (TableRef) (a)  ->
                (T_TableRef) (a) 
sem_TableRef (FunTref _ann _fn _alias )  =
    (sem_TableRef_FunTref _ann (sem_ScalarExpression _fn ) _alias )
sem_TableRef (JoinTref _ann _tbl _nat _joinType _tbl1 _onExpr _alias )  =
    (sem_TableRef_JoinTref _ann (sem_TableRef _tbl ) _nat _joinType (sem_TableRef _tbl1 ) (sem_OnExpr _onExpr ) _alias )
sem_TableRef (SubTref _ann _sel _alias )  =
    (sem_TableRef_SubTref _ann (sem_QueryExpression _sel ) _alias )
sem_TableRef (Tref _ann _tbl _alias )  =
    (sem_TableRef_Tref _ann (sem_ScalarExpression _tbl ) _alias )
-- semantic domain
type T_TableRef a  = (Catalog) ->
                     ( TableRef a ,(Environment))
data Inh_TableRef a  = Inh_TableRef {cat_Inh_TableRef :: (Catalog)}
data Syn_TableRef a  = Syn_TableRef {canonicalisedIdentifiersTree_Syn_TableRef :: TableRef a ,cenv_Syn_TableRef :: (Environment)}
wrap_TableRef :: (T_TableRef) (a)  ->
                 (Inh_TableRef) (a)  ->
                 (Syn_TableRef) (a) 
wrap_TableRef sem (Inh_TableRef _lhsIcat )  =
    (let ( _lhsOcanonicalisedIdentifiersTree,_lhsOcenv) =
             (sem _lhsIcat )
     in  (Syn_TableRef _lhsOcanonicalisedIdentifiersTree _lhsOcenv ))
sem_TableRef_FunTref :: (a) ->
                        (T_ScalarExpression) (a)  ->
                        (TableAlias) ->
                        (T_TableRef) (a) 
sem_TableRef_FunTref (ann_ :: (a)) (fn_ :: (Catalog) ->
                                           (Environment) ->
                                           ( ScalarExpression a )) (alias_ :: (TableAlias))  =
    (\ (_lhsIcat :: (Catalog)) ->
         (let _lhsOcenv :: (Environment)
              _lhsOcanonicalisedIdentifiersTree :: TableRef a 
              _fnOcat :: (Catalog)
              _fnOenv :: (Environment)
              _fnIcanonicalisedIdentifiersTree :: ScalarExpression a 
              -- "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag"(line 103, column 37)
              _lhsOcenv =
                  ({-# LINE 103 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   unimplementedEnvironment
                   {-# LINE 2499 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- self rule
              _canonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   FunTref ann_ _fnIcanonicalisedIdentifiersTree alias_
                   {-# LINE 2504 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- self rule
              _lhsOcanonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _canonicalisedIdentifiersTree
                   {-# LINE 2509 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- copy rule (down)
              _fnOcat =
                  ({-# LINE 190 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _lhsIcat
                   {-# LINE 2514 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- copy rule (chain)
              _fnOenv =
                  ({-# LINE 146 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   error "missing rule: TableRef.FunTref.fn.env"
                   {-# LINE 2519 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              ( _fnIcanonicalisedIdentifiersTree) =
                  (fn_ _fnOcat _fnOenv )
          in  ( _lhsOcanonicalisedIdentifiersTree,_lhsOcenv)))
sem_TableRef_JoinTref :: (a) ->
                         (T_TableRef) (a)  ->
                         (Natural) ->
                         (JoinType) ->
                         (T_TableRef) (a)  ->
                         (T_OnExpr) (a)  ->
                         (TableAlias) ->
                         (T_TableRef) (a) 
sem_TableRef_JoinTref (ann_ :: (a)) (tbl_ :: (Catalog) ->
                                             ( TableRef a ,(Environment))) (nat_ :: (Natural)) (joinType_ :: (JoinType)) (tbl1_ :: (Catalog) ->
                                                                                                                                   ( TableRef a ,(Environment))) (onExpr_ :: (Catalog) ->
                                                                                                                                                                             (Environment) ->
                                                                                                                                                                             ( OnExpr a )) (alias_ :: (TableAlias))  =
    (\ (_lhsIcat :: (Catalog)) ->
         (let _lhsOcenv :: (Environment)
              _onExprOenv :: (Environment)
              _lhsOcanonicalisedIdentifiersTree :: TableRef a 
              _tblOcat :: (Catalog)
              _tbl1Ocat :: (Catalog)
              _onExprOcat :: (Catalog)
              _tblIcanonicalisedIdentifiersTree :: TableRef a 
              _tblIcenv :: (Environment)
              _tbl1IcanonicalisedIdentifiersTree :: TableRef a 
              _tbl1Icenv :: (Environment)
              _onExprIcanonicalisedIdentifiersTree :: OnExpr a 
              -- "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag"(line 103, column 37)
              _lhsOcenv =
                  ({-# LINE 103 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   unimplementedEnvironment
                   {-# LINE 2552 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag"(line 152, column 16)
              _onExprOenv =
                  ({-# LINE 152 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   unimplementedEnvironment
                   {-# LINE 2557 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- self rule
              _canonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   JoinTref ann_ _tblIcanonicalisedIdentifiersTree nat_ joinType_ _tbl1IcanonicalisedIdentifiersTree _onExprIcanonicalisedIdentifiersTree alias_
                   {-# LINE 2562 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- self rule
              _lhsOcanonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _canonicalisedIdentifiersTree
                   {-# LINE 2567 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- copy rule (down)
              _tblOcat =
                  ({-# LINE 190 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _lhsIcat
                   {-# LINE 2572 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- copy rule (down)
              _tbl1Ocat =
                  ({-# LINE 190 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _lhsIcat
                   {-# LINE 2577 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- copy rule (down)
              _onExprOcat =
                  ({-# LINE 190 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _lhsIcat
                   {-# LINE 2582 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              ( _tblIcanonicalisedIdentifiersTree,_tblIcenv) =
                  (tbl_ _tblOcat )
              ( _tbl1IcanonicalisedIdentifiersTree,_tbl1Icenv) =
                  (tbl1_ _tbl1Ocat )
              ( _onExprIcanonicalisedIdentifiersTree) =
                  (onExpr_ _onExprOcat _onExprOenv )
          in  ( _lhsOcanonicalisedIdentifiersTree,_lhsOcenv)))
sem_TableRef_SubTref :: (a) ->
                        (T_QueryExpression) (a)  ->
                        (TableAlias) ->
                        (T_TableRef) (a) 
sem_TableRef_SubTref (ann_ :: (a)) (sel_ :: (Catalog) ->
                                            ( QueryExpression a ,(Environment))) (alias_ :: (TableAlias))  =
    (\ (_lhsIcat :: (Catalog)) ->
         (let _lhsOcenv :: (Environment)
              _lhsOcanonicalisedIdentifiersTree :: TableRef a 
              _selOcat :: (Catalog)
              _selIcanonicalisedIdentifiersTree :: QueryExpression a 
              _selIcenv :: (Environment)
              -- "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag"(line 103, column 37)
              _lhsOcenv =
                  ({-# LINE 103 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   unimplementedEnvironment
                   {-# LINE 2606 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- self rule
              _canonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   SubTref ann_ _selIcanonicalisedIdentifiersTree alias_
                   {-# LINE 2611 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- self rule
              _lhsOcanonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _canonicalisedIdentifiersTree
                   {-# LINE 2616 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- copy rule (down)
              _selOcat =
                  ({-# LINE 190 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _lhsIcat
                   {-# LINE 2621 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              ( _selIcanonicalisedIdentifiersTree,_selIcenv) =
                  (sel_ _selOcat )
          in  ( _lhsOcanonicalisedIdentifiersTree,_lhsOcenv)))
sem_TableRef_Tref :: (a) ->
                     (T_ScalarExpression) (a)  ->
                     (TableAlias) ->
                     (T_TableRef) (a) 
sem_TableRef_Tref (ann_ :: (a)) (tbl_ :: (Catalog) ->
                                         (Environment) ->
                                         ( ScalarExpression a )) (alias_ :: (TableAlias))  =
    (\ (_lhsIcat :: (Catalog)) ->
         (let _lhsOcenv :: (Environment)
              _lhsOcanonicalisedIdentifiersTree :: TableRef a 
              _tblOcat :: (Catalog)
              _tblOenv :: (Environment)
              _tblIcanonicalisedIdentifiersTree :: ScalarExpression a 
              -- "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag"(line 103, column 37)
              _lhsOcenv =
                  ({-# LINE 103 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   unimplementedEnvironment
                   {-# LINE 2642 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- self rule
              _canonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   Tref ann_ _tblIcanonicalisedIdentifiersTree alias_
                   {-# LINE 2647 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- self rule
              _lhsOcanonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _canonicalisedIdentifiersTree
                   {-# LINE 2652 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- copy rule (down)
              _tblOcat =
                  ({-# LINE 190 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _lhsIcat
                   {-# LINE 2657 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- copy rule (chain)
              _tblOenv =
                  ({-# LINE 146 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   error "missing rule: TableRef.Tref.tbl.env"
                   {-# LINE 2662 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              ( _tblIcanonicalisedIdentifiersTree) =
                  (tbl_ _tblOcat _tblOenv )
          in  ( _lhsOcanonicalisedIdentifiersTree,_lhsOcenv)))
-- TableRefList ------------------------------------------------
{-
   visit 0:
      inherited attribute:
         cat                  : Catalog
      synthesized attributes:
         canonicalisedIdentifiersTree : SELF 
         cenv                 : Environment
   alternatives:
      alternative Cons:
         child hd             : TableRef a
         child tl             : TableRefList a
         visit 0:
            local canonicalisedIdentifiersTree : _
      alternative Nil:
         visit 0:
            local canonicalisedIdentifiersTree : _
-}
type TableRefList a  = [TableRef a ]
-- cata
sem_TableRefList :: (TableRefList) (a)  ->
                    (T_TableRefList) (a) 
sem_TableRefList list  =
    (Prelude.foldr sem_TableRefList_Cons sem_TableRefList_Nil (Prelude.map sem_TableRef list) )
-- semantic domain
type T_TableRefList a  = (Catalog) ->
                         ( TableRefList a ,(Environment))
data Inh_TableRefList a  = Inh_TableRefList {cat_Inh_TableRefList :: (Catalog)}
data Syn_TableRefList a  = Syn_TableRefList {canonicalisedIdentifiersTree_Syn_TableRefList :: TableRefList a ,cenv_Syn_TableRefList :: (Environment)}
wrap_TableRefList :: (T_TableRefList) (a)  ->
                     (Inh_TableRefList) (a)  ->
                     (Syn_TableRefList) (a) 
wrap_TableRefList sem (Inh_TableRefList _lhsIcat )  =
    (let ( _lhsOcanonicalisedIdentifiersTree,_lhsOcenv) =
             (sem _lhsIcat )
     in  (Syn_TableRefList _lhsOcanonicalisedIdentifiersTree _lhsOcenv ))
sem_TableRefList_Cons :: (T_TableRef) (a)  ->
                         (T_TableRefList) (a)  ->
                         (T_TableRefList) (a) 
sem_TableRefList_Cons (hd_ :: (Catalog) ->
                              ( TableRef a ,(Environment))) (tl_ :: (Catalog) ->
                                                                    ( TableRefList a ,(Environment)))  =
    (\ (_lhsIcat :: (Catalog)) ->
         (let _lhsOcenv :: (Environment)
              _lhsOcanonicalisedIdentifiersTree :: TableRefList a 
              _hdOcat :: (Catalog)
              _tlOcat :: (Catalog)
              _hdIcanonicalisedIdentifiersTree :: TableRef a 
              _hdIcenv :: (Environment)
              _tlIcanonicalisedIdentifiersTree :: TableRefList a 
              _tlIcenv :: (Environment)
              -- "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag"(line 125, column 12)
              _lhsOcenv =
                  ({-# LINE 125 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   joinEnvironments _hdIcenv _tlIcenv
                   {-# LINE 2721 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- self rule
              _canonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   (:) _hdIcanonicalisedIdentifiersTree _tlIcanonicalisedIdentifiersTree
                   {-# LINE 2726 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- self rule
              _lhsOcanonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _canonicalisedIdentifiersTree
                   {-# LINE 2731 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- copy rule (down)
              _hdOcat =
                  ({-# LINE 190 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _lhsIcat
                   {-# LINE 2736 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- copy rule (down)
              _tlOcat =
                  ({-# LINE 190 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _lhsIcat
                   {-# LINE 2741 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              ( _hdIcanonicalisedIdentifiersTree,_hdIcenv) =
                  (hd_ _hdOcat )
              ( _tlIcanonicalisedIdentifiersTree,_tlIcenv) =
                  (tl_ _tlOcat )
          in  ( _lhsOcanonicalisedIdentifiersTree,_lhsOcenv)))
sem_TableRefList_Nil :: (T_TableRefList) (a) 
sem_TableRefList_Nil  =
    (\ (_lhsIcat :: (Catalog)) ->
         (let _lhsOcenv :: (Environment)
              _lhsOcanonicalisedIdentifiersTree :: TableRefList a 
              -- "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag"(line 126, column 11)
              _lhsOcenv =
                  ({-# LINE 126 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   emptyEnvironment
                   {-# LINE 2756 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- self rule
              _canonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   []
                   {-# LINE 2761 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- self rule
              _lhsOcanonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _canonicalisedIdentifiersTree
                   {-# LINE 2766 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
          in  ( _lhsOcanonicalisedIdentifiersTree,_lhsOcenv)))
-- TypeName ----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         cat                  : Catalog
      synthesized attribute:
         canonicalisedIdentifiersTree : SELF 
   alternatives:
      alternative ArrayTypeName:
         child ann            : {a}
         child typ            : TypeName a
         visit 0:
            local canonicalisedIdentifiersTree : _
      alternative PrecTypeName:
         child ann            : {a}
         child tn             : {String}
         child prec           : {Integer}
         visit 0:
            local canonicalisedIdentifiersTree : _
      alternative SetOfTypeName:
         child ann            : {a}
         child typ            : TypeName a
         visit 0:
            local canonicalisedIdentifiersTree : _
      alternative SimpleTypeName:
         child ann            : {a}
         child tn             : {String}
         visit 0:
            local canonicalisedIdentifiersTree : _
-}
data TypeName a  = ArrayTypeName (a) (TypeName a ) 
                 | PrecTypeName (a) (String) (Integer) 
                 | SetOfTypeName (a) (TypeName a ) 
                 | SimpleTypeName (a) (String) 
                 deriving ( Data,Eq,Show,Typeable)
-- cata
sem_TypeName :: (TypeName) (a)  ->
                (T_TypeName) (a) 
sem_TypeName (ArrayTypeName _ann _typ )  =
    (sem_TypeName_ArrayTypeName _ann (sem_TypeName _typ ) )
sem_TypeName (PrecTypeName _ann _tn _prec )  =
    (sem_TypeName_PrecTypeName _ann _tn _prec )
sem_TypeName (SetOfTypeName _ann _typ )  =
    (sem_TypeName_SetOfTypeName _ann (sem_TypeName _typ ) )
sem_TypeName (SimpleTypeName _ann _tn )  =
    (sem_TypeName_SimpleTypeName _ann _tn )
-- semantic domain
type T_TypeName a  = (Catalog) ->
                     ( TypeName a )
data Inh_TypeName a  = Inh_TypeName {cat_Inh_TypeName :: (Catalog)}
data Syn_TypeName a  = Syn_TypeName {canonicalisedIdentifiersTree_Syn_TypeName :: TypeName a }
wrap_TypeName :: (T_TypeName) (a)  ->
                 (Inh_TypeName) (a)  ->
                 (Syn_TypeName) (a) 
wrap_TypeName sem (Inh_TypeName _lhsIcat )  =
    (let ( _lhsOcanonicalisedIdentifiersTree) =
             (sem _lhsIcat )
     in  (Syn_TypeName _lhsOcanonicalisedIdentifiersTree ))
sem_TypeName_ArrayTypeName :: (a) ->
                              (T_TypeName) (a)  ->
                              (T_TypeName) (a) 
sem_TypeName_ArrayTypeName (ann_ :: (a)) (typ_ :: (Catalog) ->
                                                  ( TypeName a ))  =
    (\ (_lhsIcat :: (Catalog)) ->
         (let _lhsOcanonicalisedIdentifiersTree :: TypeName a 
              _typOcat :: (Catalog)
              _typIcanonicalisedIdentifiersTree :: TypeName a 
              -- self rule
              _canonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   ArrayTypeName ann_ _typIcanonicalisedIdentifiersTree
                   {-# LINE 2839 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- self rule
              _lhsOcanonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _canonicalisedIdentifiersTree
                   {-# LINE 2844 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- copy rule (down)
              _typOcat =
                  ({-# LINE 190 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _lhsIcat
                   {-# LINE 2849 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              ( _typIcanonicalisedIdentifiersTree) =
                  (typ_ _typOcat )
          in  ( _lhsOcanonicalisedIdentifiersTree)))
sem_TypeName_PrecTypeName :: (a) ->
                             (String) ->
                             (Integer) ->
                             (T_TypeName) (a) 
sem_TypeName_PrecTypeName (ann_ :: (a)) (tn_ :: (String)) (prec_ :: (Integer))  =
    (\ (_lhsIcat :: (Catalog)) ->
         (let _lhsOcanonicalisedIdentifiersTree :: TypeName a 
              -- self rule
              _canonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   PrecTypeName ann_ tn_ prec_
                   {-# LINE 2864 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- self rule
              _lhsOcanonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _canonicalisedIdentifiersTree
                   {-# LINE 2869 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
          in  ( _lhsOcanonicalisedIdentifiersTree)))
sem_TypeName_SetOfTypeName :: (a) ->
                              (T_TypeName) (a)  ->
                              (T_TypeName) (a) 
sem_TypeName_SetOfTypeName (ann_ :: (a)) (typ_ :: (Catalog) ->
                                                  ( TypeName a ))  =
    (\ (_lhsIcat :: (Catalog)) ->
         (let _lhsOcanonicalisedIdentifiersTree :: TypeName a 
              _typOcat :: (Catalog)
              _typIcanonicalisedIdentifiersTree :: TypeName a 
              -- self rule
              _canonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   SetOfTypeName ann_ _typIcanonicalisedIdentifiersTree
                   {-# LINE 2884 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- self rule
              _lhsOcanonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _canonicalisedIdentifiersTree
                   {-# LINE 2889 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- copy rule (down)
              _typOcat =
                  ({-# LINE 190 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _lhsIcat
                   {-# LINE 2894 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              ( _typIcanonicalisedIdentifiersTree) =
                  (typ_ _typOcat )
          in  ( _lhsOcanonicalisedIdentifiersTree)))
sem_TypeName_SimpleTypeName :: (a) ->
                               (String) ->
                               (T_TypeName) (a) 
sem_TypeName_SimpleTypeName (ann_ :: (a)) (tn_ :: (String))  =
    (\ (_lhsIcat :: (Catalog)) ->
         (let _lhsOcanonicalisedIdentifiersTree :: TypeName a 
              -- self rule
              _canonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   SimpleTypeName ann_ tn_
                   {-# LINE 2908 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- self rule
              _lhsOcanonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _canonicalisedIdentifiersTree
                   {-# LINE 2913 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
          in  ( _lhsOcanonicalisedIdentifiersTree)))
-- WithQuery ---------------------------------------------------
{-
   visit 0:
      inherited attribute:
         cat                  : Catalog
      synthesized attribute:
         canonicalisedIdentifiersTree : SELF 
   alternatives:
      alternative WithQuery:
         child ann            : {a}
         child name           : {String}
         child ex             : QueryExpression a
         visit 0:
            local canonicalisedIdentifiersTree : _
-}
data WithQuery a  = WithQuery (a) (String) (QueryExpression a ) 
                  deriving ( Data,Eq,Show,Typeable)
-- cata
sem_WithQuery :: (WithQuery) (a)  ->
                 (T_WithQuery) (a) 
sem_WithQuery (WithQuery _ann _name _ex )  =
    (sem_WithQuery_WithQuery _ann _name (sem_QueryExpression _ex ) )
-- semantic domain
type T_WithQuery a  = (Catalog) ->
                      ( WithQuery a )
data Inh_WithQuery a  = Inh_WithQuery {cat_Inh_WithQuery :: (Catalog)}
data Syn_WithQuery a  = Syn_WithQuery {canonicalisedIdentifiersTree_Syn_WithQuery :: WithQuery a }
wrap_WithQuery :: (T_WithQuery) (a)  ->
                  (Inh_WithQuery) (a)  ->
                  (Syn_WithQuery) (a) 
wrap_WithQuery sem (Inh_WithQuery _lhsIcat )  =
    (let ( _lhsOcanonicalisedIdentifiersTree) =
             (sem _lhsIcat )
     in  (Syn_WithQuery _lhsOcanonicalisedIdentifiersTree ))
sem_WithQuery_WithQuery :: (a) ->
                           (String) ->
                           (T_QueryExpression) (a)  ->
                           (T_WithQuery) (a) 
sem_WithQuery_WithQuery (ann_ :: (a)) (name_ :: (String)) (ex_ :: (Catalog) ->
                                                                  ( QueryExpression a ,(Environment)))  =
    (\ (_lhsIcat :: (Catalog)) ->
         (let _lhsOcanonicalisedIdentifiersTree :: WithQuery a 
              _exOcat :: (Catalog)
              _exIcanonicalisedIdentifiersTree :: QueryExpression a 
              _exIcenv :: (Environment)
              -- self rule
              _canonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   WithQuery ann_ name_ _exIcanonicalisedIdentifiersTree
                   {-# LINE 2964 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- self rule
              _lhsOcanonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _canonicalisedIdentifiersTree
                   {-# LINE 2969 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- copy rule (down)
              _exOcat =
                  ({-# LINE 190 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _lhsIcat
                   {-# LINE 2974 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              ( _exIcanonicalisedIdentifiersTree,_exIcenv) =
                  (ex_ _exOcat )
          in  ( _lhsOcanonicalisedIdentifiersTree)))
-- WithQueryList -----------------------------------------------
{-
   visit 0:
      inherited attribute:
         cat                  : Catalog
      synthesized attribute:
         canonicalisedIdentifiersTree : SELF 
   alternatives:
      alternative Cons:
         child hd             : WithQuery a
         child tl             : WithQueryList a
         visit 0:
            local canonicalisedIdentifiersTree : _
      alternative Nil:
         visit 0:
            local canonicalisedIdentifiersTree : _
-}
type WithQueryList a  = [WithQuery a ]
-- cata
sem_WithQueryList :: (WithQueryList) (a)  ->
                     (T_WithQueryList) (a) 
sem_WithQueryList list  =
    (Prelude.foldr sem_WithQueryList_Cons sem_WithQueryList_Nil (Prelude.map sem_WithQuery list) )
-- semantic domain
type T_WithQueryList a  = (Catalog) ->
                          ( WithQueryList a )
data Inh_WithQueryList a  = Inh_WithQueryList {cat_Inh_WithQueryList :: (Catalog)}
data Syn_WithQueryList a  = Syn_WithQueryList {canonicalisedIdentifiersTree_Syn_WithQueryList :: WithQueryList a }
wrap_WithQueryList :: (T_WithQueryList) (a)  ->
                      (Inh_WithQueryList) (a)  ->
                      (Syn_WithQueryList) (a) 
wrap_WithQueryList sem (Inh_WithQueryList _lhsIcat )  =
    (let ( _lhsOcanonicalisedIdentifiersTree) =
             (sem _lhsIcat )
     in  (Syn_WithQueryList _lhsOcanonicalisedIdentifiersTree ))
sem_WithQueryList_Cons :: (T_WithQuery) (a)  ->
                          (T_WithQueryList) (a)  ->
                          (T_WithQueryList) (a) 
sem_WithQueryList_Cons (hd_ :: (Catalog) ->
                               ( WithQuery a )) (tl_ :: (Catalog) ->
                                                        ( WithQueryList a ))  =
    (\ (_lhsIcat :: (Catalog)) ->
         (let _lhsOcanonicalisedIdentifiersTree :: WithQueryList a 
              _hdOcat :: (Catalog)
              _tlOcat :: (Catalog)
              _hdIcanonicalisedIdentifiersTree :: WithQuery a 
              _tlIcanonicalisedIdentifiersTree :: WithQueryList a 
              -- self rule
              _canonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   (:) _hdIcanonicalisedIdentifiersTree _tlIcanonicalisedIdentifiersTree
                   {-# LINE 3029 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- self rule
              _lhsOcanonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _canonicalisedIdentifiersTree
                   {-# LINE 3034 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- copy rule (down)
              _hdOcat =
                  ({-# LINE 190 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _lhsIcat
                   {-# LINE 3039 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- copy rule (down)
              _tlOcat =
                  ({-# LINE 190 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _lhsIcat
                   {-# LINE 3044 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              ( _hdIcanonicalisedIdentifiersTree) =
                  (hd_ _hdOcat )
              ( _tlIcanonicalisedIdentifiersTree) =
                  (tl_ _tlOcat )
          in  ( _lhsOcanonicalisedIdentifiersTree)))
sem_WithQueryList_Nil :: (T_WithQueryList) (a) 
sem_WithQueryList_Nil  =
    (\ (_lhsIcat :: (Catalog)) ->
         (let _lhsOcanonicalisedIdentifiersTree :: WithQueryList a 
              -- self rule
              _canonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   []
                   {-# LINE 3058 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
              -- self rule
              _lhsOcanonicalisedIdentifiersTree =
                  ({-# LINE 192 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals//TypeChecking/CanonicaliseIdentifiers.ag" #-}
                   _canonicalisedIdentifiersTree
                   {-# LINE 3063 "/home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.hs" #-})
          in  ( _lhsOcanonicalisedIdentifiersTree)))