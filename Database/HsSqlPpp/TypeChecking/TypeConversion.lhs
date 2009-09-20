Copyright 2009 Jake Wheat

This file contains the functions for resolving types and
function/operator resolution. See the pg manual chapter 10:

http://www.postgresql.org/docs/8.4/interactive/typeconv.html

This code is really spaghettified.

findCallMatch - pass in a name and a list of arguments, and it returns
the matching function. (pg manual 10.2,10.3)

resolveResultSetType - pass in a set of types, and it tries to find
the common type they can all be cast to. (pg manual 10.5)

checkAssignmentValid - pass in source type and target type, returns
                typelist[] if ok, otherwise error, pg manual 10.4
                Value Storage

> {-# OPTIONS_HADDOCK hide #-}

> module Database.HsSqlPpp.TypeChecking.TypeConversion (
>                        findCallMatch
>                       ,resolveResultSetType
>                       ,checkAssignmentValid
>                       ) where

> import Data.Maybe
> import Data.List

> import Database.HsSqlPpp.TypeChecking.TypeType
> import Database.HsSqlPpp.TypeChecking.Scope
> import Database.HsSqlPpp.TypeChecking.AstUtils
> import Database.HsSqlPpp.TypeChecking.EnvironmentInternal


= findCallMatch

findCallMatch - partially implements the type conversion rules for
finding an operator or function match given a name and list of
arguments with partial or fully specified types

TODO:, qualifiers
namespaces
function style casts not in catalog
variadic args
default args
domains -> base type
what about aggregates and window functions?

Algo:

cands = all fns with matching names
        and same number of args

if exact match on types in this list, use it
  (if binary operator being matched, and one arg is typed and one is
  unknown, also match an operator by assuming the unknown is the same
  as the typed arg)

best match part:

filter cands with args which don't exactly match input args, and input
args cannot be converted by an implicit cast. unknowns count as
matching anything
if one left: use it

filter for preferred types:

for each cand, count each arg at each position which needs conversion,
and the cand type is a preferred type at that position.
if there are cands with count>0, keep only cands with the max count,
if one return it
if there are no cands with count>0, keep them all

check unknowns:
if any input args are unknown, and any cand accepts string at that
position, fix that arg's category as string, otherwise if all cands
accept same category at that position, fix that input args as that
category.
if we still have unknowns, then fail

discard cands which don't match the new input arg/category list

for each categorised input arg, if any cand accepts preferred type at
that position, get rid of cands which don't accept preferred type at
that position

if one left: use
else fail

polymorphic matching:
want to create a set of matches to insert into the cast pairs list
so:

find all matches on name, num args and have polymorphic parameters

for each one, check the polymorphic categories - eliminate fns that
have params in wrong category - array, non array, enum.
work out the base types for the polymorphic args at each spot based on
the args passed - so each arg is unchanged except arrays which have
the array part stripped off

now we have a list of types to match against the polymorphic params,
use resolveResultSetType to see if we can produce a match, if so,
create a new prototype which is the same as the polymorphic function
but with this matching arg swapped in, work out the casts and add it
into cand cast pairs, after exact match has been run.


findCallMatch is a bit of a mess

> type ProtArgCast = (FunctionPrototype, [ArgCastFlavour])

> findCallMatch :: Scope -> String -> [Type] ->  Either [TypeError] FunctionPrototype
> findCallMatch scope f inArgs =
>     returnIfOnne [
>        exactMatch
>       ,binOp1UnknownMatch
>       ,polymorpicExactMatches
>       ,reachable
>       ,mostExactMatches
>       ,filteredForPreferred
>       ,unknownMatchesByCat]
>       [NoMatchingOperator f inArgs]
>     where
>       -- basic lists which roughly mirror algo
>       -- get the possibly matching candidates
>       initialCandList :: [FunctionPrototype]
>       initialCandList = filter (\(candf,candArgs,_) ->
>                                   (candf,length candArgs) == (f,length inArgs))
>                           allFns
>
>       -- record what casts are needed for each candidate
>       castPairs :: [[ArgCastFlavour]]
>       castPairs = map (listCastPairs . getFnArgs) initialCandList
>
>       candCastPairs :: [ProtArgCast]
>       candCastPairs = zip initialCandList castPairs
>
>       -- see if we have an exact match
>       exactMatch :: [ProtArgCast]
>       exactMatch = filterCandCastPairs (all (==ExactMatch)) candCastPairs
>
>       -- implement the one known, one unknown resolution for binary operators
>       binOp1UnknownMatch :: [ProtArgCast]
>       binOp1UnknownMatch = getBinOp1UnknownMatch candCastPairs
>
>       --collect possible polymorphic matches
>       polymorphicMatches :: [ProtArgCast]
>       polymorphicMatches = filterPolymorphics candCastPairs
>
>       polymorpicExactMatches :: [ProtArgCast]
>       polymorpicExactMatches = filterCandCastPairs (all (==ExactMatch)) polymorphicMatches
>
>       -- eliminate candidates for which the inargs cannot be casted to
>       reachable :: [ProtArgCast]
>       reachable = mergePolys (filterCandCastPairs (none (==CannotCast)) candCastPairs)
>                     polymorphicMatches
>
>       mostExactMatches :: [ProtArgCast]
>       mostExactMatches =
>         let inArgsBase = map (replaceWithBase scope) inArgs
>             exactCounts :: [Int]
>             exactCounts =
>               map ((length
>                       . filter (\(a1,a2) -> a1==replaceWithBase scope a2)
>                       . zip inArgsBase)
>                 . (\((_,a,_),_) -> a)) reachable
>             pairs = zip reachable exactCounts
>             maxm = maximum exactCounts
>         in case () of
>              _ | null reachable -> []
>                | maxm > 0 -> map fst $ filter (\(_,b) -> b == maxm) pairs
>                | otherwise -> []
>
>       -- keep the cands with the most casts to preferred types
>       preferredTypesCounts = countPreferredTypeCasts reachable
>       keepCounts = maximum preferredTypesCounts
>       itemCountPairs :: [(ProtArgCast,Int)]
>       itemCountPairs = zip reachable preferredTypesCounts
>       filteredForPreferred :: [ProtArgCast]
>       filteredForPreferred = map fst $ filter (\(_,i) -> i == keepCounts) itemCountPairs
>
>       -- collect the inArg type categories to do unknown inArg resolution
>       argCats :: [Either () String]
>       argCats = getCastCategoriesForUnknowns filteredForPreferred
>       unknownMatchesByCat :: [ProtArgCast]
>       unknownMatchesByCat = getCandCatMatches filteredForPreferred argCats
>
>       -------------
>
>       listCastPairs :: [Type] -> [ArgCastFlavour]
>       listCastPairs l = listCastPairs' inArgs l
>                         where
>                           listCastPairs' :: [Type] -> [Type] -> [ArgCastFlavour]
>                           listCastPairs' (ia:ias) (ca:cas) =
>                               (case () of
>                                  _ | ia == ca -> ExactMatch
>                                    | implicitlyCastableFromTo scope ia ca ->
>                                        if isPreferredType scope ca
>                                          then ImplicitToPreferred
>                                          else ImplicitToNonPreferred
>                                    | otherwise -> CannotCast
>                               ) : listCastPairs' ias cas
>                           listCastPairs' [] [] = []
>                           listCastPairs' _ _ = error "internal error: mismatched num args in implicit cast algorithm"
>
>
>       getBinOp1UnknownMatch :: [ProtArgCast] -> [ProtArgCast]
>       getBinOp1UnknownMatch cands =
>           if not (isOperatorName f &&
>                   length inArgs == 2 &&
>                   count (==UnknownStringLit) inArgs == 1)
>             then []
>             else let newInArgs =
>                          replicate 2 (if head inArgs == UnknownStringLit
>                                         then inArgs !! 1
>                                         else head inArgs)
>                  in filter (\((_,a,_),_) -> a == newInArgs) cands
>
>       filterPolymorphics :: [ProtArgCast] -> [ProtArgCast]
>       filterPolymorphics cl =
>           let ms :: [ProtArgCast]
>               ms = filter canMatch polys
>               polyTypes :: [Maybe Type]
>               polyTypes = map resolvePolyType ms
>               polyTypePairs :: [(Maybe Type, ProtArgCast)]
>               polyTypePairs = zip polyTypes ms
>               keepPolyTypePairs :: [(Type, ProtArgCast)]
>               keepPolyTypePairs =
>                 mapMaybe (\(t,p) -> case t of
>                                            Nothing -> Nothing
>                                            Just t' -> Just (t',p))
>                               polyTypePairs
>               finalRows = map (\(t,p) -> instantiatePolyType p t)
>                               keepPolyTypePairs
>               --create the new cast lists
>               cps :: [[ArgCastFlavour]]
>               cps = map (listCastPairs . getFnArgs . fst) finalRows
>           in zip (map fst finalRows) cps
>           where
>             polys :: [ProtArgCast]
>             polys = filter (\((_,a,_),_) -> any (`elem`
>                                              [Pseudo Any
>                                              ,Pseudo AnyArray
>                                              ,Pseudo AnyElement
>                                              ,Pseudo AnyEnum
>                                              ,Pseudo AnyNonArray]) a) cl
>             canMatch :: ProtArgCast -> Bool
>             canMatch pac =
>                let ((_,fnArgs,_),_) = pac
>                in canMatch' inArgs fnArgs
>                where
>                  canMatch' [] [] = True
>                  canMatch' (ia:ias) (pa:pas) =
>                    case pa of
>                      Pseudo Any -> nextMatch
>                      Pseudo AnyArray -> isArrayType ia && nextMatch
>                      Pseudo AnyElement -> nextMatch
>                      Pseudo AnyEnum -> False
>                      Pseudo AnyNonArray -> if isArrayType ia
>                                              then False
>                                              else nextMatch
>                      _ -> True
>                    where
>                      nextMatch = canMatch' ias pas
>                  canMatch' _ _ = error "internal error: mismatched lists in canMatch'"
>             resolvePolyType :: ProtArgCast -> Maybe Type
>             resolvePolyType ((_,fnArgs,_),_) =
>                 {-trace ("\nresolving " ++ show fnArgs ++ " against " ++ show inArgs ++ "\n") $-}
>                 let argPairs = zip inArgs fnArgs
>                     typeList = catMaybes $ flip map argPairs
>                                  (\(ia,fa) -> case fa of
>                                                   Pseudo Any -> if isArrayType ia
>                                                                          then Just $ unwrapArray ia
>                                                                          else Just ia
>                                                   Pseudo AnyArray -> Just $ unwrapArray ia
>                                                   Pseudo AnyElement -> if isArrayType ia
>                                                                          then Just $ unwrapArray ia
>                                                                          else Just ia
>                                                   Pseudo AnyEnum -> Nothing
>                                                   Pseudo AnyNonArray -> Just ia
>                                                   _ -> Nothing)
>                 in {-trace ("\nresolve types: " ++ show typeList ++ "\n") $-}
>                    case resolveResultSetType scope typeList of
>                      Left _ -> Nothing
>                      Right t -> Just t
>             instantiatePolyType :: ProtArgCast -> Type -> ProtArgCast
>             instantiatePolyType pac t =
>               let ((fn,a,r),_) = pac
>                   instArgs = swapPolys t a
>                   p1 = (fn, instArgs, swapPoly t r)
>               in let x = (p1,listCastPairs instArgs)
>                  in {-trace ("\nfixed:" ++ show x ++ "\n")-} x
>               where
>                 swapPolys :: Type -> [Type] -> [Type]
>                 swapPolys = map . swapPoly
>                 swapPoly :: Type -> Type -> Type
>                 swapPoly pit at =
>                   case at of
>                     Pseudo Any -> if isArrayType at
>                                     then ArrayType pit
>                                     else pit
>                     Pseudo AnyArray -> ArrayType pit
>                     Pseudo AnyElement -> if isArrayType at
>                                            then ArrayType pit
>                                            else pit
>                     Pseudo AnyEnum -> pit
>                     Pseudo AnyNonArray -> pit
>                     _ -> at
>       --merge in the instantiated poly functions, with a twist:
>       -- if we already have the exact same set of args in the non poly list
>       -- as a poly, then don't include that poly
>       mergePolys :: [ProtArgCast] -> [ProtArgCast] -> [ProtArgCast]
>       mergePolys orig polys =
>           let origArgs = map (\((_,a,_),_) -> a) orig
>               filteredPolys = filter (\((_,a,_),_) -> a `notElem` origArgs) polys
>           in orig ++ filteredPolys
>
>       countPreferredTypeCasts :: [ProtArgCast] -> [Int]
>       countPreferredTypeCasts =
>           map (\(_,cp) -> count (==ImplicitToPreferred) cp)
>
>       -- Left () is used for inArgs which aren't unknown,
>       --                      and for unknowns which we don't have a
>       --                      unique category
>       -- Right s -> s is the single letter category at
>       --                           that position
>       getCastCategoriesForUnknowns :: [ProtArgCast] -> [Either () String]
>       getCastCategoriesForUnknowns cands =
>           filterArgN 0
>           where
>             candArgLists :: [[Type]]
>             candArgLists = map (\((_,a,_), _) -> a) cands
>             filterArgN :: Int -> [Either () String]
>             filterArgN n =
>                 if n == length inArgs
>                   then []
>                   else let targType = inArgs !! n
>                        in ((if targType /= UnknownStringLit
>                               then Left ()
>                               else getCandsCatAt n) : filterArgN (n+1))
>                 where
>                   getCandsCatAt :: Int -> Either () String
>                   getCandsCatAt n' =
>                       let typesAtN = map (!!n') candArgLists
>                           catsAtN = map (getTypeCategory scope) typesAtN
>                       in case () of
>                            --if any are string choose string
>                            _ | any (== "S") catsAtN -> Right "S"
>                              -- if all are same cat choose that
>                              | all (== head catsAtN) catsAtN -> Right $ head catsAtN
>                              -- otherwise no match, this will be
>                              -- picked up as complete failure to match
>                              -- later on
>                              | otherwise -> Left ()
>
>       getCandCatMatches :: [ProtArgCast] -> [Either () String] -> [ProtArgCast]
>       getCandCatMatches candsA cats = getMatches candsA 0
>          where
>            getMatches :: [ProtArgCast] -> Int -> [ProtArgCast]
>            getMatches cands n =
>                case () of
>                  _ | n == length inArgs -> cands
>                    | (inArgs !! n) /= UnknownStringLit -> getMatches cands (n + 1)
>                    | otherwise ->
>                        let catMatches :: [ProtArgCast]
>                            catMatches = filter (\c -> Right (getCatForArgN n c) ==
>                                                      (cats !! n)) cands
>                            prefMatches :: [ProtArgCast]
>                            prefMatches = filter (isPreferredType scope .
>                                                    getTypeForArgN n) catMatches
>                            keepMatches :: [ProtArgCast]
>                            keepMatches = if length prefMatches > 0
>                                            then prefMatches
>                                            else catMatches
>                        in getMatches keepMatches (n + 1)
>            getTypeForArgN :: Int -> ProtArgCast -> Type
>            getTypeForArgN n ((_,a,_),_) = a !! n
>            getCatForArgN :: Int -> ProtArgCast -> String
>            getCatForArgN n = getTypeCategory scope . getTypeForArgN n
>
>       -- utils
>       -- filter a candidate/cast flavours pair by a predicate on each
>       -- individual cast flavour
>       filterCandCastPairs :: ([ArgCastFlavour] -> Bool)
>                           -> [ProtArgCast]
>                           -> [ProtArgCast]
>       filterCandCastPairs predi = filter (\(_,cp) -> predi cp)
>
>       getFnArgs :: FunctionPrototype -> [Type]
>       getFnArgs (_,a,_) = a
>       returnIfOnne [] e = Left e
>       returnIfOnne (l:ls) e = if length l == 1
>                               then Right $ getHeadFn l
>                               else returnIfOnne ls e
>
>       getHeadFn :: [ProtArgCast] -> FunctionPrototype
>       getHeadFn l =  let ((hdFn, _):_) = l
>                      in hdFn
>       allFns = keywordOperatorTypes ++ specialFunctionTypes ++ scopeAllFns scope

>       none p = not . any p
>       count p = length . filter p
>
> data ArgCastFlavour = ExactMatch
>                     | CannotCast
>                     | ImplicitToPreferred
>                     | ImplicitToNonPreferred
>                       deriving (Eq,Show)
>
> isPreferredType :: Scope -> Type -> Bool
> isPreferredType scope t = case find (\(t1,_,_)-> t1==t) (scopeTypeCategories scope) of
>                       Nothing -> error $ "internal error: couldn't find type category information: " ++ show t
>                       Just (_,_,p) -> p
>
> getTypeCategory :: Scope -> Type -> String
> getTypeCategory scope t = case find (\(t1,_,_)-> t1==t) (scopeTypeCategories scope) of
>                       Nothing -> error $ "internal error: couldn't find type category information: " ++ show t
>                       Just (_,c,_) -> c
>
>
> implicitlyCastableFromTo :: Scope -> Type -> Type -> Bool
> implicitlyCastableFromTo scope from to = from == UnknownStringLit ||
>                                      any (==(from,to,ImplicitCastContext)) (scopeCasts scope)
>


resolveResultSetType -
partially implement the typing of results sets where the types aren't
all the same and not unknown
used in union,except,intersect columns, case, array ctor, values, greatest and least

algo:
if all inputs are same and not unknown -> that type
replace domains with base types
if all inputs are unknown then text
if the non unknown types aren't all in same category then fail
choose first input type that is a preferred type if there is one
choose last non unknown type that has implicit casts from all preceding inputs
check all can convert to selected type else fail

code is not as much of a mess as findCallMatch

> resolveResultSetType :: Scope -> [Type] -> Either [TypeError] Type
> resolveResultSetType scope inArgs =
>   chainTypeCheckFailed inArgs $ do
>       case () of
>               _ | null inArgs -> Left [TypelessEmptyArray]
>                 | allSameType -> Right $ head inArgs
>                 | allSameBaseType -> Right $ head inArgsBase
>                 --todo: do domains
>                 | allUnknown -> Right $ ScalarType "text"
>                 | not allSameCat ->
>                     Left [IncompatibleTypeSet inArgs]
>                 | isJust targetType &&
>                   allConvertibleToFrom (fromJust targetType) inArgs ->
>                       Right $ fromJust targetType
>                 | otherwise -> Left [IncompatibleTypeSet inArgs]
>   where
>      allSameType = all (== head inArgs) inArgs &&
>                      head inArgs /= UnknownStringLit
>      allSameBaseType = all (== head inArgsBase) inArgsBase &&
>                      head inArgsBase /= UnknownStringLit
>      inArgsBase = map (replaceWithBase scope) inArgs
>      allUnknown = all (==UnknownStringLit) inArgsBase
>      allSameCat = let firstCat = getTypeCategory scope (head knownTypes)
>                   in all (\t -> getTypeCategory scope t == firstCat)
>                          knownTypes
>      targetType = case catMaybes [firstPreferred, lastAllConvertibleTo] of
>                     [] -> Nothing
>                     (x:_) -> Just x
>      firstPreferred = find (isPreferredType scope) knownTypes
>      lastAllConvertibleTo = firstAllConvertibleTo (reverse knownTypes)
>      firstAllConvertibleTo (x:xs) = if allConvertibleToFrom x xs
>                                       then Just x
>                                       else firstAllConvertibleTo xs
>      firstAllConvertibleTo [] = Nothing
>      matchOrImplicitToFrom t t1 = t == t1 ||
>                                   implicitlyCastableFromTo scope t1 t
>      knownTypes = filter (/=UnknownStringLit) inArgsBase
>      allConvertibleToFrom = all . matchOrImplicitToFrom

> replaceWithBase :: Scope -> Type -> Type
> replaceWithBase scope t@(DomainType _) =
>   case lookup t (scopeDomainDefs scope) of
>     Nothing -> error $ "internal error - couldn't find base type for " ++ show t
>     Just u -> replaceWithBase scope u
> replaceWithBase _ t = t

todo:
row ctor implicitly and explicitly cast to a composite type
cast empty array, where else can an empty array work?

================================================================================

= checkAssignmentValue

assignment is ok if:
types are equal
there is a cast from src to target

> checkAssignmentValid :: Scope -> Type -> Type -> Either [TypeError] ()
> checkAssignmentValid scope src tgt =
>     case () of
>       _ | src == tgt -> Right()
>         | assignCastableFromTo scope src tgt -> Right ()
>         | otherwise -> Left [IncompatibleTypes tgt src]

> assignCastableFromTo :: Scope -> Type -> Type -> Bool
> assignCastableFromTo scope from to = from == UnknownStringLit ||
>                                any (`elem` [(from,to,ImplicitCastContext)
>                                            ,(from,to,AssignmentCastContext)]) (scopeCasts scope)
