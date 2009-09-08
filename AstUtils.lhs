Copyright 2009 Jake Wheat

This file holds all the non-short bits of code that are mainly used in
TypeChecking.ag.

random implementation note:
If you see one of these: TypeList [] - and don't get it - is used to
represent a variety of different things, like node type checked ok
when the node doesn't produce a type but can produce a type error,
etc.. This could probably be reviewed and made to work a bit better.

> module AstUtils where

> import Data.Maybe
> import Data.List
> import qualified Data.Map as M
> import Control.Monad.Error
> import Debug.Trace

> import TypeType
> import Scope
> import DefaultScope


> data OperatorType = BinaryOp | PrefixOp | PostfixOp
>                   deriving (Eq,Show)

for now, assume that all the overloaded operators that have the
same name are all either binary, prefix or postfix, otherwise the
getoperatortype would need the types of the arguments to determine
the operator type, and the parser would have to be a lot cleverer

> getOperatorType :: String -> OperatorType
> getOperatorType s = case () of
>                       _ | any (\(x,_,_) -> x == s) (scopeBinaryOperators defaultScope) ->
>                             BinaryOp
>                         | any (\(x,_,_) -> x == s ||
>                                            (x=="-" && s=="u-"))
>                               (scopePrefixOperators defaultScope) ->
>                             PrefixOp
>                         | any (\(x,_,_) -> x == s) (scopePostfixOperators defaultScope) ->
>                             PostfixOp
>                         | s `elem` ["!and", "!or","!like"] -> BinaryOp
>                         | s `elem` ["!not"] -> PrefixOp
>                         | s `elem` ["!isNull", "!isNotNull"] -> PostfixOp
>                         | otherwise ->
>                             error $ "don't know flavour of operator " ++ s

================================================================================
Error reporting

> data Message = Error MySourcePos MessageStuff
>              | Warning MySourcePos MessageStuff
>              | Notice MySourcePos MessageStuff
>                deriving (Eq)
>
> data MessageStuff = ContinueNotInLoop
>                   | CustomMessage String
>                     deriving (Eq,Show)
>
> instance Show Message where
>    show m = showMessage m
>
> showMessage :: Message -> [Char]
> showMessage m = case m of
>                   Error sp s -> showit "Error" sp s
>                   Warning sp s -> showit "Warning" sp s
>                   Notice sp s -> showit "Notice" sp s
>                 where
>                   showit lev (fn,l,c) s = lev ++ "\n" ++ fn ++ ":"
>                                           ++ show l ++ ":" ++ show c ++ ":\n"
>                                           ++ show s ++ "\n"
>

================================================================================

= type checking utils

== unkErr

shorthand used with catMaybe

takes a type and returns any type errors, or if no errors, unknowns,
returns nothing if it doesn't find any type errors or unknowns. Looks
at the immediate type, or inside the first level if passed a type
list, or unnamedcompositetype.


> unkErr :: Type -> Maybe Type
> unkErr t =
>     case t of
>       a@(TypeError _ _) -> Just a
>       UnknownType -> Just UnknownType
>       TypeList l -> doTypeList l
>       UnnamedCompositeType c -> doTypeList (map snd c)
>       _ -> Nothing
>     where
>       -- run through the type list, if there are any errors, collect
>       -- them all into a list
>       -- otherwise, if there are any unknowns, then the type is
>       -- unknown
>       -- otherwise, return nothing
>       doTypeList ts =
>           let unks = filter (\u -> case u of
>                                      UnknownType -> True
>                                      _ -> False) ts
>               errs = filter (\u -> case u of
>                                      TypeError _ _ -> True
>                                      _ -> False) ts
>           in case () of
>                _ | length errs > 0 ->
>                      Just $ case () of
>                                     _ | length errs == 1 -> head errs
>                                       | otherwise -> TypeList errs
>                  | length unks > 0 -> Just UnknownType
>                  | otherwise -> Nothing

> checkErrors :: [Type] -> Type -> Type
> checkErrors (t:ts) r = case unkErr t of
>                        Just e -> e
>                        Nothing -> checkErrors ts r
> checkErrors [] r = r

======


> checkTypeExists :: Scope -> MySourcePos -> Type -> Type
> checkTypeExists scope sp t =
>     if t `elem` (scopeTypes scope)
>       then TypeList [] -- this works with the checkErrors function
>       else TypeError sp (UnknownTypeError t)



================================================================================

= basic types

random notes on pg types:

== domains:
the point of domains is you can't put constraints on types, but you
can wrap a type up in a domain and put a constraint on it there

== literals/selectors

source strings are parsed as unknown type: they can be implicitly cast
to almost any type in the right contexts.

rows ctors can also be implicitly cast to any composite type matching
the elements (how exactly are they matched? - number of elements, type
compatibility of elements, just by context?)

string literals are not checked for valid syntax currently, but this
will probably change so we can type check string literals statically,
whereas pg defers all checking to runtime, because it has to cope with
custom data types. this code isn't going to be able to support such
custom data types very well, so it can get away with doing more static
checks on this sort of thing.

== notes on type checking types

=== basic type checking
at the moment - just check type exists in predetermined list of type
names
todo: option to read types from database catalog at time of type
checking
todo: collect type names from current source file to check against
A lot of the infrastructure to do this is already in place. We also
need to do this for all other definitions, etc.

Type aliases

Some types in postgresql have multiple names. I think this is
hardcoded in the pg parser.

For the canonical name, we use the name given in the postgresql
pg_type catalog relvar.

TODO: Change the ast canonical names: where there is a choice, prefer
the sql standard name, where there are multiple sql standard names,
choose the most concise or common one, so the ast will use different
canonical names to postgresql.

planned ast canonical names:
numbers:
int2, int4/integer, int8 -> smallint, int, bigint
numeric, decimal -> numeric
float(1) to float(24), real -> float(24)
float, float(25) to float(53), double precision -> float
serial, serial4 -> int
bigserial, serial8 -> bigint
character varying(n), varchar(n)-> varchar(n)
character(n), char(n) -> char(n)

TODO:

what about PrecTypeName? - need to fix the ast and parser (these are
called type modifiers in pg)

also, what can setof be applied to - don't know if it can apply to an
array or setof type

array types have to match an exact array type in the catalog, so we
can't create an arbitrary array of any type



aliases to protect client code if/when the ast canonical names are
changed



> typeSmallInt,typeBigInt,typeInt,typeNumeric,typeFloat4,
>   typeFloat8,typeVarChar,typeChar,typeBool :: Type
> typeSmallInt = ScalarType "int2"
> typeBigInt = ScalarType "int8"
> typeInt = ScalarType "int4"
> typeNumeric = ScalarType "numeric"
> typeFloat4 = ScalarType "float4"
> typeFloat8 = ScalarType "float8"
> typeVarChar = ScalarType "varchar"
> typeChar = ScalarType "char"
> typeBool = ScalarType "bool"

> canonicalizeType :: Type -> Type
> canonicalizeType t =
>     case t of
>       ScalarType s -> cName s
>       ArrayType a -> ArrayType $ canonicalizeType a
>       SetOfType a -> SetOfType $ canonicalizeType a
>       t1@_ -> t1
>     where
>       cName s = case () of
>                   _ | s `elem` smallIntNames -> typeSmallInt
>                     | s `elem` intNames -> typeInt
>                     | s `elem` bigIntNames -> typeBigInt
>                     | s `elem` numericNames -> typeNumeric
>                     | s `elem` float4Names -> typeFloat4
>                     | s `elem` float8Names -> typeFloat8
>                     | s `elem` varcharNames -> typeVarChar
>                     | s `elem` charNames -> typeChar
>                     | s `elem` boolNames -> typeBool
>                     | otherwise -> ScalarType s
>       smallIntNames = ["int2", "smallint"]
>       intNames = ["int4", "integer", "int"]
>       bigIntNames = ["int8", "bigint"]
>       numericNames = ["numeric", "decimal"]
>       float4Names = ["real", "float4"]
>       float8Names = ["double precision", "float"]
>       varcharNames = ["character varying", "varchar"]
>       charNames = ["character", "char"]
>       boolNames = ["boolean", "bool"]

================================================================================

> keywordOperatorTypes :: [(String,[Type],Type)]
> keywordOperatorTypes = [
>   ("!and", [typeBool, typeBool], typeBool)
>  ,("!or", [typeBool, typeBool], typeBool)
>  ,("!like", [ScalarType "text", ScalarType "text"], typeBool)
>  ,("!not", [typeBool], typeBool)
>  ,("!isNull", [Pseudo AnyElement], typeBool)
>  ,("!isNotNull", [Pseudo AnyElement], typeBool)
>  ,("!arrayCtor", [Pseudo AnyElement], Pseudo AnyArray) -- not quite right,
>                                         -- needs variadic support
>                                         -- currently works via a special case
>  ,("!between", [Pseudo AnyElement
>               ,Pseudo AnyElement
>               ,Pseudo AnyElement], Pseudo AnyElement)
>  ,("!substring", [ScalarType "text",typeInt,typeInt], ScalarType "text")
>  ,("!arraySub", [Pseudo AnyArray,typeInt], Pseudo AnyElement)
>  ]

todo:
polymorphic functions
row ctor implicitly and explicitly cast to a composite type
cast empty array

findCallMatch - partially implements the type conversion rules for
finding an operator or function match given a name and list of
arguments with partial or fully specified types

TODO:
namespaces
function style casts not in catalog
variadic args
default args
domains -> base type

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
now we have a list of types, use resolveResultSetType to see if we
can produce a match, if so, create a new prototype which is the same
as the polymorphic function but with these args swapped in, work out the
casts and add it into cand cast pairs, after exact match has been run.


findCallMatch is a bit of a mess

> type ProtArgCast = (FunctionPrototype, [ArgCastFlavour])

> findCallMatch :: Scope -> MySourcePos -> String -> [Type] ->  Either Type FunctionPrototype
> findCallMatch scope sp f inArgs =
>     returnIfOnne [
>        exactMatch
>       ,binOp1UnknownMatch
>       ,polymorpicExactMatches
>       ,reachable
>       ,filteredForPreferred
>       ,unknownMatchesByCat]
>       (TypeError sp (NoMatchingOperator f inArgs))
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
>       castPairs = map listCastPairs $ map getFnArgs initialCandList
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
>                                        case isPreferredType scope ca of
>                                          True -> ImplicitToPreferred
>                                          False -> ImplicitToNonPreferred
>                                    | otherwise -> CannotCast
>                               ) : listCastPairs' ias cas
>                           listCastPairs' [] [] = []
>                           listCastPairs' _ _ = error "internal error: mismatched num args in implicit cast algorithm"
>
>
>       getBinOp1UnknownMatch :: [ProtArgCast] -> [ProtArgCast]
>       getBinOp1UnknownMatch cands =
>           if not (isOperator f &&
>                   length inArgs == 2 &&
>                   (count (==UnknownStringLit) inArgs) == 1)
>             then []
>             else let newInArgs =
>                          take 2 $ repeat (if head inArgs == UnknownStringLit
>                                             then inArgs !! 1
>                                             else head inArgs)
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
>                 catMaybes $ map (\(t,p) -> case t of
>                                              Nothing -> Nothing
>                                              Just t' -> Just (t',p))
>                               polyTypePairs
>               finalRows = map (\(t,p) -> instantiatePolyType p t)
>                               keepPolyTypePairs
>               --create the new cast lists
>               cps :: [[ArgCastFlavour]]
>               cps = map listCastPairs $ map (getFnArgs . fst) finalRows
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
>                      Pseudo AnyArray -> if isArrayType ia
>                                           then nextMatch
>                                           else False
>                      Pseudo AnyElement -> nextMatch
>                      Pseudo AnyEnum -> False
>                      Pseudo AnyNonArray -> if isArrayType ia
>                                              then False
>                                              else nextMatch
>                      _ -> True
>                    where
>                      nextMatch = canMatch' ias pas
>                  canMatch' _ _ = error "internal error"
>             resolvePolyType :: ProtArgCast -> Maybe Type
>             resolvePolyType ((_,fnArgs,_),_) =
>                 {-trace ("\nresolving " ++ show fnArgs ++ " against " ++ show inArgs ++ "\n") $-}
>                 let argPairs = zip inArgs fnArgs
>                     typeList = catMaybes $ flip map argPairs
>                                  (\(ia,fa) -> case fa of
>                                                   Pseudo Any -> if isArrayType ia
>                                                                          then Just $ typeFromArray ia
>                                                                          else Just $ ia
>                                                   Pseudo AnyArray -> Just $ typeFromArray ia
>                                                   Pseudo AnyElement -> if isArrayType ia
>                                                                          then Just $ typeFromArray ia
>                                                                          else Just $ ia
>                                                   Pseudo AnyEnum -> Nothing
>                                                   Pseudo AnyNonArray -> Just ia
>                                                   _ -> Nothing)
>                 in {-trace ("\nresolve types: " ++ show typeList ++ "\n") $-}
>                    case resolveResultSetType scope sp typeList of
>                      UnknownType -> Nothing
>                      TypeError _ _ -> Nothing
>                      x -> Just x
>             instantiatePolyType :: ProtArgCast -> Type -> ProtArgCast
>             instantiatePolyType pac t =
>               let ((fn,a,r),_) = pac
>                   instArgs = swapPolys t a
>                   p1 = (fn, instArgs, swapPoly t r)
>               in let x = (p1,listCastPairs instArgs)
>                  in {-trace ("\nfixed:" ++ show x ++ "\n")-} x
>               where
>                 swapPolys :: Type -> [Type] -> [Type]
>                 swapPolys pit l = map (swapPoly pit) l
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
>               filteredPolys = filter (\((_,a,_),_) -> not (a `elem` origArgs)) polys
>           in orig ++ filteredPolys
>
>       countPreferredTypeCasts :: [ProtArgCast] -> [Int]
>       countPreferredTypeCasts l =
>           map (\(_,cp) -> count (==ImplicitToPreferred) cp) l
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
>                            prefMatches = filter (\c -> isPreferredType scope
>                                                        (getTypeForArgN n c)) catMatches
>                            keepMatches :: [ProtArgCast]
>                            keepMatches = if length prefMatches > 0
>                                            then prefMatches
>                                            else catMatches
>                        in getMatches keepMatches (n + 1)
>            getTypeForArgN :: Int -> ProtArgCast -> Type
>            getTypeForArgN n ((_,a,_),_) = a !! n
>            getCatForArgN :: Int -> ProtArgCast -> String
>            getCatForArgN n c = getTypeCategory scope (getTypeForArgN n c)
>
>       -- utils
>       -- filter a candidate/cast flavours pair by a predicate on each
>       -- individual cast flavour
>       filterCandCastPairs :: ([ArgCastFlavour] -> Bool)
>                           -> [ProtArgCast]
>                           -> [ProtArgCast]
>       filterCandCastPairs predi ccp = filter (\(_,cp) -> predi cp) ccp
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
>       allFns = keywordOperatorTypes ++ scopeAllFns scope

>       none p l = not (any p l)
>       count p l = length $ filter p l
>
> data ArgCastFlavour = ExactMatch
>                     | CannotCast
>                     | ImplicitToPreferred
>                     | ImplicitToNonPreferred
>                       deriving (Eq,Show)
>
> isOperator :: String -> Bool
> isOperator s = any (`elem` "+-*/<>=~!@#%^&|`?") s
>
> isPreferredType :: Scope -> Type -> Bool
> isPreferredType scope t = case find (\(t1,_,_)-> t1==t) (scopeTypeCategories scope) of
>                       Nothing -> error $ "internal error, couldn't find type category information: " ++ show t
>                       Just (_,_,p) -> p
>
> getTypeCategory :: Scope -> Type -> String
> getTypeCategory scope t = case find (\(t1,_,_)-> t1==t) (scopeTypeCategories scope) of
>                       Nothing -> error $ "internal error, couldn't find type category information: " ++ show t
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


> resolveResultSetType :: Scope -> MySourcePos -> [Type] -> Type
> resolveResultSetType scope sp inArgs =
>    checkErrors [TypeList inArgs] ret
>    where
>      ret = case () of
>                    _ | length inArgs == 0 -> TypeError sp TypelessEmptyArray
>                      | allSameType -> head inArgs
>                      --todo: do domains
>                      | allUnknown -> ScalarType "text"
>                      | not allSameCat -> TypeError sp (IncompatibleTypes inArgs)
>                      | isJust targetType &&
>                          allConvertibleToFrom
>                          (fromJust targetType)
>                          inArgs -> fromJust targetType
>                      | otherwise -> TypeError sp (IncompatibleTypes inArgs)
>      allSameType = all (== head inArgs) inArgs && head inArgs /= UnknownStringLit
>      allUnknown = all (==UnknownStringLit) inArgs
>      allSameCat = let firstCat = getTypeCategory scope (head knownTypes)
>                   in all (\t -> getTypeCategory scope t == firstCat) knownTypes
>      targetType = case catMaybes [firstPreferred, lastAllConvertibleTo] of
>                     [] -> Nothing
>                     (x:_) -> Just x
>      firstPreferred = find (isPreferredType scope) knownTypes
>      lastAllConvertibleTo = firstAllConvertibleTo (reverse knownTypes)
>      firstAllConvertibleTo (x:xs) = if allConvertibleToFrom x xs
>                                       then Just x
>                                       else firstAllConvertibleTo xs
>      firstAllConvertibleTo [] = Nothing
>      matchOrImplicitToFrom t t1 = t == t1 || implicitlyCastableFromTo scope t1 t
>      knownTypes = filter (/=UnknownStringLit) inArgs
>      allConvertibleToFrom t ts = all (matchOrImplicitToFrom t) ts


> getAttrs :: Scope -> [CompositeFlavour] -> String -> Maybe CompositeDef
> getAttrs scope f n = find (\(nm,fl,_) ->
>                                fl `elem` f
>                                  && nm == n)
>                          (scopeAttrDefs scope)


> combineTableTypesWithUsingList :: Scope -> MySourcePos -> [String] -> Type -> Type -> Type
> combineTableTypesWithUsingList scope sp l t1c t2c =
>     --check t1 and t2 have l
>     let t1 = getTypesFromComp t1c
>         t2 = getTypesFromComp t2c
>         names1 = getNames t1
>         names2 = getNames t2
>         error1 = if not (contained l names1) ||
>                     not (contained l names2)
>                    then TypeError sp MissingJoinAttribute
>                    else TypeList []
>         --check the types
>         joinColumns = map (getColumnType t1 t2) l
>         nonJoinColumns =
>             let notJoin = (\(s,_) -> not (s `elem` l))
>             in filter notJoin t1 ++ filter notJoin t2
>     in checkErrors [error1]
>                    (UnnamedCompositeType $ joinColumns ++ nonJoinColumns)
>     where
>       getNames :: [(String,Type)] -> [String]
>       getNames = map fst
>       contained l1 l2 = all (`elem` l2) l1
>       getColumnType t1 t2 f =
>           let ct1 = getFieldType t1 f
>               ct2 = getFieldType t2 f
>           in (f, resolveResultSetType scope sp [ct1,ct2])
>       getFieldType t f = snd $ fromJust $ find (\(s,_) -> s == f) t

> getCompositeFromSetOf :: Type -> Type
> getCompositeFromSetOf (SetOfType a@(UnnamedCompositeType _)) = a
> getCompositeFromSetOf _ = UnnamedCompositeType []

> getColumnsAsTypes :: Type -> M.Map String Type
> getColumnsAsTypes (UnnamedCompositeType a) = M.fromList a
> getColumnsAsTypes _ = M.empty

> getTypesFromComp :: Type -> [(String,Type)]
> getTypesFromComp (UnnamedCompositeType a) = a
> getTypesFromComp _ = []

> getTypesFromComp2 :: Type -> [Type]
> getTypesFromComp2 (UnnamedCompositeType a) = map snd a
> getTypesFromComp2 _ = []


> typeCheckFunCall :: Scope -> MySourcePos -> FunName -> Type -> Type
> typeCheckFunCall scope sp fnName argsType =
>     checkErrors [argsType] ret
>     where
>       ret = case fnName of
>           Operator "!arrayCtor" -> let t = resolveResultSetType scope sp $ typesFromTypeList argsType
>                                    in checkErrors [t] $ ArrayType t
>           Operator "!between" -> let f1 = lookupFn ">=" [as !! 0, as !! 1]
>                                      f2 = lookupFn "<=" [as !! 0, as !! 2]
>                                      f3 = lookupFn "!and" [f1,f2]
>                                  in checkErrors [f1,f2] f3
>                                  where
>                                    as = typesFromTypeList argsType
>           Operator s ->  lookupFn s (typesFromTypeList argsType)
>           SimpleFun f -> lookupFn f (typesFromTypeList argsType)
>           RowCtor -> UnknownType
>       lookupFn s1 args = case findCallMatch scope sp
>                                              (if s1 == "u-" then "-" else s1) args of
>                                Left te -> te
>                                Right (_,_,r) -> r


> typeCheckValuesExpr :: Scope -> MySourcePos -> Type -> Type
> typeCheckValuesExpr scope sp vll =
>         let rowsTs1 = typesFromTypeList vll
>             --convert into [[Type]]
>             rowsTs = map typesFromTypeList rowsTs1
>             --check all same length
>             lengths = map length rowsTs
>             error1 = case () of
>                       _ | length rowsTs1 == 0 ->
>                             TypeError sp NoRowsGivenForValues
>                         | not (all (==head lengths) lengths) ->
>                             TypeError sp
>                                  ValuesListsMustBeSameLength
>                         | otherwise -> TypeList []
>             colNames = map (\(a,b) -> a ++ b) $
>                        zip (repeat "column")
>                            (map show [1..head lengths])
>             colTypeLists = transpose rowsTs
>             colTypes = map (resolveResultSetType scope sp) colTypeLists
>             ty = SetOfType $ UnnamedCompositeType $ zip colNames colTypes
>         in checkErrors (error1:colTypes) ty

> appendCompositeTypes :: Type -> Type -> Type
> appendCompositeTypes (UnnamedCompositeType a) (UnnamedCompositeType b) =
>     UnnamedCompositeType (a ++ b)
> appendCompositeTypes _ _ = error "internal error"

> addCompositeFields :: Type -> [(String,Type)] -> Type
> addCompositeFields (UnnamedCompositeType a) l =
>     UnnamedCompositeType (a ++ l)
> addCompositeFields _ _ = error "internal error"

> consCompositeField :: (String,Type) -> Type -> Type
> consCompositeField l (UnnamedCompositeType a) =
>     UnnamedCompositeType (l:a)
> consCompositeField _ _ = error "internal error"

> data TInternalError = TInternalError String
>                      deriving (Eq, Ord, Show)

> instance Error TInternalError where
>    noMsg  = TInternalError "oh noes!"
>    strMsg = TInternalError