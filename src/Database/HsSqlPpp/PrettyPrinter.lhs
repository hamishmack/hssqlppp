
> {- | Functions to convert sql asts to valid SQL source code. Includes
>    a function - 'printSqlAnn' - to output the annotations from a tree
>    in comments in the outputted SQL source.
>
>    Produces sort of readable code, but mainly just written to produce
>    reparsable text. Could do with some work to make the outputted text
>    layout better.
> -}
> {-# LANGUAGE PatternGuards #-}
> module Database.HsSqlPpp.PrettyPrinter
>     (
>      printScalarExpression
>     ,printQueryExpression
>     ,printQueryExpressions
>     ) where
>
> import Text.PrettyPrint
> import Data.Char
>
> import Database.HsSqlPpp.Ast
> --import Database.HsSqlPpp.Annotation
> import Database.HsSqlPpp.Catalog
> import Database.HsSqlPpp.Utils.Utils

--------------------------------------------------------------------------------

Public functions

 > -- | convert an ast back to valid SQL source, it's also almost human readable.
 > printSql :: StatementList -> String
 > printSql = printSqlAnn (const "")

 >
 > -- | convert the ast back to valid source, and convert any annotations to
 > -- text using the function provided and interpolate the output of
 > -- this function(inside comments) with the SQL source.
 > printSqlAnn :: (Annotation -> String) -> StatementList -> String
 > printSqlAnn f ast = render $ vcat (map (convStatement f) ast) <> text "\n"

>
> -- | Testing function, pretty print an expression
> printScalarExpression :: ScalarExpression a -> String
> printScalarExpression = render . convExp

> printQueryExpression :: QueryExpression a -> String
> printQueryExpression = render . convSelectExpressionTL

> printQueryExpressions :: [QueryExpression a] -> String
> printQueryExpressions = render . hcat . map convSelectExpressionTL


-------------------------------------------------------------------------------

Conversion routines - convert Sql asts into Docs

-------------------------------------------------------------------------------

> convSelectExpressionTL :: QueryExpression a -> Doc
> convSelectExpressionTL s = convSelectExpression True True s <> statementEnd

> statementEnd :: Doc
> statementEnd = text ";"

Statement components

> -- selects
>
> convSelectExpression :: Bool -> Bool -> QueryExpression a -> Doc
> convSelectExpression writeSelect _ (Select _ dis l tb wh grp hav
>                                 order lim off) =
>   text (if writeSelect then "select" else "")
>   <+> (case dis of
>          Dupes -> empty
>          Distinct -> text "distinct")
>   <+> convSelList l
>   $+$ nest 2 (
>               (if null tb
>                  then empty
>                  else text "from" <+> hcatCsvMap convTref tb)
>               $+$ convWhere wh)
>   <+> ifNotEmpty (\g -> text "group by" <+> hcatCsvMap convExp g) grp
>   <+> maybeConv (\h -> text "having" <+> convExp h) hav
>   <+> ifNotEmpty (\o -> text "order by" <+> hcatCsvMap (\(oe,od) -> convExp oe
>                   <+> convDir od) o) order
>   <+> maybeConv (\lm -> text "limit" <+> convExp lm) lim
>   <+> maybeConv (\offs -> text "offset" <+> convExp offs) off
>
> convSelectExpression writeSelect topLev (CombineSelect _ tp s1 s2) =
>   let p = convSelectExpression writeSelect False s1
>           $+$ (case tp of
>                        Except -> text "except"
>                        Union -> text "union"
>                        UnionAll -> text "union" <+> text "all"
>                        Intersect -> text "intersect")
>           $+$ convSelectExpression True False s2
>   in if topLev then p else parens p
> convSelectExpression _ _ (Values _ expss) =
>   text "values" $$ nest 2 (vcat $ csv $ map (parens . csvExp) expss)
> convSelectExpression _ _ (WithSelect _ wqs ex) =
>   text "with" $$ nest 2 (vcat $ csv $ map pwq wqs)
>        $+$ convSelectExpression True False ex
>   where
>     pwq (WithQuery _ nm ex1) =
>       text nm <+> text "as"
>       <+> parens (convSelectExpression True False ex1)
>
> convTref :: TableRef a -> Doc
> convTref (Tref _ f a) = convExp f <+> convTrefAlias a
> convTref (JoinTref _ t1 nat jt t2 ex a) =
>         parens (convTref t1
>         $+$ (case nat of
>                       Natural -> text "natural"
>                       Unnatural -> empty)
>         <+> text (case jt of
>                           Inner -> "inner"
>                           Cross -> "cross"
>                           LeftOuter -> "left outer"
>                           RightOuter -> "right outer"
>                           FullOuter -> "full outer")
>         <+> text "join"
>         <+> convTref t2
>         <+> maybeConv (nest 2 . convJoinExpression) ex
>         <+> convTrefAlias a)
>         where
>           convJoinExpression (JoinOn _ e) = text "on" <+> convExp e
>           convJoinExpression (JoinUsing _ ids) =
>               text "using" <+> parens (hcatCsvMap text ids)
>
> convTref (SubTref _ sub alias) =
>         parens (convSelectExpression True True sub)
>         <+> text "as" <+> convTrefAlias alias
> convTref (FunTref _ f@(FunCall _ _ _) a) = convExp f <+> convTrefAlias a
> convTref (FunTref _ _x _) =
>       error $ "internal error: node not supported in function tref: "
>             -- ++ show x
>
> convTrefAlias :: TableAlias -> Doc
> convTrefAlias NoAlias = empty
> convTrefAlias (TableAlias t) = text t
> convTrefAlias (FullAlias t s) = text t <+> parens (hcatCsvMap text s)

> convDir :: Direction -> Doc
> convDir d = text $ case d of
>                           Asc -> "asc"
>                           Desc -> "desc"
>
> convWhere :: Maybe (ScalarExpression a) -> Doc
> convWhere (Just ex) = text "where" <+> convExp ex
> convWhere Nothing = empty
>
> convSelList :: (SelectList a) -> Doc
> convSelList (SelectList _ ex) =
>   hcatCsvMap convSelItem ex
>   where
>     convSelItem (SelectItem _ ex1 nm) = convExpSl ex1 <+> text "as" <+> text nm
>     convSelItem (SelExp _ e) = convExpSl e
>
>
> convTypeName :: (TypeName a) -> Doc
> convTypeName (SimpleTypeName _ s) = text s
> convTypeName (PrecTypeName _ s i) = text s <> parens(integer i)
> convTypeName (ArrayTypeName _ t) = convTypeName t <> text "[]"
> convTypeName (SetOfTypeName _ t) = text "setof" <+> convTypeName t
>
> -- expressions
>
> convExp :: (ScalarExpression a) -> Doc
> convExp (Identifier _ i) =
>   if quotesNeeded
>      then text $ "\"" ++ i ++ "\""
>      else text i
>   where
>     --needs some work - quotes needed if contains invalid unquoted
>     --chars, or maybe if matches keyword or similar
>     quotesNeeded = case i of
>                      x:_ | not (isLetter x || x `elem` "_*") -> True
>                      _ | all okChar i -> False
>                        | otherwise -> True
>                    where
>                      okChar x =isAlphaNum x || x `elem` "*_."
> convExp (QIdentifier a i1@(Identifier _ _) i) = convExp i1 <> text "." <> convExp (Identifier a i)
> convExp (QIdentifier a e i) = parens (convExp e) <> text "." <> convExp (Identifier a i)

> --convExp (PIdentifier _ i) = parens $ convExp i
> convExp (IntegerLit _ n) = integer n
> convExp (FloatLit _ n) = double n
> convExp (StringLit _ s) = -- needs some thought about using $$?
>                           text "'" <> text replaceQuotes <> text "'"
>                           where
>                             replaceQuotes = replace "'" "''" s {-if tag == "'"
>                                               then replace "'" "''" s
>                                               else s-}
>
> convExp (FunCall _ n es) =
>     --check for special operators
>    case n of
>      "!arrayctor" -> text "array" <> brackets (csvExp es)
>      "!between" -> convExp (head es) <+> text "between"
>                    <+> parens (convExp (es !! 1))
>                   <+> text "and"
>                   <+> parens (convExp (es !! 2))
>      "!substring" -> text "substring"
>                      <> parens (convExp (head es)
>                                 <+> text "from" <+> convExp (es !! 1)
>                                 <+> text "for" <+> convExp (es !! 2))
>      "!arraysub" -> case es of
>                        (Identifier _ i : es1) -> text i
>                                                  <> brackets (csvExp es1)
>                        _ -> parens (convExp (head es))
>                             <> brackets (csvExp (tail es))
>      "!rowctor" -> text "row" <> parens (hcatCsvMap convExp es)
>      "."   -- special case to avoid ws around '.'. Don't know if this is important
>            -- or just cosmetic
>          | [a,b] <- es -> convExp a <> text "." <> convExp b
>      _ | isOperatorName n ->
>         case forceRight (getOperatorType defaultTemplate1Catalog n) of
>                           BinaryOp ->
>                               parens (convExp (head es)
>                                       <+> text (filterKeyword n)
>                                       <+> convExp (es !! 1))
>                           PrefixOp -> parens (text (if n == "u-"
>                                                        then "-"
>                                                        else filterKeyword n)
>                                                <+> parens (convExp (head es)))
>                           PostfixOp -> parens (convExp (head es)
>                                        <+> text (filterKeyword n))
>        | otherwise -> text n <> parens (csvExp es)
>    where
>      filterKeyword t = case t of
>                          "!and" -> "and"
>                          "!or" -> "or"
>                          "!not" -> "not"
>                          "!isnull" -> "is null"
>                          "!isnotnull" -> "is not null"
>                          "!like" -> "like"
>                          x -> x
>
> convExp (BooleanLit _ b) = bool b
> convExp (InPredicate _ att t lst) =
>   convExp att <+> (if not t then text "not" else empty) <+> text "in"
>   <+> parens (case lst of
>                        InList _ expr -> csvExp expr
>                        InSelect _ sel -> convSelectExpression True True sel)
> convExp (LiftOperator _ op flav args) =
>   convExp (head args) <+> text op
>   <+> text (case flav of
>               LiftAny -> "any"
>               LiftAll -> "all")
>   <+> parens (convExp $ head $ tail args)
> convExp (ScalarSubQuery _ s) = parens (convSelectExpression True True s)
> convExp (NullLit _) = text "null"
> convExp (WindowFn _ fn part order asc frm) =
>   convExp fn <+> text "over"
>   <+> parens (if hp || ho
>               then (if hp
>                     then text "partition by" <+> csvExp part
>                     else empty)
>                     <+> (if ho
>                          then text "order by" <+> csvExp order
>                               <+> convDir asc
>                          else empty)
>                     <+> convFrm
>               else empty)
>   where
>     hp = not (null part)
>     ho = not (null order)
>     convFrm = case frm of
>                 FrameUnboundedPreceding -> text "range unbounded preceding"
>                 FrameUnboundedFull -> text "range between unbounded \
>                                            \preceding and unbounded following"
>                 FrameRowsUnboundedPreceding -> text "rows unbounded preceding"
>
> convExp (Case _ whens els) =
>   text "case"
>   $+$ nest 2 (vcat (map convWhen whens)
>               $+$ maybeConv (\e -> text "else" <+> convExp e) els)
>   $+$ text "end"
>       where
>         convWhen (ex1, ex2) =
>             text "when" <+> hcatCsvMap convExp ex1
>             <+> text "then" <+> convExp ex2
>
> convExp (CaseSimple _ val whens els) =
>   text "case" <+> convExp val
>   $+$ nest 2 (vcat (map convWhen whens)
>               $+$ maybeConv (\e -> text "else" <+> convExp e) els)
>   $+$ text "end"
>       where
>         convWhen (ex1, ex2) =
>             text "when" <+> hcatCsvMap convExp ex1
>             <+> text "then" <+> convExp ex2
>
> convExp (PositionalArg _ a) = text "$" <> integer a
> convExp (Placeholder _) = text "?"
> convExp (Exists _ s) =
>   text "exists" <+> parens (convSelectExpression True True s)
> convExp (Cast _ ex t) = text "cast" <> parens (convExp ex
>                                              <+> text "as"
>                                              <+> convTypeName t)

hack for selecting from composites in select list, the pg parser
needs p.x to look like (p).x when p is a composite in a select list
fortunately, we can output everything which is identifier . something
with the brackets and it works.

> convExpSl :: (ScalarExpression a) -> Doc
> convExpSl (FunCall _ "." es) | [a@(Identifier _ _), b] <- es =
>   parens (convExpSl a) <> text "." <> convExpSl b
> convExpSl x = convExp x

> --utils
>
> -- convert a list of expressions to horizontal csv
>
> csvExp :: [ScalarExpression a] -> Doc
> csvExp = hcatCsvMap convExp
>
> maybeConv :: (t -> Doc) -> Maybe t -> Doc
> maybeConv f c =
>     case c of
>       Nothing -> empty
>       Just a -> f a
>
> csv :: [Doc] -> [Doc]
> csv = punctuate comma
>
> hcatCsv :: [Doc] -> Doc
> hcatCsv = hcat . csv
>
> ifNotEmpty :: ([a] -> Doc) -> [a] -> Doc
> ifNotEmpty c l = if null l then empty else c l
>
> hcatCsvMap :: (a -> Doc) -> [a] -> Doc
> hcatCsvMap ex = hcatCsv . map ex
>
> bool :: Bool -> Doc
> bool b = if b then text "true" else text "false"
