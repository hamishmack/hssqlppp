
Convert qgen output into sql server format

> {-# LANGUAGE QuasiQuotes #-}
> import Data.Generics.Uniplate.Data

> import System.Environment
> import Data.Data

> import Database.HsSqlPpp.Parser
> import Database.HsSqlPpp.Ast
> import Database.HsSqlPpp.Pretty
> import Database.HsSqlPpp.Quote
> import Database.HsSqlPpp.Annotation

> main :: IO ()
> main = do
>   [fn] <- getArgs
>   f <- readFile fn
>   putStrLn $ fixSql f

> fixSql :: String -> String
> fixSql sql =
>   let qe = either (error . show) id $ parseStatements "" sql
>       qe' = fixSqlAst qe
>   in printStatements qe'

> fixSqlAst :: Data a => a -> a
> fixSqlAst = fixDate . fixSubstring . fixExtract . fixIntervals

 dateadd(day, -90, ‘1998-12-01’)
Instead of:
 date ‘1998-12-01’ - interval ‘90’ day

> fixIntervals :: Data a => a -> a
> fixIntervals = transformBi $ \x -> case x of
>   [sqlExpr| $(a) + $(b) |] | Just (i,v,d) <- dateInfo a b ->
>      [sqlExpr| dateAdd($i(i),$(v),$s(d))|]
>   [sqlExpr| $(a) - $(b) |]| Just (i,v,d) <- dateInfo a b ->
>      [sqlExpr| dateAdd($i(i),-$(v),$s(d))|]
>   x' -> x'
>   where
>     dateInfo (TypedStringLit _ (SimpleTypeName _ "date") d)
>              (Interval _ v i _)
>              | Just i' <- lookup i [(IntervalDay,"day")
>                                    ,(IntervalMonth,"month")
>                                    ,(IntervalYear,"year")]
>              = Just (i',NumberLit emptyAnnotation v,d)
>     dateInfo _ _ = Nothing

 datepart(year,l_shipdate)
Instead of:
 extract(year from l_shipdate)

> fixExtract :: Data a => a -> a
> fixExtract = transformBi $ \x -> case x of
>   [sqlExpr| extract(year from $(expr) ) |] ->
>       [sqlExpr| datepart(year,$(expr)) |]
>   x' -> x'


 substring(c_phone,1,2)
Instead of:
 substring(c_phone from 1 for 2)

> fixSubstring :: Data a => a -> a
> fixSubstring = transformBi $ \x -> case x of
>   [sqlExpr| substring($(i) from $(a) for $(b)) |] ->
>       [sqlExpr| substring($(i),$(a),$(b)) |]
>   x' -> x'

 ‘1998-12-01’
Instead of:
 date ‘1998-12-01’

> fixDate :: Data a => a -> a
> fixDate = transformBi $ \x -> case x of
>    TypedStringLit a (SimpleTypeName _ "date") d -> StringLit a d
>    x' -> x'