
Tests using the tpch queries. Just tests the result type at the
moment.

> module Database.HsSqlPpp.Tests.TypeChecking.TpchTests
>     (tpchTests) where
>
> import Test.HUnit
> import Test.Framework
> import Test.Framework.Providers.HUnit
> --import Data.List
> import Data.Generics.Uniplate.Data
> import Control.Monad
> --import Text.Groom
>
> --import Database.HsSqlPpp.Utils.Here
> import Database.HsSqlPpp.Parser
> import Database.HsSqlPpp.TypeChecker
> import Database.HsSqlPpp.Annotation
> import Database.HsSqlPpp.Catalog
> import Database.HsSqlPpp.Types
> --import Database.HsSqlPpp.Utils.PPExpr
> --import Database.HsSqlPpp.Tests.TestUtils
> import Database.HsSqlPpp.Pretty
> import Database.HsSqlPpp.Tests.TpchData
>
> data Item = Group String [Item]
>           | Query String String Type
>
> tpchTests :: Test.Framework.Test
> tpchTests = itemToTft tpchTestData
>
> tpchTestData :: Item
> tpchTestData =
>   Group "tpch" $
>         -- FIXME: 15 has with expression which doesn't quite work atm
>         -- (todo with fixup identifiers I think
>         let (s,e) = splitAt 14 t
>         in s ++ drop 1 e
>   where
>     t = zipWith (\(n,s) t' ->  Query n s (SetOfType $ CompositeType t'))
>          tpchQueries
>          [-- q1
>           [("l_returnflag", typeChar)
>           ,("l_linestatus", typeChar)
>           ,("sum_qty", typeNumeric)
>           ,("sum_base_price", typeNumeric)
>           ,("sum_disc_price", typeNumeric)
>           ,("sum_charge", typeNumeric)
>           ,("avg_qty", typeNumeric)
>           ,("avg_price", typeNumeric)
>           ,("avg_disc", typeNumeric)
>           ,("count_order", typeBigInt)]
>          ,--q2
>           [("s_acctbal", typeNumeric)
>           ,("s_name", typeChar)
>           ,("n_name", typeChar)
>           ,("p_partkey", typeInt)
>           ,("p_mfgr", typeChar)
>           ,("s_address", typeVarChar)
>           ,("s_phone", typeChar)
>           ,("s_comment", typeVarChar)]
>          ,--q3
>           [("l_orderkey", typeInt)
>           ,("revenue", typeNumeric)
>           ,("o_orderdate", typeDate)
>           ,("o_shippriority", typeInt)]
>          ,--q4
>           [("o_orderpriority", typeChar)
>           ,("order_count", typeBigInt)]
>          ,--q5
>           [("n_name", typeChar)
>           ,("revenue", typeNumeric)]
>          ,--q6
>           [("revenue", typeNumeric)]
>          ,--q7
>           [("supp_nation", typeChar)
>           ,("cust_nation", typeChar)
>           ,("l_year", typeFloat8)
>           ,("revenue", typeNumeric)]
>          ,--q8
>           [("o_year", typeFloat8)
>           ,("mkt_share", typeNumeric)]
>          ,--q9
>           [("nation", typeChar)
>           ,("o_year", typeFloat8)
>           ,("sum_profit", typeNumeric)]
>          ,--q10
>           [("c_custkey",typeInt)
>           ,("c_name", typeChar)
>           ,("revenue", typeNumeric)
>           ,("c_acctbal", typeNumeric)
>           ,("n_name", typeChar)
>           ,("c_address", typeVarChar)
>           ,("c_phone", typeChar)
>           ,("c_comment", typeVarChar)]
>          ,--q11
>           [("ps_partkey", typeInt)
>           ,("value", typeNumeric)]
>          ,--q12
>           [("l_shipmode", typeChar)
>           ,("high_line_count", typeBigInt)
>           ,("low_line_count", typeBigInt)]
>          ,--q13
>           [("c_count", typeBigInt)
>           ,("custdist", typeBigInt)]
>          ,--q14
>           [("promo_revenue", typeNumeric)]
>          ,--q15
>           [("s_suppkey", typeInt)
>           ,("s_name", typeChar)
>           ,("s_address", typeVarChar)
>           ,("s_phone", typeChar)
>           ,("total_revenue", typeNumeric)]
>          ,--q16
>           [("p_brand", typeChar)
>           ,("p_type", typeVarChar)
>           ,("p_size", typeInt)
>           ,("supplier_cnt", typeBigInt)]
>          ,--q17
>           [("avg_yearly", typeNumeric)]
>          ,--q18
>           [("c_name", typeChar)
>           ,("c_custkey", typeInt)
>           ,("o_orderkey", typeInt)
>           ,("o_orderdate", typeDate)
>           ,("o_totalprice", typeNumeric)
>           ,("sum", typeNumeric)]
>          ,--q19
>           [("revenue", typeNumeric)]
>          ,--q20
>           [("s_name", typeChar)
>           ,("s_address", typeVarChar)]
>          ,--q21
>           [("s_name", typeChar)
>           ,("numwait", typeBigInt)]
>          ,--q22
>           [("cntrycode", ScalarType "char")
>           ,("numcust", typeBigInt)
>           ,("totacctbal", typeNumeric)]
>          ]

> itemToTft :: Item -> Test.Framework.Test
> itemToTft (Group s is) = testGroup s $ map itemToTft is
> itemToTft (Query n sql t) = testCase ("typecheck tpch " ++ n ++ ":") $ do
>   let ast = case parseQueryExpr "" sql of
>               Left e -> error $ show e
>               Right l -> l
>       aast = typeCheckQueryExpr cat ast
>       ty = atype $ getAnnotation aast
>       er :: [TypeError]
>       er = universeBi aast
>       res = if null er
>             then case ty of
>                    Nothing -> Left []
>                    Just ty' -> Right ty'
>             else Left er
>       ok = Right t == res
>   unless ok $ putStrLn $ "bad sql: " ++ printQueryExpr aast
>        -- ++ "\n" ++ groom aast
>   assertBool "" ok
>   where
>     cat = case updateCatalog defaultTemplate1Catalog tpchCatalog of
>                         Left x -> error $ show x
>                         Right e -> e



