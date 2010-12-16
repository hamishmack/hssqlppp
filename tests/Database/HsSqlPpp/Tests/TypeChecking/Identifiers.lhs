checks adding qualifiers to identifers and replacing *

> module Database.HsSqlPpp.Tests.TypeChecking.Identifiers
>     (identifierTests
>     ) where

> import Test.HUnit
> import Test.Framework
> import Test.Framework.Providers.HUnit
> import Data.Generics

> import Database.HsSqlPpp.Annotation
> import Database.HsSqlPpp.AstInternals.AstInternal
> import Database.HsSqlPpp.Parser
> import Database.HsSqlPpp.SqlTypes
> import Database.HsSqlPpp.Catalog

> data TestItem = Group String [TestItem]
>               | TestItem [CatalogUpdate] String String

> sdbt :: [CatalogUpdate]
> sdbt = [CatCreateTable "t"
>                        [("a", typeInt)
>                        ,("b", typeInt)
>                        ,("c", typeInt)
>                        ,("d", typeInt)]
>                        []
>        ,CatCreateTable "u"
>                        [("e", typeInt)
>                        ,("f", typeInt)
>                        ,("g", typeInt)
>                        ,("h", typeInt)]
>                        []]

> identifierTestData :: TestItem
> identifierTestData =
>   Group "identifer tests" [rangeQualifierTests
>                           ,asterixTests]

> rangeQualifierTests :: TestItem
> rangeQualifierTests =
>   Group "range qualifier tests"
>         [simpleSelects
>         ,aggregates
>         ,joins
>         ,subqueries
>         ]

= simple selects

> simpleSelects :: TestItem
> simpleSelects =
>   Group "simple selects"
>         [ts "select a from t;" "select t.a from t;"
>         ,ts "select a from t as u;" "select u.a from t as u;"
>         ,ts "select a from t where b;" "select t.a from t where t.b;"
>         ,ts "select a from t\n\
>             \union\n\
>             \select e from u;"
>             "select t.a from t\n\
>             \union\n\
>             \select u.e from u;"
>         ,ts "select a from t order by c,d asc;"
>             "select t.a from t order by t.c,t.d asc;"
>         ]
>   where
>     ts = TestItem sdbt

= aggregates

> aggregates :: TestItem
> aggregates = Group "aggregates"
>         [ts "select b, count(a), avg(a) from t group by b having sum(c) > 5;"
>             "select t.b, count(t.a), avg(t.a) from t group by t.b having sum(t.c) > 5;"
>         ,ts "select row_number()\n\
>             \       over(partition by (a,b) order by c) as place\n\
>             \  from t;"
>             "select row_number()\n\
>             \       over(partition by (t.a,t.b) order by t.c) as place\n\
>             \  from t;"
>         ]
>   where
>     ts = TestItem sdbt

= joins

> joins :: TestItem
> joins = Group "joins"
>         [ts "select a,b from t,u;"
>             "select t.a,t.b from t,u;"
>         ,ts "select a,e from t,u where b=f;"
>             "select t.a,u.e from t,u where t.b=u.f;"
>         ,ts "select a,e from t inner join u on a = e;"
>             "select t.a,u.e from t inner join u on t.a = u.e;"
>         ,ts "select a from t inner join u using(c);"
>             "select t.a from t inner join u using(c);"
>         ]
>   where
>     ts = TestItem sdbt

= subqueries

> subqueries :: TestItem
> subqueries = Group "subqueries"
>         [
>         ]
>   where
>     ts = TestItem sdbt

= *

> asterixTests :: TestItem
> asterixTests =
>   Group "asterix tests"
>         [ts "select * from t;" "select t.a,t.b,t.c,t.d from t;"
>         ,ts "select t.* from t,u;" "select t.a,t.b,t.c,t.d from t,u;"
>         ,ts "select * from t,u;" "select t.a,t.b,t.c,t.d, u.e,u.f,u.g,u.h from t,u;"
>         ,ts "select count(*) from t;" "select count(*) from t;"
>         ]
>   where
>     ts = TestItem sdbt

--------------------------------------------

> identifierTests :: Test.Framework.Test
> identifierTests = makeTest identifierTestData

> makeTest :: TestItem -> Test.Framework.Test
> makeTest (Group n ts) = testGroup n $ map makeTest ts

> makeTest (TestItem dbt sql csql) = testCase sql $ do
>   let cat = either (error . show) id $ updateCatalog defaultTemplate1Catalog dbt
>       p = either (error . show) id . fmap resetSourcePositions . parseQueryExpression ""
>       [tcsqla] = canonicaliseIdentifiers cat [p sql]
>       csqla = p csql
>   assertEqual "" csqla tcsqla
