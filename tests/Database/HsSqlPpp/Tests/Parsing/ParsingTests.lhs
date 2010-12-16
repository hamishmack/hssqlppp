
The automated tests, uses hunit to check a bunch of text expressions
and sql statements parse to the correct tree, and then checks pretty
printing and then reparsing gives the same tree. The code was mostly
written almost in tdd style, which the order/ coverage of these tests
reflects.

There are no tests for invalid syntax at the moment.

> {-# LANGUAGE QuasiQuotes #-}
>
> module Database.HsSqlPpp.Tests.Parsing.ParsingTests (parsingTests) where
>
> import Test.HUnit
> import Test.Framework
> import Test.Framework.Providers.HUnit
> import Data.Generics
>
> import Database.HsSqlPpp.Tests.Here
>
> import Database.HsSqlPpp.Ast
> import Database.HsSqlPpp.Annotation
> import Database.HsSqlPpp.Parser
> import Database.HsSqlPpp.PrettyPrinter
>
> data Item = Expr String (ScalarExpression SourcePosition)
>           | Stmt String [QueryExpression SourcePosition]
>           | Group String [Item]

> parsingTests :: Test.Framework.Test
> parsingTests = itemToTft parsingTestData
>
> parsingTestData :: Item
> parsingTestData =
>   Group "parsingTests" [

--------------------------------------------------------------------------------

>    Group "parse expressions" [
>     Group "basic expressions" [
>       e "1" (IntegerLit ea 1)
>      ,e "-1" (FunCall ea "u-" [IntegerLit ea 1])
>      ,e "1.1" (FloatLit ea 1.1)
>      ,e "-1.1" (FunCall ea "u-" [FloatLit ea 1.1])
>      ,e " 1 + 1 " (FunCall ea "+" [IntegerLit ea 1
>                                   ,IntegerLit ea 1])
>      ,e "1+1+1" (FunCall ea "+" [FunCall ea "+" [IntegerLit ea 1
>                                                 ,IntegerLit ea 1]
>                                 ,IntegerLit ea 1])
>      ]
>    ,Group "parens" [

check some basic parens use wrt naked values and row constructors
these tests reflect how pg seems to interpret the variants.

>       e "(1)" (IntegerLit ea 1)
>      ,e "row ()" (FunCall ea "!rowctor" [])
>      ,e "row (1)" (FunCall ea "!rowctor" [IntegerLit ea 1])
>      ,e "row (1,2)" (FunCall ea "!rowctor" [IntegerLit ea 1,IntegerLit ea 2])
>      ,e "(1,2)" (FunCall ea "!rowctor" [IntegerLit ea 1,IntegerLit ea 2])
>      ]
>    ,Group "more basic expressions" [

test some more really basic expressions

>       e "'test'" (stringQ "test")
>      ,e "''" (stringQ "")
>      ,e "hello" (Identifier ea "hello")
>      ,e "helloTest" (Identifier ea "helloTest")
>      ,e "hello_test" (Identifier ea "hello_test")
>      ,e "\"this is an identifier\"" (Identifier ea "this is an identifier")
>      ,e "hello1234" (Identifier ea "hello1234")
>      ,e "true" (BooleanLit ea True)
>      ,e "false" (BooleanLit ea False)
>      ,e "null" (NullLit ea)
>      ]
>    ,Group "array ctor and selector" [
>       e "array[1,2]" (FunCall ea "!arrayctor" [IntegerLit ea 1, IntegerLit ea 2])
>      ,e "a[1]" (FunCall ea "!arraysub" [Identifier ea "a", IntegerLit ea 1])
>      ]
>    ,Group "simple operators" [
>       e "1 + tst1" (FunCall ea "+" [IntegerLit ea 1
>                                ,Identifier ea "tst1"])
>      ,e "tst1 + 1" (FunCall ea "+" [Identifier ea "tst1"
>                                ,IntegerLit ea 1])
>      ,e "tst + tst1" (FunCall ea "+" [Identifier ea "tst"
>                                  ,Identifier ea "tst1"])
>      ,e "'a' || 'b'" (FunCall ea "||" [stringQ "a"
>                                   ,stringQ "b"])
>      ,e "'stuff'::text" (Cast ea (stringQ "stuff") (SimpleTypeName ea "text"))
>      ,e "245::float(24)" (Cast ea (IntegerLit ea 245) (PrecTypeName ea "float" 24))
>      ,e "245::double precision" (Cast ea (IntegerLit ea 245) (SimpleTypeName ea "double precision"))
>      ,e "a between 1 and 3"
>         (FunCall ea "!between" [Identifier ea "a", IntegerLit ea 1, IntegerLit ea 3])
>      ,e "cast(a as text)"
>         (Cast ea (Identifier ea "a") (SimpleTypeName ea "text"))
>      ,e "@ a"
>         (FunCall ea "@" [Identifier ea "a"])
>      ,e "substring(a from 0 for 3)"
>         (FunCall ea "!substring" [Identifier ea "a", IntegerLit ea 0, IntegerLit ea 3])
>      ,e "substring(a from 0 for (5 - 3))"
>         (FunCall ea "!substring" [Identifier ea "a",IntegerLit ea 0,
>          FunCall ea "-" [IntegerLit ea 5,IntegerLit ea 3]])
>      ,e "a like b"
>         (FunCall ea "!like" [Identifier ea "a", Identifier ea "b"])
>      ]
>    ,Group "function calls" [
>       e "fn()" (FunCall ea "fn" [])
>      ,e "fn(1)" (FunCall ea "fn" [IntegerLit ea 1])
>      ,e "fn('test')" (FunCall ea "fn" [stringQ "test"])
>      ,e "fn(1,'test')" (FunCall ea "fn" [IntegerLit ea 1, stringQ "test"])
>      ,e "fn('test')" (FunCall ea "fn" [stringQ "test"])
>      ]
>    ,Group "simple whitespace sanity checks" [
>       e "fn (1)" (FunCall ea "fn" [IntegerLit ea 1])
>      ,e "fn( 1)" (FunCall ea "fn" [IntegerLit ea 1])
>      ,e "fn(1 )" (FunCall ea "fn" [IntegerLit ea 1])
>      ,e "fn(1) " (FunCall ea "fn" [IntegerLit ea 1])
>      ]
>    ,Group "null stuff" [
>       e "not null" (FunCall ea "!not" [NullLit ea])
>      ,e "a is null" (FunCall ea "!isnull" [Identifier ea "a"])
>      ,e "a is not null" (FunCall ea "!isnotnull" [Identifier ea "a"])
>      ,e "not not true" (FunCall ea "!not"
>                          [FunCall ea "!not"
>                           [BooleanLit ea True]])
>      ]

>    ,Group "case expressions" [
>       e {-"case when a,b then 3\n\
>         \     when c then 4\n\
>         \     else 5\n\
>         \end" -}
>         [$here|
>          case when a,b then 3
>               when c then 4
>               else 5
>          end
>          |]
>         (Case ea [([Identifier ea "a", Identifier ea "b"], IntegerLit ea 3)
>               ,([Identifier ea "c"], IntegerLit ea 4)]
>          (Just $ IntegerLit ea 5))
>      ,e  "case 1 when 2 then 3 else 4 end"
>         (CaseSimple ea (IntegerLit ea 1)
>            [([IntegerLit ea 2], IntegerLit ea 3)]
>          (Just $ IntegerLit ea 4))
>      ]
>    ,Group "positional args" [
>       e "$1" (PositionalArg ea 1)
>      ,e "?" (Placeholder ea)
>      ,e "a = ?" (FunCall ea "=" [Identifier ea "a",Placeholder ea])
>      ]
>    ,Group "exists" [
>       e "exists (select 1 from a)"
>       (Exists ea (selectFrom [SelExp ea (IntegerLit ea 1)] (Tref ea (i "a") NoAlias)))
>      ]
>    ,Group "in variants" [
>       e "t in (1,2)"
>       (InPredicate ea (Identifier ea "t") True (InList ea [IntegerLit ea 1,IntegerLit ea 2]))
>      ,e "t not in (1,2)"
>       (InPredicate ea (Identifier ea "t") False (InList ea [IntegerLit ea 1,IntegerLit ea 2]))
>      ,e "(t,u) in (1,2)"
>       (InPredicate ea (FunCall ea "!rowctor" [Identifier ea "t",Identifier ea "u"]) True
>        (InList ea [IntegerLit ea 1,IntegerLit ea 2]))
>      ,e "3 = any (array[1,2])"
>       (LiftOperator ea "=" LiftAny [IntegerLit ea 3
>                                     ,FunCall ea "!arrayctor" [IntegerLit ea 1
>                                                              ,IntegerLit ea 2]])
>      ,e "3 = all (array[1,2,4])"
>       (LiftOperator ea "=" LiftAll [IntegerLit ea 3
>                                     ,FunCall ea "!arrayctor" [IntegerLit ea 1
>                                                              ,IntegerLit ea 2
>                                                              ,IntegerLit ea 4]])
>      ]
>    ,Group "comparison operators" [
>       e "a < b"
>       (FunCall ea "<" [Identifier ea "a", Identifier ea "b"])
>      ,e "a <> b"
>       (FunCall ea "<>" [Identifier ea "a", Identifier ea "b"])
>      ,e "a != b"
>       (FunCall ea "<>" [Identifier ea "a", Identifier ea "b"])
>      ]

test some string parsing, want to check single quote behaviour,
and dollar quoting, including nesting.

>    ,Group "string parsing" [
>       e "''" (stringQ "")
>      ,e "''''" (stringQ "'")
>      ,e "'test'''" (stringQ "test'")
>      ,e "'''test'" (stringQ "'test")
>      ,e "'te''st'" (stringQ "te'st")
>      ,e "$$test$$" (StringLit ea "test")
>      ,e "$$te'st$$" (StringLit ea "te'st")
>      ,e "$st$test$st$" (StringLit ea "test")
>      ,e "$outer$te$$yup$$st$outer$" (StringLit ea "te$$yup$$st")
>      ,e "'spl$$it'" (stringQ "spl$$it")
>      ]
>    ,Group "bracketed things" [
>       e "(p).x" (qi "p" "x")
>      ,e "(select f(((a).x, y)::z))"
>         (ScalarSubQuery ea
>          (selectE (sl
>                    [SelExp ea
>                     (FunCall ea "f" [Cast ea
>                                      (FunCall ea "!rowctor"
>                                       [qi "a" "x"
>                                       ,Identifier ea "y"])
>                                      (SimpleTypeName ea "z")])])))
>      ]
>      ]

--------------------------------------------------------------------------------

select statements

>   ,Group "simple select statements" [
>     Group "select no table" [
>       s "select 1;" [selectE (SelectList ea [SelExp ea (IntegerLit ea 1)])]
>      ]
>    ,Group "select from table" [
>       s "select * from tbl;"
>       [selectFrom (selIL ["*"]) (Tref ea (i "tbl") NoAlias)]
>      ,s "select a,b from tbl;"
>       [selectFrom (selIL ["a", "b"]) (Tref ea (i "tbl") NoAlias)]
>      ,s "select a,b from inf.tbl;"
>       [selectFrom (selIL ["a", "b"]) (Tref ea (qi "inf" "tbl") NoAlias)]
>      ,s "select distinct * from tbl;"
>       [Select ea Distinct (SelectList ea (selIL ["*"])) [Tref ea (i "tbl") NoAlias]
>        Nothing [] Nothing [] Nothing Nothing]
>      ,s "select a from tbl where b=2;"
>       [selectFromWhere
>         (selIL ["a"])
>         (Tref ea (i "tbl") NoAlias)
>         (FunCall ea "="
>          [Identifier ea "b", IntegerLit ea 2])]
>      ,s "select a from tbl where b=2 and c=3;"
>       [selectFromWhere
>         (selIL ["a"])
>         (Tref ea (i "tbl") NoAlias)
>         (FunCall ea "!and"
>          [FunCall ea "="  [Identifier ea "b", IntegerLit ea 2]
>          ,FunCall ea "=" [Identifier ea "c", IntegerLit ea 3]])]
>      ]
>

>    ,Group "more select statements" [
>       s "select a from tbl\n\
>         \except\n\
>         \select a from tbl1;"
>       [CombineSelect ea Except
>        (selectFrom (selIL ["a"]) (Tref ea (i "tbl") NoAlias))
>        (selectFrom (selIL ["a"]) (Tref ea (i "tbl1") NoAlias))]
>      ,s "select a from tbl where true\n\
>         \except\n\
>         \select a from tbl1 where true;"
>       [CombineSelect ea Except
>        (selectFromWhere (selIL ["a"]) (Tref ea (i "tbl") NoAlias) (BooleanLit ea True))
>        (selectFromWhere (selIL ["a"]) (Tref ea (i "tbl1") NoAlias) (BooleanLit ea True))]
>      ,s "select a from tbl\n\
>         \union\n\
>         \select a from tbl1;"
>       [CombineSelect ea Union
>        (selectFrom (selIL ["a"]) (Tref ea (i "tbl") NoAlias))
>        (selectFrom (selIL ["a"]) (Tref ea (i "tbl1") NoAlias))]
>      ,s "select a from tbl\n\
>         \union all\n\
>         \select a from tbl1;"
>       [CombineSelect ea UnionAll
>        (selectFrom (selIL ["a"]) (Tref ea (i "tbl") NoAlias))
>        (selectFrom (selIL ["a"]) (Tref ea (i "tbl1") NoAlias))]
>      ,s "(select 1 union select 2) union select 3;"
>       [
>        (CombineSelect ea Union
>         (CombineSelect ea Union
>          (selectE (SelectList ea [SelExp ea (IntegerLit ea 1)]))
>          (selectE (SelectList ea [SelExp ea (IntegerLit ea 2)])))
>         (selectE (SelectList ea [SelExp ea (IntegerLit ea 3)])))]
>      ,s "select 1 union (select 2 union select 3);"
>       [
>        (CombineSelect ea Union
>         (selectE (SelectList ea [SelExp ea (IntegerLit ea 1)]))
>         (CombineSelect ea Union
>          (selectE (SelectList ea [SelExp ea (IntegerLit ea 2)]))
>          (selectE (SelectList ea [SelExp ea (IntegerLit ea 3)]))))]
>      ,s [$here|
>          with a as (select 1 as a1),
>               b as (select * from a)
>               select * from b; |]
>          [
>           (WithSelect ea
>            [WithQuery ea "a" (selectE $ SelectList ea
>                                [SelectItem ea (IntegerLit ea 1) "a1"])
>            ,WithQuery ea "b" (selectFrom (selIL ["*"]) (Tref ea (i "a") NoAlias))]
>            (selectFrom (selIL ["*"]) (Tref ea (i "b") NoAlias)))]
>      ,s [$here|
>          with a as (select 1 as a1),
>               b as (select * from a)
>               select * from a
>               union select * from b; |]
>          [
>           (WithSelect ea
>            [WithQuery ea "a" (selectE $ SelectList ea
>                                [SelectItem ea (IntegerLit ea 1) "a1"])
>            ,WithQuery ea "b" (selectFrom (selIL ["*"]) (Tref ea (i "a") NoAlias))]
>            (CombineSelect ea Union
>              (selectFrom (selIL ["*"]) (Tref ea (i "a") NoAlias))
>              (selectFrom (selIL ["*"]) (Tref ea (i "b") NoAlias))))]
>      ,s "select a as b from tbl;"
>       [selectFrom [SelectItem ea (Identifier ea "a") "b"] (Tref ea (i "tbl") NoAlias)]
>      ,s "select a + b as b from tbl;"
>       [selectFrom
>        [SelectItem ea
>         (FunCall ea "+"
>          [Identifier ea "a", Identifier ea "b"]) "b"]
>        (Tref ea (i "tbl") NoAlias)]
>      ,s "select a.* from tbl a;"
>       [selectFrom (selEL [qi "a" "*"]) (Tref ea (i "tbl") (TableAlias "a"))]
>      ,s "select a.* from tbl a(b,c);"
>       [selectFrom (selEL [qi "a" "*"]) (Tref ea (i "tbl") (FullAlias "a" ["b","c"]))]

>      ,s "select * from t1 a, t2 b;"
>             [
>              (Select ea Dupes
>               (SelectList ea
>                [SelExp ea (Identifier ea "*")])
>               [Tref ea (i "t1") (TableAlias "a"),Tref ea (i "t2") (TableAlias "b")]
>               Nothing [] Nothing [] Nothing Nothing)]
>      ,s "select a from b inner join c on b.a=c.a;"
>       [selectFrom
>        (selIL ["a"])
>        (JoinTref ea (Tref ea (i "b") NoAlias) Unnatural Inner (Tref ea (i "c") NoAlias)
>           (Just (JoinOn ea
>            (FunCall ea "=" [qi "b" "a", qi "c" "a"]))) NoAlias)]
>      ,s "select a from b inner join c as d on b.a=d.a;"
>       [selectFrom
>        (selIL ["a"])
>        (JoinTref ea (Tref ea (i "b") NoAlias) Unnatural Inner (Tref ea (i "c") (TableAlias "d"))
>           (Just (JoinOn ea
>            (FunCall ea "=" [qi "b" "a", qi "d" "a"]))) NoAlias)]
>      ,s "select a from b inner join c using(d,e);"
>       [selectFrom
>        (selIL ["a"])
>        (JoinTref ea (Tref ea (i "b") NoAlias) Unnatural Inner (Tref ea (i "c") NoAlias)
>           (Just (JoinUsing ea ["d","e"])) NoAlias)]
>      ,s "select a from b natural inner join c;"
>       [selectFrom
>        (selIL ["a"])
>        (JoinTref ea (Tref ea (i "b") NoAlias) Natural Inner (Tref ea (i "c") NoAlias) Nothing NoAlias)]
>      ,s "select a from b left outer join c;"
>       [selectFrom
>        (selIL ["a"])
>        (JoinTref ea (Tref ea (i "b") NoAlias) Unnatural LeftOuter (Tref ea (i "c") NoAlias) Nothing NoAlias)]
>      ,s "select a from b full outer join c;"
>       [selectFrom
>        (selIL ["a"])
>        (JoinTref ea (Tref ea (i "b") NoAlias) Unnatural FullOuter (Tref ea (i "c") NoAlias) Nothing NoAlias)]
>      ,s "select a from b right outer join c;"
>       [selectFrom
>        (selIL ["a"])
>        (JoinTref ea (Tref ea (i "b") NoAlias) Unnatural RightOuter (Tref ea (i "c") NoAlias) Nothing NoAlias)]
>      ,s "select a from b cross join c;"
>       [selectFrom
>        (selIL ["a"])
>        (JoinTref ea (Tref ea (i "b") NoAlias) Unnatural Cross (Tref ea (i "c") NoAlias) Nothing NoAlias)]
>      ,s "select a from (b natural join c);"
>       [selectFrom
>        (selIL ["a"])
>        (JoinTref ea (Tref ea (i "b") NoAlias) Natural Inner (Tref ea (i "c") NoAlias) Nothing NoAlias)]
>      ,s "select x from a cross join b cross join c;"
>        [
>         (selectFrom (selIL ["x"])
>          (JoinTref ea
>          (JoinTref ea
>           (Tref ea (i "a") NoAlias)
>            Unnatural Cross
>           (Tref ea (i "b") NoAlias)
>           Nothing NoAlias)
>          Unnatural Cross
>          (Tref ea (i "c") NoAlias)
>          Nothing NoAlias))]
>      ,s "select x from ((a cross join b) cross join c);"
>        [
>         (selectFrom (selIL ["x"])
>          (JoinTref ea
>          (JoinTref ea
>           (Tref ea (i "a") NoAlias)
>            Unnatural Cross
>           (Tref ea (i "b") NoAlias)
>           Nothing NoAlias)
>          Unnatural Cross
>          (Tref ea (i "c") NoAlias)
>          Nothing NoAlias))]
>      ,s "select x from (a cross join (b cross join c));"
>        [
>         (selectFrom (selIL ["x"])
>          (JoinTref ea
>           (Tref ea (i "a") NoAlias)
>           Unnatural Cross
>           (JoinTref ea
>            (Tref ea (i "b") NoAlias)
>            Unnatural Cross
>            (Tref ea (i "c") NoAlias)
>            Nothing NoAlias)
>           Nothing NoAlias))]

>      ,s "select x from ((a cross join b) cross join c);"
>        [
>         (selectFrom (selIL ["x"])
>          (JoinTref ea
>          (JoinTref ea
>           (Tref ea (i "a") NoAlias)
>            Unnatural Cross
>           (Tref ea (i "b") NoAlias)
>           Nothing NoAlias)
>          Unnatural Cross
>          (Tref ea (i "c") NoAlias)
>          Nothing NoAlias))]
>      ,s "select x from (a cross join b) cross join c;"
>        [
>         (selectFrom (selIL ["x"])
>          (JoinTref ea
>          (JoinTref ea
>           (Tref ea (i "a") NoAlias)
>            Unnatural Cross
>           (Tref ea (i "b") NoAlias)
>           Nothing NoAlias)
>          Unnatural Cross
>          (Tref ea (i "c") NoAlias)
>          Nothing NoAlias))]
>      ,s "select x from ((a cross join b) cross join c) cross join d;"
>        [
>         (selectFrom (selIL ["x"])
>          (JoinTref ea
>           (JoinTref ea
>            (JoinTref ea
>             (Tref ea (i "a") NoAlias)
>             Unnatural Cross
>             (Tref ea (i "b") NoAlias)
>             Nothing NoAlias)
>            Unnatural Cross
>            (Tref ea (i "c") NoAlias)
>            Nothing NoAlias)
>           Unnatural Cross
>           (Tref ea (i "d") NoAlias)
>           Nothing NoAlias))]
>      ,s "select a from b\n\
>         \    inner join c\n\
>         \      on true\n\
>         \    inner join d\n\
>         \      on 1=1;"
>       [selectFrom
>        [SelExp ea (Identifier ea "a")]
>        (JoinTref ea
>         (JoinTref ea (Tref ea (i "b") NoAlias) Unnatural Inner (Tref ea (i "c") NoAlias)
>          (Just $ JoinOn ea (BooleanLit ea True)) NoAlias)
>         Unnatural Inner (Tref ea (i "d") NoAlias)
>         (Just $ JoinOn ea (FunCall ea "="
>                [IntegerLit ea 1, IntegerLit ea 1])) NoAlias)]

>      ,s "select row_number() over(order by a) as place from tbl;"
>       [selectFrom [SelectItem ea
>                    (WindowFn ea
>                     (FunCall ea "row_number" [])
>                     []
>                     [Identifier ea "a"] Asc FrameUnboundedPreceding)
>                    "place"]
>        (Tref ea (i "tbl") NoAlias)]
>      ,s "select row_number() over(order by a asc) as place from tbl;"
>       [selectFrom [SelectItem ea
>                    (WindowFn ea
>                     (FunCall ea "row_number" [])
>                     []
>                     [Identifier ea "a"] Asc FrameUnboundedPreceding)
>                    "place"]
>        (Tref ea (i "tbl") NoAlias)]
>      ,s "select row_number() over(order by a desc) as place from tbl;"
>       [selectFrom [SelectItem ea
>                    (WindowFn ea
>                     (FunCall ea "row_number" [])
>                     []
>                     [Identifier ea "a"] Desc FrameUnboundedPreceding)
>                    "place"]
>        (Tref ea (i "tbl") NoAlias)]
>      ,s "select row_number()\n\
>         \over(partition by (a,b) order by c) as place\n\
>         \from tbl;"
>       [selectFrom [SelectItem ea
>                    (WindowFn ea
>                     (FunCall ea "row_number" [])
>                     [FunCall ea "!rowctor" [Identifier ea "a",Identifier ea "b"]]
>                     [Identifier ea "c"] Asc FrameUnboundedPreceding)
>                    "place"]
>        (Tref ea (i "tbl") NoAlias)]
>      ,s "select * from a natural inner join (select * from b) as a;"
>       [selectFrom
>        (selIL ["*"])
>        (JoinTref ea (Tref ea (i "a") NoAlias) Natural
>         Inner (SubTref ea (selectFrom
>                         (selIL ["*"])
>                         (Tref ea (i "b") NoAlias)) (TableAlias "a"))
>         Nothing NoAlias)]
>      ,s "select * from a order by c;"
>       [Select ea  Dupes
>        (sl (selIL ["*"]))
>        [Tref ea (i "a") NoAlias]
>        Nothing [] Nothing [(Identifier ea "c",Asc)] Nothing Nothing]
>      ,s "select *\n\
>            \from Adventure\n\
>            \order by Clicks desc, AdventureID;"
>       [Select ea Dupes
>        (sl (selIL ["*"]))
>        [Tref ea (i "Adventure") NoAlias]
>        Nothing [] Nothing [(Identifier ea "Clicks",Desc)
>                           ,(Identifier ea "AdventureID",Asc)] Nothing Nothing]
>      ,s "select * from a order by c,d asc;"
>       [Select ea Dupes
>        (sl (selIL ["*"]))
>        [Tref ea (i "a") NoAlias]
>        Nothing [] Nothing [(Identifier ea "c", Asc)
>                           ,(Identifier ea "d", Asc)] Nothing Nothing]
>      ,s "select * from a order by c,d desc;"
>       [Select ea Dupes
>        (sl (selIL ["*"]))
>        [Tref ea (i "a") NoAlias]
>        Nothing [] Nothing [(Identifier ea "c", Asc)
>                           ,(Identifier ea "d", Desc)] Nothing Nothing]
>      ,s "select * from a order by c limit 1;"
>       [Select ea Dupes
>        (sl (selIL ["*"]))
>        [Tref ea (i "a") NoAlias]
>        Nothing [] Nothing [(Identifier ea "c",Asc)] (Just (IntegerLit ea 1)) Nothing]
>      ,s "select * from a order by c offset 3;"
>       [Select ea Dupes
>        (sl (selIL ["*"]))
>        [Tref ea (i "a") NoAlias]
>        Nothing [] Nothing [(Identifier ea "c",Asc)] Nothing (Just $ IntegerLit ea 3)]
>      ,s "select a from (select b from c) as d;"
>         [selectFrom
>          (selIL ["a"])
>          (SubTref ea (selectFrom
>                    (selIL ["b"])
>                    (Tref ea (i "c") NoAlias))
>           (TableAlias "d"))]
>      ,s "select * from gen();"
>         [selectFrom (selIL ["*"]) (FunTref ea (FunCall ea "gen" []) NoAlias)]
>      ,s "select * from gen() as t;"
>       [selectFrom
>        (selIL ["*"])
>        (FunTref ea (FunCall ea "gen" [])(TableAlias  "t"))]
>      ,s "select a, count(b) from c group by a;"
>         [Select ea Dupes
>          (sl [selI "a", SelExp ea (FunCall ea "count" [Identifier ea "b"])])
>          [Tref ea (i "c") NoAlias] Nothing [Identifier ea "a"]
>          Nothing [] Nothing Nothing]
>      ,s "select a, count(b) as cnt from c group by a having cnt > 4;"
>         [Select ea Dupes
>          (sl [selI "a", SelectItem ea (FunCall ea "count" [Identifier ea "b"]) "cnt"])
>          [Tref ea (i "c") NoAlias] Nothing [Identifier ea "a"]
>          (Just $ FunCall ea ">" [Identifier ea "cnt", IntegerLit ea 4])
>          [] Nothing Nothing]
>      ,s "select a from (select 1 as a, 2 as b) x;"
>         [selectFrom
>          [selI "a"]
>          (SubTref ea (selectE $ SelectList ea
>                                [SelectItem ea (IntegerLit ea 1) "a"
>                                ,SelectItem ea (IntegerLit ea 2) "b"])
>                   (TableAlias "x"))]
>      ]

>    ,Group "multiple statements" [
>       s "select 1;\nselect 2;" [selectE $ sl [SelExp ea (IntegerLit ea 1)]
>                                ,selectE $ sl [SelExp ea (IntegerLit ea 2)]]
>      ]
>    ,Group "comments" [
>       s "" []
>      ,s "-- this is a test" []
>      ,s "/* this is\n\
>         \a test*/" []
>      ,s "select 1;\n\
>         \-- this is a test\n\
>         \select -- this is a test\n\
>         \2;" [selectE $ sl [SelExp ea (IntegerLit ea 1)]
>              ,selectE $ sl [SelExp ea (IntegerLit ea 2)]
>              ]
>      ,s "select 1;\n\
>         \/* this is\n\
>         \a test*/\n\
>         \select /* this is a test*/2;"
>                     [selectE $ sl [SelExp ea (IntegerLit ea 1)]
>                     ,selectE $ sl [SelExp ea (IntegerLit ea 2)]
>                     ]
>      ]
>    ,Group "some mis stuff" [
>       s "select (p).x, (p).y from pos;"
>         [selectFrom (selEL [qi "p" "x"
>                                                 ,qi "p" "y"])
>                                          (Tref ea (i "pos") NoAlias)]
>      ,s "select ($1).x, ($1).y from pos;"
>         [selectFrom (selEL [QIdentifier ea (PositionalArg ea 1) "x"
>                                                 ,QIdentifier ea (PositionalArg ea 1) "y"])
>                                          (Tref ea (i "pos") NoAlias)]
>      ,s "select row_number() over(), x from tb;"
>       [selectFrom
>        [SelExp ea
>                     (WindowFn ea
>                     (FunCall ea "row_number" [])
>                     []
>                     [] Asc FrameUnboundedPreceding)
>        , selI "x"]
>        (Tref ea (i "tb") NoAlias)]
>      ]
>      ]
>     ]
>
>  where
>    e = Expr
>    s = Stmt

-------------------------------------------------------------------------------

shortcuts for constructing test data and asts

> stringQ :: String -> ScalarExpression SourcePosition
> stringQ = StringLit ea
>
> selectFrom :: SelectItemList SourcePosition
>            -> TableRef SourcePosition
>            -> QueryExpression SourcePosition
> selectFrom selList frm = Select ea Dupes (SelectList ea selList)
>                            [frm] Nothing [] Nothing [] Nothing Nothing
>
> selectE :: SelectList SourcePosition -> QueryExpression SourcePosition
> selectE selList = Select ea Dupes selList
>                     [] Nothing [] Nothing [] Nothing Nothing
>
> selIL :: [String] -> [SelectItem SourcePosition]
> selIL = map selI
> selEL :: [ScalarExpression SourcePosition] -> [SelectItem SourcePosition]
> selEL = map (SelExp ea)
>
> i :: String -> ScalarExpression SourcePosition
> i = Identifier ea
>
> qi :: String -> String -> ScalarExpression SourcePosition
> qi c n = QIdentifier ea (i c) n
>
> selI :: String -> SelectItem SourcePosition
> selI = SelExp ea . Identifier ea
>
> sl :: SelectItemList SourcePosition -> SelectList SourcePosition
> sl a = SelectList ea a
>
> selectFromWhere :: SelectItemList SourcePosition
>                 -> TableRef SourcePosition
>                 -> ScalarExpression SourcePosition
>                 -> QueryExpression SourcePosition
> selectFromWhere selList frm whr =
>     Select ea Dupes (SelectList ea selList)
>                [frm] (Just whr) [] Nothing [] Nothing Nothing
>

> ea :: SourcePosition
> ea = ("",0,0)

--------------------------------------------------------------------------------

Unit test helpers

> itemToTft :: Item -> Test.Framework.Test
> itemToTft (Expr a b) = testParseExpression a b
> itemToTft (Stmt a b) = testParseStatements a b
> itemToTft (Group s is) = testGroup s $ map itemToTft is
>
> testParseExpression :: String -> ScalarExpression SourcePosition -> Test.Framework.Test
> testParseExpression src ast = parseUtil src ast
>                                  (parseScalarExpression "") printScalarExpression
>
> testParseStatements :: String -> [QueryExpression SourcePosition] -> Test.Framework.Test
> testParseStatements src ast = parseUtil src ast (parseQueryExpressions "") printQueryExpressions
>
>

> parseUtil :: (Show t, Eq b, Show b, Data b) =>
>              String
>           -> b
>           -> (String -> Either t b)
>           -> (b -> String)
>           -> Test.Framework.Test
> parseUtil src ast parser printer = testCase ("parse " ++ src) $
>   case parser src of
>     Left er -> assertFailure $ show er
>     Right ast' -> do
>       assertEqual ("parse " ++ src) ast $ resetSourcePositions ast'
>       case parser (printer ast) of
>         Left er -> assertFailure $ "reparse\n" ++ show er ++ "\n" -- ++ pp ++ "\n"
>         Right ast'' -> assertEqual ("reparse " ++ printer ast) ast $ resetSourcePositions ast''
