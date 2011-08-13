

> module Database.HsSqlPpp.Tests.Parsing.Plpgsql (pgplsqlParsingTestData) where
>
> --import Test.HUnit
> --import Test.Framework
> --import Test.Framework.Providers.HUnit
> --import Data.Generics
>
> --import Database.HsSqlPpp.Utils.Here
>
> import Database.HsSqlPpp.Ast
> --import Database.HsSqlPpp.Annotation
> --import Database.HsSqlPpp.Parser
> --import Database.HsSqlPpp.Pretty

> import Database.HsSqlPpp.Tests.Parsing.Utils

> pgplsqlParsingTestData:: Item
> pgplsqlParsingTestData =
>   Group "plpgsql" [
>     Group "simple plpgsql statements" [
>       f "success := true;"
>       [Assignment ea (ei "success") (BooleanLit ea True)]
>      ,f "success = true;"
>       [Assignment ea (ei "success") (BooleanLit ea True)]
>      ,f "return true;"
>       [Return ea $ Just (BooleanLit ea True)]
>      ,f "return;"
>       [Return ea Nothing]
>      ,f "return next 1;"
>       [ReturnNext ea $ NumberLit ea "1"]
>      ,f "return query select a from b;"
>       [ReturnQuery ea $ selectFrom [selI "a"] (Tref ea (i "b") (NoAlias ea))]
>      ,f "raise notice 'stuff %', 1;"
>       [Raise ea RNotice "stuff %" [NumberLit ea "1"]]
>      ,f "perform test();"
>       [Perform ea $ FunCall ea "test" []]
>      ,f "perform test(a,b);"
>       [Perform ea $ FunCall ea "test" [Identifier ea "a", Identifier ea "b"]]
>      ,f "perform test(r.relvar_name || '_and_stuff');"
>       [Perform ea $ FunCall ea "test" [
>                     FunCall ea "||" [eqi "r" "relvar_name"
>                                     ,stringQ "_and_stuff"]]]
>      {-,f "select into a,b c,d from e;"
>       [Into ea False [ei "a", ei "b"]
>        $ QueryStatement ea $ Select ea Dupes (SelectList ea [selI "c", selI "d"])
>              [Tref ea (i "e") (NoAlias ea)] Nothing [] Nothing [] Nothing Nothing]-}
>      {-,f "select c,d into a,b from e;"
>       [Into ea False [ei "a", ei "b"]
>        $ QueryStatement ea $ Select ea Dupes (SelectList ea [selI "c", selI "d"])
>        [Tref ea (i "e") (NoAlias ea)] Nothing [] Nothing [] Nothing Nothing]-}
>      ,f "update pieces\n\
>         \set a=b returning tag into r.tag;"
>       [Into ea False [IntoIdentifier ea ["r","tag"]]
>          $ Update ea (dqi "pieces") [FunCall ea "=" [Identifier ea "a"
>                                                     ,Identifier ea "b"]]
>            []
>            Nothing (Just (SelectList ea
>                           [SelExp ea (Identifier ea "tag")]))]
>      ,f "insert into t(a) values (1) returning id into x;"
>       [Into ea False [IntoIdentifier ea ["x"]]
>        $ Insert ea
>         (dqi "t")
>         ["a"]
>         (Values ea [[NumberLit ea "1"]])
>         (Just $ sl [selI "id"])]

>      ,f "update t\n\
>         \  set x = 1 returning id into z;"
>       [Into ea False [IntoIdentifier ea ["z"]]
>       $ Update ea (dqi "t") [FunCall ea "=" [Identifier ea "x", NumberLit ea "1"]]
>         [] Nothing (Just $ sl [selI "id"])]

>      ,f "execute s;"
>       [Execute ea (Identifier ea "s")]
>      ,f "execute s into r;"
>       [Into ea False [IntoIdentifier ea ["r"]] (Execute ea (Identifier ea "s"))]
>     ,f "continue;" [ContinueStatement ea Nothing]
>     ]
>
>     ,Group "other plpgsql statements" [
>       f "for r in select a from tbl loop\n\
>         \null;\n\
>         \end loop;"
>       [ForQueryStatement ea Nothing (ei "r") (selectFrom  [selI "a"] (Tref ea (i "tbl") (NoAlias ea)))
>        [NullStatement ea]]
>      ,f "for r in select a from tbl where true loop\n\
>         \null;\n\
>         \end loop;"
>       [ForQueryStatement ea Nothing (ei "r")
>        (selectFromWhere [selI "a"] (Tref ea (i "tbl") (NoAlias ea)) (BooleanLit ea True))
>        [NullStatement ea]]
>      ,f "for r in 1 .. 10 loop\n\
>         \null;\n\
>         \end loop;"
>       [ForIntegerStatement ea Nothing (ei "r")
>        (NumberLit ea "1") (NumberLit ea "10")
>        [NullStatement ea]]
>       -- catch a bug in lexing where 1..10 is parsed as
>       -- num "1.", num ".10", instead of num 1, symbol "..", num 10
>      ,f "for r in 1..10 loop\n\
>         \null;\n\
>         \end loop;"
>       [ForIntegerStatement ea Nothing (ei "r")
>        (NumberLit ea "1") (NumberLit ea "10")
>        [NullStatement ea]]
>
>      ,f "if a=b then\n\
>         \  update c set d = e;\n\
>         \end if;"
>       [If ea [(FunCall ea "=" [ei "a", ei "b"]
>               ,[Update ea (dqi "c") [FunCall ea "=" [ei "d"
>                                                     ,ei "e"]] [] Nothing Nothing])]
>        []]
>      ,f "if true then\n\
>         \  null;\n\
>         \else\n\
>         \  null;\n\
>         \end if;"
>       [If ea [((BooleanLit ea True),[NullStatement ea])]
>        [NullStatement ea]]
>      ,f "if true then\n\
>         \  null;\n\
>         \elseif false then\n\
>         \  return;\n\
>         \end if;"
>       [If ea [((BooleanLit ea True), [NullStatement ea])
>           ,((BooleanLit ea False), [Return ea Nothing])]
>        []]
>      ,f "if true then\n\
>         \  null;\n\
>         \elseif false then\n\
>         \  return;\n\
>         \elsif false then\n\
>         \  return;\n\
>         \else\n\
>         \  return;\n\
>         \end if;"
>       [If ea [((BooleanLit ea True), [NullStatement ea])
>           ,((BooleanLit ea False), [Return ea Nothing])
>           ,((BooleanLit ea False), [Return ea Nothing])]
>        [Return ea Nothing]]
>      ,f "case a\n\
>         \  when b then null;\n\
>         \  when c,d then null;\n\
>         \  else null;\n\
>         \end case;"
>      [CaseStatementSimple ea (Identifier ea "a")
>       [([Identifier ea "b"], [NullStatement ea])
>       ,([Identifier ea "c", Identifier ea "d"], [NullStatement ea])]
>       [NullStatement ea]]
>     ]]
>  where
>    --e = Expr
>    --s = Stmt
>    f = PgSqlStmt