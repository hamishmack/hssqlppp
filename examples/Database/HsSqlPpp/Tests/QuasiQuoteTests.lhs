Copyright 2010 Jake Wheat

Tests mainly for antiquotation, plus examples of where antiquotes work.

> {-# LANGUAGE QuasiQuotes,ScopedTypeVariables #-}

> module Database.HsSqlPpp.Tests.QuasiQuoteTests (quasiQuoteTests, quasiQuoteTestData, Item(..)) where

> import Test.HUnit
> import Test.Framework
> import Test.Framework.Providers.HUnit
> import Data.Generics

> import Database.HsSqlPpp.Utils.Here

> import Database.HsSqlPpp.Ast
> import Database.HsSqlPpp.Annotation
> import Database.HsSqlPpp.Parser
> import Database.HsSqlPpp.PrettyPrinter
> import Database.HsSqlPpp.SqlQuote


> data Item = Expr Expression Expression
>           | Stmts [Statement] [Statement]
>           | PgSqlStmts [Statement] [Statement]
>           | Stmt Statement Statement
>           | PgSqlStmt Statement Statement
>           | Group String [Item]
> quasiQuoteTests :: Test.Framework.Test
> quasiQuoteTests = itemToTft quasiQuoteTestData

> quasiQuoteTestData :: Item
> quasiQuoteTestData =
>   Group "quasiQuoteTests" [

================================================================================

expressions

>    Group "stuff" [
>               let tablename = "my_table"
>                   varname = "my_field"
>                   typename = "text"
>               in Stmt [$sqlStmt|
>
>                     create table $(tablename) (
>                       $(varname) $(typename)
>                     );
>
>                        |]
>                       [$sqlStmt|
>                     create table my_table (
>                       my_field text
>                     );
>                        |]

>     ]]

let x = "y" in Expr [$sqlExpr| $(x) |]

================================================================================

Unit test helpers

> itemToTft :: Item -> Test.Framework.Test
> itemToTft (Expr a b) = testCase (printExpression b) $ stripEqual a b
> itemToTft (PgSqlStmt a b) = testCase (printSql [b]) $ stripEqual a b
> itemToTft (Stmt a b) = testCase (printSql [b]) $  stripEqual a b
> itemToTft (PgSqlStmts a b) = testCase (printSql b) $ stripEqual a b
> itemToTft (Stmts a b) = testCase (printSql b) $ stripEqual a b
> itemToTft (Group s is) = testGroup s $ map itemToTft is


 > testExpression :: Expression -> Expression -> Test.Framework.Test
 > testExpression src ast = stripEqual src ast

> stripEqual a b = assertEqual "" (stripAnnotations a) (stripAnnotations b)

TODO
new idea for testing:
parsesql -> ast1
parse, pretty print, parse -> ast2
load into pg, pg_dump, parse -> ast3
parse, pretty print, load into pg, pg_dump, parse -> ast4
check all these asts are the same
