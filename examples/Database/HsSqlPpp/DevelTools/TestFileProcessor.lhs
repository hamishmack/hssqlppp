        
Copyright 2010 Jake Wheat

Code to read some of the test files and convert to html to serve as
examples.

> {-# LANGUAGE QuasiQuotes #-}
> module Database.HsSqlPpp.DevelTools.TestFileProcessor
>     (parserTestsTable
>     ,typeCheckTestsTable
>     ,quasiQuoteTestsTable) where
> import Data.Char
>
> import Database.HsSqlPpp.Utils.Utils
> import Database.HsSqlPpp.Utils.Here
> import Database.HsSqlPpp.Tests.ParserTests as PT
> import Database.HsSqlPpp.Tests.TypeCheckTests as TT
> import Language.Haskell.Exts hiding (String)
> --import qualified Language.Haskell.Exts as Exts
> --import Data.Generics
> import Data.Generics.Uniplate.Data
>
> data Row = Row [[Text]]
>          | HHeader String
>
> data Text = Text String
>           | Sql String
>           | Haskell String
>
> parserTestsTable :: String
> parserTestsTable = parserIntro ++ rowsToHtml (mapParserTests PT.parserTestData)
>
> typeCheckTestsTable :: String
> typeCheckTestsTable = typeCheckIntro ++ rowsToHtml (mapTypeCheckTests TT.typeCheckTestData)
>
>
> mapParserTests :: PT.Item -> [Row]
> mapParserTests (PT.Expr s e) = [Row [[Sql s],[Haskell (ppExpr e)]]]
> mapParserTests (PT.Stmt s e) = [Row [[Sql s],[Haskell (ppExpr e)]]]
> mapParserTests (PT.PgSqlStmt s e) = [Row [[Sql s],[Haskell (ppExpr e)]]]
> mapParserTests (PT.Group n is) = HHeader n : concatMap mapParserTests is

need to use haskell-src-exts for the quasi quote tests since we want
to get the quasi quote source syntax, not the asts it produces at
compile time.

> mapTypeCheckTests :: TT.Item -> [Row]
> mapTypeCheckTests (TT.Group n is) = HHeader n : concatMap mapTypeCheckTests is
> mapTypeCheckTests (TT.Expr s r) = [Row [[Sql s],[Haskell (ppExpr r)]]]
> mapTypeCheckTests (TT.StmtType s r) = [Row [[Sql s],[Haskell (ppExpr r)]]]
> mapTypeCheckTests (TT.CatStmtType s c r) = [Row [[Haskell (ppExpr c),Sql s],[Haskell (ppExpr r)]]]
> mapTypeCheckTests (TT.Ddl s c) = [Row [[Sql s],[Haskell (ppExpr c)]]]
>
> rowsToHtml :: [Row] -> String
> rowsToHtml rs =
>   "<table>" ++
>   concatMap rowToHtml rs ++
>   "</table>"
>   where
>     rowToHtml (Row rws) =
>         let w = 100 `div` (length rws)
>             md r = "<td style='width:" ++
>                      show w ++ "%'>" ++ concatMap tToH r ++
>                      "</td>"
>         in "<tr>" ++ concatMap md rws ++ "</tr>"
>     rowToHtml (HHeader s) =
>         "</table>\n" ++ s ++ "\n" ++ map (const '=') s ++ "\n<table>\n"
>     tToH (Text s) = s
>     tToH (Sql s) = code "SqlPostgresql" s
>     tToH (Haskell s) = code "haskell" s
>     code t s = "\n\n~~~~~~{." ++ t ++ "}\n"
>                ++ trim s
>                ++ "\n~~~~~~\n\n"
>
> trim :: String -> String
> trim = f . f
>    where f = reverse . dropWhile isSpace

-------------------------------------------------------------------------------

> quasiQuoteTestsTable :: IO String
> quasiQuoteTestsTable = do
>
>   ast <- pf "examples/Database/HsSqlPpp/Tests/QuasiQuoteTests.lhs"
>   let lets = [l | l@(Let _ _) <- universeBi ast]
>   --mapM_ (putStrLn . prettyPrint) lets
>   return $ qqIntro ++ rowsToHtml (map ((\s -> Row [[Haskell s]]) . prettyPrint) lets)


> pf :: String -> IO Module
> pf f = do
>   x <- parseFile f
>   case x of
>         ParseOk ast -> return ast
>         e -> error $ show e

 >                     [Let
 >                        (BDecls
 >                           [PatBind
 >                              (SrcLoc{srcFilename =
 >                                        "examples/Database/HsSqlPpp/Tests/QuasiQuoteTests.lhs",
 >                                      srcLine = 37, srcColumn = 21})
 >                              (PVar (Ident "tablename"))
 >                              Nothing
 >                              (UnGuardedRhs (Lit (String "my_table")))
 >                              (BDecls []),
 >                            PatBind
 >                              (SrcLoc{srcFilename =
 >                                        "examples/Database/HsSqlPpp/Tests/QuasiQuoteTests.lhs",
 >                                      srcLine = 38, srcColumn = 21})
 >                              (PVar (Ident "varname"))
 >                              Nothing
 >                              (UnGuardedRhs (Lit (String "my_field")))
 >                              (BDecls []),
 >                            PatBind
 >                              (SrcLoc{srcFilename =
 >                                        "examples/Database/HsSqlPpp/Tests/QuasiQuoteTests.lhs",
 >                                      srcLine = 39, srcColumn = 21})
 >                              (PVar (Ident "typename"))
 >                              Nothing
 >                              (UnGuardedRhs (Lit (String "text")))
 >                              (BDecls [])])
 >                        (App
 >                           (App (Con (UnQual (Ident "Stmt")))
 >                              (QuasiQuote "sqlStmt"
  >                                 "\n \n                      create table $(tablename) (\n                        $(varnam e) $(typename)\n                      );\n \n                         "))
 >                           (QuasiQuote "sqlStmt"
 >                              "\n                      create table my_table (\n                        my_field text\n >                        );\n                         "))])])))

> parserIntro :: String
> parserIntro = [$here|
>
> The parser examples have the sql source on the left, and the ast that the parser produces
> the right, stripped of the source position annotations.
>
> |]

> typeCheckIntro :: String
> typeCheckIntro = [$here|
>
> The type checking examples have the sql on the left and the result of type checking
> on the right. Different sections are using different tests:
>
> * parse and type check expression, return either type error or type
> * parse and type check a list of statements, return either type error or the top level statement type annotation
> * update the catalog, then check a list of statements
> * parse and type check some ddl statements, return the list of catalog updates they generate
>
> It's a bit rough at the moment, the clarity will be improved.
>
> |]

> qqIntro :: String
> qqIntro = [$here|
>
> Pretty rough presentation, each example is a lets, with a pair of sql
> quasiquotes: one with antiquotes, and one with the resultant sql without antiquotes.
>
> |]

