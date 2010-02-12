Copyright 2010 Jake Wheat

File to parse the denormalized6nf syntax

> {-# LANGUAGE QuasiQuotes, DeriveDataTypeable #-}
> module Database.HsSqlPpp.Examples.Extensions.DenormSyntax
>     (D6nfStatement(..)
>     ,parseD6nf
>     ,denormParseTests
>     )where
>
> import Text.Parsec hiding(many, optional, (<|>), string)
> --import Text.Parsec.Expr
> import Text.Parsec.String
> --import Text.Parsec.Perm
> import Test.HUnit
> import Test.Framework
> import Test.Framework.Providers.HUnit
> import Control.Applicative
> import Data.Generics
> --import Control.Monad.Identity
> --import Control.Monad.Error
>
> import Database.HsSqlPpp.Ast
> import Database.HsSqlPpp.AstInternals.AstAnti (attributeDef)
> import Database.HsSqlPpp.Parsing.ParserInternal
> import Database.HsSqlPpp.Parsing.Lexer
> import Database.HsSqlPpp.Utils.Here
> import Database.HsSqlPpp.Utils.Utils
> import Database.HsSqlPpp.Annotation
>
> data D6nfStatement = DTable String [String] [AttributeDef]
>                    | MutualExclusion String String
>                      deriving (Eq,Show,Typeable,Data)
>

tests
=====

> data ParseTest = ParseTest String (Either String [D6nfStatement])
>
> tests :: [ParseTest]
> tests = [ParseTest [$here|
>         pieces (
>           ptype text primary key
>         );
>         |] $ Right [ DTable "pieces" []
>                        [AttributeDef []
>                         "ptype"
>                         (SimpleTypeName [] "text")
>                         Nothing
>                         [RowPrimaryKeyConstraint [] ""]]]
>          ,ParseTest [$here|
>         creatures : pieces (
>           speed int,
>           agility int
>         );
>         |] $ Right [ DTable "creatures" ["pieces"]
>                        [AttributeDef [] "speed"
>                         (SimpleTypeName [] "int") Nothing []
>                        ,AttributeDef [] "agility"
>                         (SimpleTypeName [] "int") Nothing []]]
>          ,ParseTest [$here|
>         mutually_exclusive(attackers,creatures);
>         |] $ Right [ MutualExclusion "attackers" "creatures"]
>
>          ,ParseTest [$here|
>         monsters : creatures, attackers (
>           resistance int,
>           armour int
>         );
>         |] $ Right [ DTable "monsters" ["creatures", "attackers"]
>                        [AttributeDef [] "resistance"
>                         (SimpleTypeName [] "int") Nothing []
>                        ,AttributeDef [] "armour"
>                         (SimpleTypeName [] "int") Nothing []]]
>          ,ParseTest [$here|
>         attacking_creatures : pieces,attackers;
>         |] $ Right [ DTable "attacking_creatures" ["pieces","attackers"] []]
>         ]
>
> denormParseTests :: Test.Framework.Test
> denormParseTests = testGroup "d6nf parsing" $ map parseTestConv tests
>
> parseTestConv :: ParseTest -> Test.Framework.Test
> parseTestConv (ParseTest s t) =
>   testCase s $ do
>     let t1 = parseD6nf s
>     assertEqual "" t (stripAnnotations t1)

--------------------------------

parsing code
============

> type MyParser = GenParser Token ParseState
>
> type ParseState = ()
>
> startState :: ()
> startState = ()
>
> parseD6nf :: String -> Either String [D6nfStatement]
> parseD6nf s = do
>     toks <- tsl $ lexSqlText "" s
>     tsl $ runParser statements startState "" toks
>
> statements :: MyParser [D6nfStatement]
> statements = many1 statement
>
> statement :: MyParser D6nfStatement
> statement = mutualExclusion <|> dTable
>
> dTable :: MyParser D6nfStatement
> dTable = DTable
>          <$> idString
>          <*> option [] (symbol ":" *> commaSep1 idString)
>          <*> (option []
>                      (parens (commaSep
>                              (attributeDef <$> tableAttribute))))
>              <* symbol ";"
>
> mutualExclusion :: MyParser D6nfStatement
> mutualExclusion = do
>   keyword "mutually_exclusive"
>   parens (MutualExclusion
>           <$> idString
>           <*> (symbol "," *> idString)) <* symbol ";"
