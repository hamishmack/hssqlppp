
This file contains the lexer for sql source text.

Lexicon:

~~~~
string
identifier or keyword
symbols - operators and ;,()[]
positional arg
int
float
copy payload (used to lex copy from stdin data)
~~~~

> module Database.HsSqlPpp.Parsing.Lexer (
>               Token
>              ,Tok(..)
>              ,lexSqlFile
>              ,lexSqlText
>              ,lexSqlTextWithPosition
>              ,identifierString
>              ,LexState
>              ) where
> import Text.Parsec hiding(many, optional, (<|>))
> import qualified Text.Parsec.Token as P
> import Text.Parsec.Language
> --import Text.Parsec.String
> import Text.Parsec.Pos
>
> import Control.Applicative
> import Control.Monad.Identity

> import Data.Maybe
>
> import Database.HsSqlPpp.Parsing.ParseErrors
> import Database.HsSqlPpp.Utils.Utils
> -- import Database.HsSqlPpp.Ast.Name

================================================================================

= data types

> type Token = (SourcePos, Tok)
>
> data Tok = StringTok String String --delim, value (delim will one of
>                                    --', $$, $[stuff]$

>          | IdStringTok String -- either a identifier component (without .) or a *
>          | QIdStringTok String -- same as IdStringTok with quotes
>          | BIdStringTok String -- same as IdStringTok with brackets

>          | SymbolTok String -- operators, and ()[],;: and also .
>                             -- '*' is currently always lexed as an id
>                             --   rather than an operator
>                             -- this gets fixed in the parsing stage

>          | PositionalArgTok Integer -- used for $1, etc.

Use a numbertok with a string to parse numbers. This is mainly so that
numeric constants can be parsed accurately - if they are parsed to
floats in the ast then converted back to numeric, then the accuracy
can be lost (e.g. something like "0.2" parsing to 0.199999999 float.

>          | NumberTok String

>          | CopyPayloadTok String -- support copy from stdin; with inline data
>            deriving (Eq,Show)
>
> type LexState = [Tok]
> type Parser = ParsecT String LexState Identity
>
> lexSqlFile :: FilePath -> IO (Either ParseErrorExtra [Token])
> lexSqlFile f = do
>   te <- readFile f
>   let x = runParser sqlTokens [] f te
>   return $ toParseErrorExtra x Nothing te
>
> lexSqlText :: String -> String -> Either ParseErrorExtra [Token]
> lexSqlText f s = toParseErrorExtra (runParser sqlTokens [] f s) Nothing s
>
> lexSqlTextWithPosition :: String -> Int -> Int -> String
>                        -> Either ParseErrorExtra [Token]
> lexSqlTextWithPosition f l c s =
>   toParseErrorExtra (runParser (do
>                                 setPosition (newPos f l c)
>                                 sqlTokens) [] f s) (Just (l,c)) s

================================================================================

= lexers

lexer for tokens, contains a hack for copy from stdin with inline
table data.

> sqlTokens :: Parser [Token]
> sqlTokens =
>   setState [] >>
>   whiteSpace >>
>   many sqlToken <* eof

Lexer for an individual token.

Could lex lazily and when the lexer reads a copy from stdin statement,
it switches lexers to lex the inline table data, then switches
back. Don't know how to do this in parsec, or even if it is possible,
so as a work around, you use the state to trap if we've just seen 'from
stdin;', if so, you read the copy payload as one big token, otherwise
we read a normal token.

> sqlToken :: Parser Token
> sqlToken = do
>            sp <- getPosition
>            sta <- getState
>            t <- if sta == [ft,st,mt]
>                 then copyPayload
>                 else try sqlNumber
>                  <|> try sqlString
>                  <|> try idString
>                  <|> try qidString
>                  <|> try bidString
>                  <|> try positionalArg
>                  <|> try sqlSymbol
>            updateState $ \stt ->
>              case () of
>                      _ | stt == [] && t == ft -> [ft]
>                        | stt == [ft] && t == st -> [ft,st]
>                        | stt == [ft,st] && t == mt -> [ft,st,mt]
>                        | otherwise -> []
>
>            return (sp,t)
>            where
>              ft = IdStringTok "from"
>              st = IdStringTok "stdin"
>              mt = SymbolTok ";"

== specialized token parsers

> sqlString :: Parser Tok
> sqlString = stringQuotes <|> stringLD
>   where
>     --parse a string delimited by single quotes
>     stringQuotes = StringTok "\'" <$> stringPar
>     stringPar = optional (char 'E') *> char '\''
>                 *> readQuoteEscape <* whiteSpace
>     --(readquoteescape reads the trailing ')

have to read two consecutive single quotes as a quote character
instead of the end of the string, probably an easier way to do this

other escapes (e.g. \n \t) are left unprocessed

>     readQuoteEscape = do
>                       x <- anyChar
>                       if x == '\''
>                         then try ((x:) <$> (char '\'' *> readQuoteEscape))
>                              <|> return ""
>                         else (x:) <$> readQuoteEscape

parse a dollar quoted string

>     stringLD = do
>                -- cope with $$ as well as $[identifier]$
>                tag <- try (char '$' *> ((char '$' *> return "")
>                                    <|> (identifierString <* char '$')))
>                s <- lexeme $ manyTill anyChar
>                       (try $ char '$' <* string tag <* char '$')
>                return $ StringTok ("$" ++ tag ++ "$") s
>
> idString :: Parser Tok
> idString = IdStringTok <$> identifierString

> qidString :: Parser Tok
> qidString = QIdStringTok <$> qidentifierString

> bidString :: Parser Tok
> bidString = BIdStringTok <$> bidentifierString


>
> positionalArg :: Parser Tok
> positionalArg = char '$' >> PositionalArgTok <$> integer


Lexing symbols:

~~~~
approach 1:
try to keep multi symbol operators as single lexical items
(e.g. "==", "~=="

approach 2:
make each character a separate element
e.g. == lexes to ['=', '=']
then the parser sorts this out

Sort of using approach 1 at the moment, see below

== notes on symbols in pg operators
pg symbols can be made from:

=_*/<>=~!@#%^&|`?

no --, /* in symbols

can't end in + or - unless contains
~!@#%^&|?

Most of this isn't relevant for the current lexer.

== sql symbols for this lexer:

sql symbol is one of
()[],; - single character
+-*/<>=~!@#%^&|`? string - one or more of these, parsed until hit char
which isn't one of these (including whitespace). This will parse some
standard sql expressions wrongly at the moment, work around is to add
whitespace e.g. i think 3*-4 is valid sql, should lex as '3' '*' '-'
'4', but will currently lex as '3' '*-' '4'. This is planned to be
fixed in the parser.
.. := :: : - other special cases
A single * will lex as an identifier rather than a symbol, the parser
deals with this.

~~~~

> sqlSymbol :: Parser Tok
> sqlSymbol =
>   SymbolTok <$> lexeme (choice [
>                          replicate 1 <$> oneOf "()[],;"
>                         ,try $ string ".."
>                         ,string "."
>                         ,try $ string "::"
>                         ,try $ string ":="
>                         ,string ":"
>                         ,try $ string "$(" -- antiquote standard splice
>                         ,try $ string "$s(" -- antiquote string splice
>                         ,string "$i(" -- antiquote identifier splice
>                         ,many1 (oneOf "+-*/<>=~!@#%^&|`?")
>                         ])
>

parse a number:
digits
digits.[digits][e[+-]digits]
[digits].digits[e[+-]digits]
digitse[+-]digits

I'm sure the implementation can be simpler than this

> sqlNumber :: Parser Tok
> sqlNumber = NumberTok <$> lexeme (
>   choice [do
>           -- starts with digits
>           d <- digits
>           suff <- choice [-- complete fractional part
>                           try fracPart
>                          ,-- dot followed by optional exp
>                           -- check for .. symbol
>                           choice [try $ do
>                                         _ <- lookAhead $ string ".."
>                                         return []
>                                  ,do
>                                   _ <- char '.'
>                                   e <- optionMaybe expn
>                                   return $ concat $ catMaybes
>                                     [Just "."
>                                     ,e]
>                                   ]
>                          ,--no dot then expn
>                           expn
>                           -- just an integer
>                          ,return ""
>                          ]
>           return $ d ++ suff
>          ,fracPart
>          ])
>   where
>      fracPart = do
>           _ <- char '.'
>           d <- digits
>           e <- optionMaybe expn
>           return $ concat $ catMaybes
>             [Just "."
>             ,Just d
>             ,e]
>      expn = do
>        _ <- char 'e'
>        s <- optionMaybe (char '+' <|> char '-')
>        d <- digits
>        return $ concat $ catMaybes [Just "e"
>                                    ,fmap (:[]) s
>                                    ,Just d]
>      digits = many1 digit

================================================================================

additional parser bits and pieces

include * in identifier strings during lexing. This parser is also
used for keywords, so identifiers and keywords aren't distinguished
until during proper parsing, and * isn't really examined until type
checking

> identifierString :: Parser String
> identifierString = lexeme $ (letter <|> char '_')
>                             <:> many (alphaNum <|> char '_')

todo:
select adrelid as "a""a" from pg_attrdef;
creates a column named: 'a"a' with a double quote in it

> qidentifierString :: Parser String
> qidentifierString = lexeme $ char '"' *> many (noneOf "\"") <* char '"'

> bidentifierString :: Parser String
> bidentifierString = lexeme $ char '[' *> many (noneOf "]") <* char ']'

parse the block of inline data for a copy from stdin, ends with \. on
its own on a line

> copyPayload :: Parser Tok
> copyPayload = CopyPayloadTok <$> lexeme (getLinesTillMatches "\\.\n")
>   where
>     getLinesTillMatches s = do
>                             x <- getALine
>                             if x == s
>                               then return ""
>                               else (x++) <$> getLinesTillMatches s
>     getALine = (++"\n") <$> manyTill anyChar (try newline)
>

================================================================================

= parsec pass throughs

> --symbol :: String -> Parser String
> --symbol = P.symbol lexer
>

> integer :: Parser Integer
> integer = lexeme $ P.integer lexer

> whiteSpace :: Parser ()
> whiteSpace = P.whiteSpace lexer
>
> lexeme :: Parser a -> Parser a
> lexeme = P.lexeme lexer

this lexer isn't really used as much as it could be, probably some of
the fields are not used at all (like identifier and operator stuff)

> lexer :: P.GenTokenParser String LexState Identity
> lexer = P.makeTokenParser (emptyDef {
>                             P.commentStart = "/*"
>                            ,P.commentEnd = "*/"
>                            ,P.commentLine = "--"
>                            ,P.nestedComments = False
>                            ,P.identStart = letter <|> char '_'
>                            ,P.identLetter    = alphaNum <|> oneOf "_"
>                            ,P.opStart        = P.opLetter emptyDef
>                            ,P.opLetter       = oneOf opLetters
>                            ,P.reservedOpNames= []
>                            ,P.reservedNames  = []
>                            ,P.caseSensitive  = False
>                            })
>
> opLetters :: String
> opLetters = ".:^*/%+-<>=|!"
