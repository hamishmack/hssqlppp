PATH=$PATH:/home/jake/.cabal/bin/ uuagc  -dcfwsp --cycle --genlinepragmas /home/jake/wd/hssqlppp/selects/src/Database/HsSqlPpp/AstInternals/AstInternal.ag && ghc -XScopedTypeVariables -XDeriveDataTypeable --make -Wall -threaded -i/home/jake/wd/hssqlppp/selects/src:/home/jake/wd/hssqlppp/selects/tests:/home/jake/wd/hssqlppp/selects/devel /home/jake/wd/hssqlppp/selects/tests/Tests.lhs

> import Database.HsSqlPpp.Tests.Parsing.ParsingTests
> import Database.HsSqlPpp.Tests.TypeChecking.Identifiers

> import Test.Framework

> main :: IO ()
> main = defaultMain [parsingTests
>                    ,identifierTests]


PATH=$PATH:/home/jake/.cabal/bin/ ghc -XScopedTypeVariables -XDeriveDataTypeable -cpp -pgmPcpphs -optP--cpp --make -i/home/jake/wd/hssqlppp/trunk/src:/home/jake/wd/hssqlppp/trunk/tests:/home/jake/wd/hssqlppp/trunk/devel:/home/jake/wd/hssqlppp/trunk/examples/chaos/:/home/jake/wd/hssqlppp/trunk/examples/extensions/:/home/jake/wd/hssqlppp/trunk/examples/util/:/home/jake/wd/hssqlppp/trunk/examples/wrappers/ /home/jake/wd/hssqlppp/trunk/tests/Tests.lhs