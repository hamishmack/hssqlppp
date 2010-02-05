Copyright 2010 Jake Wheat

Extension to remove the boilerplate from adding tables with a single
attribute and single row, a bit like a global variable in the
database.

> {-# LANGUAGE ViewPatterns, QuasiQuotes #-}
>
> module Database.HsSqlPpp.Examples.Extensions.CreateVar
>     where
>
> import Data.Generics
> import Data.Generics.Uniplate.Data
>
> import Database.HsSqlPpp.Ast
> import Database.HsSqlPpp.Utils.Here
> import Database.HsSqlPpp.Examples.Extensions.ExtensionsUtils
>
> createVarExample :: ExtensionTest
> createVarExample = ExtensionTest
>   "CreateVar"
>   createVar
>   "select create_var('varname', 'vartype');"
>   [$here|
>
>   create table varname_table (
>     varname vartype
>   );
>
>   create function get_varname() returns vartype as $a$
>     select * from varname_table;
>   $a$ language sql stable;
>
>   -- haven't needed this
>   /*drop function if exists varname_table_constraint_trigger_operator();
>   create function varname_table_constraint_trigger_operator() returns trigger as $a$
>   begin
>     null;
>   end;
>   $a$ language plpgsql;*/
>
>   -- todo once constraint extension is written
>   /*create function check_con_varname_table_varname_key() returns boolean as $a$
>   begin
>     return true;
>   end;
>   $a$ language plpgsql stable;
>
>   create function check_con_varname_table_01_tuple() returns boolean as $a$
>   begin
>     return true;
>   end;
>   $a$ language plpgsql stable;
>   drop function if exists varname_table_constraint_trigger_operator();
>   create function varname_table_constraint_trigger_operator() returns trigger as $a$
>   begin
>     null;
>   end;
>   $a$ language plpgsql;*/
>   |]
>
> createVar :: Data a => a -> a
> createVar =
>     transformBi $ \x ->
>       case x of
>         (funCallView -> FunCallView _ "create_var" [StringLit _ _ tableName,StringLit _ _ typeName]):tl
>             -> mapStrings [("createvarvarname_table", tableName ++ "_table")
>                                  ,("createvarvarname", tableName)
>                                  ,("createvarvartype", typeName)
>                                  ,("get_createvarvarname", "get_" ++ typeName)]
>                  createVarTemplate
>                  ++ tl
>         x1 -> x1
>
> createVarTemplate :: [Statement]
> createVarTemplate = --let (ExtensionTest _ _ x) = createVarExample
>                     --in x --don't do this cos it doesn't give us a very good test
>   readTemplate
>   [$here|
>
>   create table createvarvarname_table (
>    createvarvarname createvarvartype
>   );
>
>   create function get_createvarvarname() returns createvarvartype as $a$
>     select * from createvarvarname_table;
>   $a$ language sql stable;
>
>   |]
