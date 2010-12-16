
> {- | This module contains the ast node data types. They are very
>      permissive, in that they allow a lot of invalid SQL to be
>      represented. The type checking process should catch all invalid
>      trees, but doesn't quite manage at the moment.  Sorry about all
>      the seemingly pointless type synonyms below, they are an
>      artefact of using UUAGC. You can see labels for the fields by
>      looking at the ag source here:
>      <http://community.haskell.org/~JakeWheat/hssqlppp/source/src/Database/HsSqlPpp/AstInternals/AstInternal.ag.html>
>      -}
>
> module Database.HsSqlPpp.Ast
>     (-- * Query Expressions
>      QueryExpression(..)
>     ,WithQueryList
>     ,WithQuery(..)
>     ,CombineType(..)
>     ,Direction(..)
>     ,Distinct(..)
>     -- * Select Lists
>     ,SelectList(..)
>     ,SelectItem(..)
>     -- * Table Refs
>     ,TableRef(..)
>     ,TableAlias(..)
>     ,JoinExpression(..)
>     ,JoinType(..)
>     ,Natural(..)
>     -- * Scalar Expressions
>     ,ScalarExpression(..)
>     ,LiftFlavour(..)
>     ,InList(..)
>     ,FrameClause(..)
>     ,TypeName(..)
>     -- * Boilerplate Typedefs
>     ,SelectItemList
>     ,TableRefList
>     ,MaybeExpression
>     ,MaybeBoolExpression
>     ,OnExpr
>     ,ExpressionList
>     ,ExpressionListList
>     ,ExpressionDirectionPair
>     ,ExpressionDirectionPairList
>     ,CaseExpressionListExpressionPair
>     ,CaseExpressionListExpressionPairList
>     ) where
>
> import Database.HsSqlPpp.AstInternals.AstInternal
