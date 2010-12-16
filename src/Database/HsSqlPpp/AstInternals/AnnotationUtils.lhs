
This module contains some utilities and generic code for working with
asts and annotations which depend on the ast types.

> {-# LANGUAGE ScopedTypeVariables #-}
>
> module Database.HsSqlPpp.AstInternals.AnnotationUtils
>     (
>      --getStatementAnnotations
>      resetSourcePositions
>     ) where
>
> import Data.Generics
> import Data.Generics.Uniplate.Data
>
> import Database.HsSqlPpp.AstInternals.AstInternal
> import Database.HsSqlPpp.AstInternals.AstAnnotation
>
> -- | Run through the ast and return all the annotations attached to
> --   a Statement node.
> --getStatementAnnotations :: Data a => a -> [Annotation]
> --getStatementAnnotations st =
> --    [getAnnotation s | (s::Statement) <- universeBi st]

 > resetAnnotations :: Data a => a -> a
 > resetAnnotations =
 >     updateAnnotations (const emptyAnnotation)

> resetSourcePositions :: Data a => a -> a
> resetSourcePositions = transformBi resetSp
>   where
>     resetSp :: SourcePosition -> SourcePosition
>     resetSp = const ("",0,0)


transformBi $ \x ->

 > resetAnnotations =
 >     updateAnnotations (const emptyAnnotation)