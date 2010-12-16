
This is the public module to the annotation data types and support
functions (not including those that depend on the ast data types).

> {- | Contains the annotation data types and a few auxiliary functions.
> -}
>
> module Database.HsSqlPpp.Annotation
>     (
>      -- * Annotation data types
>      Annotation(..)
>     ,SourcePosition
>     ,StatementType
>     ,getAnnotation
>     ,updateAnnotations
>     ,updateAnnotation
>     ,getAnnotations
>     ,emptyAnnotation
>     ,resetSourcePositions
>     ) where
>
> import Database.HsSqlPpp.AstInternals.AstAnnotation
> import Database.HsSqlPpp.AstInternals.AnnotationUtils
