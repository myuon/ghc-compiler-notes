`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/types/Kind.hs>`_

compiler/types/Kind.hs
======================


Note [Kind Constraint and kind Type]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/types/Kind.hs#L37>`__

The kind Constraint is the kind of classes and other type constraints.
The special thing about types of kind Constraint is that
 * They are displayed with double arrow:
     f :: Ord a => a -> a
 * They are implicitly instantiated at call sites; so the type inference
   engine inserts an extra argument of type (Ord a) at every call site
   to f.

However, once type inference is over, there is *no* distinction between
Constraint and Type. Indeed we can have coercions between the two. Consider
   class C a where
     op :: a -> a
For this single-method class we may generate a newtype, which in turn
generates an axiom witnessing
    C a ~ (a -> a)
so on the left we have Constraint, and on the right we have Type.
See #7451.

Bottom line: although 'Type' and 'Constraint' are distinct TyCons, with
distinct uniques, they are treated as equal at all times except
during type inference.

