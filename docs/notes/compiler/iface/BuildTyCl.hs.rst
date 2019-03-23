`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/iface/BuildTyCl.hs>`_

Note [Class newtypes and equality predicates]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
        class (a ~ F b) => C a b where
          op :: a -> b

We cannot represent this by a newtype, even though it's not
existential, because there are two value fields (the equality
predicate and op. See #2238

Moreover,
          class (a ~ F b) => C a b where {}
Here we can't use a newtype either, even though there is only
one field, because equality predicates are unboxed, and classes
are boxed.

