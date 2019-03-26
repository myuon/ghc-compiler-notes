`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcDerivUtils.hs>`_

compiler/typecheck/TcDerivUtils.hs
==================================


Note [Deriving and unused record selectors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcDerivUtils.hs#L475>`__

Consider this (see #13919):

::

  module Main (main) where

::

  data Foo = MkFoo {bar :: String} deriving Show

::

  main :: IO ()
  main = print (Foo "hello")

Strictly speaking, the record selector `bar` is unused in this module, since
neither `main` nor the derived `Show` instance for `Foo` mention `bar`.
However, the behavior of `main` is affected by the presence of `bar`, since
it will print different output depending on whether `MkFoo` is defined using
record selectors or not. Therefore, we do not to issue a
"Defined but not used: ‘bar’" warning for this module, since removing `bar`
changes the program's behavior. This is the reason behind the [Name] part of
the return type of `hasStockDeriving`—it tracks all of the record selector
`Name`s for which -Wunused-binds should be suppressed.

Currently, the only three stock derived classes that require this are Read,
Show, and Generic, as their derived code all depend on the record selectors
of the derived data type's constructors.

See also Note [Newtype deriving and unused constructors] in TcDeriv for
another example of a similar trick.



Note [Deriving any class]
~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcDerivUtils.hs#L891>`__

Classic uses of a deriving clause, or a standalone-deriving declaration, are
for:
  * a stock class like Eq or Show, for which GHC knows how to generate
    the instance code
  * a newtype, via the mechanism enabled by GeneralizedNewtypeDeriving

The DeriveAnyClass extension adds a third way to derive instances, based on
empty instance declarations.

The canonical use case is in combination with GHC.Generics and default method
signatures. These allow us to have instance declarations being empty, but still
useful, e.g.

::

  data T a = ...blah..blah... deriving( Generic )
  instance C a => C (T a)  -- No 'where' clause

where C is some "random" user-defined class.

This boilerplate code can be replaced by the more compact

::

  data T a = ...blah..blah... deriving( Generic, C )

if DeriveAnyClass is enabled.

This is not restricted to Generics; any class can be derived, simply giving
rise to an empty instance.

Unfortunately, it is not clear how to determine the context (when using a
deriving clause; in standalone deriving, the user provides the context).
GHC uses the same heuristic for figuring out the class context that it uses for
Eq in the case of *-kinded classes, and for Functor in the case of
* -> *-kinded classes. That may not be optimal or even wrong. But in such
cases, standalone deriving can still be used.



Note [Check that the type variable is truly universal]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcDerivUtils.hs#L927>`__

For Functor and Traversable instances, we must check that the *last argument*
of the type constructor is used truly universally quantified.  Example

::

   data T a b where
     T1 :: a -> b -> T a b      -- Fine! Vanilla H-98
     T2 :: b -> c -> T a b      -- Fine! Existential c, but we can still map over 'b'
     T3 :: b -> T Int b         -- Fine! Constraint 'a', but 'b' is still polymorphic
     T4 :: Ord b => b -> T a b  -- No!  'b' is constrained
     T5 :: b -> T b b           -- No!  'b' is constrained
     T6 :: T a (b,b)            -- No!  'b' is constrained

Notice that only the first of these constructors is vanilla H-98. We only
need to take care about the last argument (b in this case).  See #8678.
Eg. for T1-T3 we can write

::

     fmap f (T1 a b) = T1 a (f b)
     fmap f (T2 b c) = T2 (f b) c
     fmap f (T3 x)   = T3 (f x)

We need not perform these checks for Foldable instances, however, since
functions in Foldable can only consume existentially quantified type variables,
rather than produce them (as is the case in Functor and Traversable functions.)
As a result, T can have a derived Foldable instance:

::

    foldr f z (T1 a b) = f b z
    foldr f z (T2 b c) = f b z
    foldr f z (T3 x)   = f x z
    foldr f z (T4 x)   = f x z
    foldr f z (T5 x)   = f x z
    foldr _ z T6       = z

See Note [DeriveFoldable with ExistentialQuantification] in TcGenFunctor.

For Functor and Traversable, we must take care not to let type synonyms
unfairly reject a type for not being truly universally quantified. An
example of this is:

::

    type C (a :: Constraint) b = a
    data T a b = C (Show a) b => MkT b

Here, the existential context (C (Show a) b) does technically mention the last
type variable b. But this is OK, because expanding the type synonym C would
give us the context (Show a), which doesn't mention b. Therefore, we must make
sure to expand type synonyms before performing this check. Not doing so led to
#13813.

