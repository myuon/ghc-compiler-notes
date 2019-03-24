`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/libraries/base/GHC/Base.hs>`_

Note [Depend on GHC.Integer]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The Integer type is special because TidyPgm uses
GHC.Integer.Type.mkInteger to construct Integer literal values
Currently it reads the interface file whether or not the current
module *has* any Integer literals, so it's important that
GHC.Integer.Type (in package integer-gmp or integer-simple) is
compiled before any other module.  (There's a hack in GHC to disable
this for packages ghc-prim, integer-gmp, integer-simple, which aren't
allowed to contain any Integer literals.)

Likewise we implicitly need Integer when deriving things like Eq
instances.

The danger is that if the build system doesn't know about the dependency
on Integer, it'll compile some base module before GHC.Integer.Type,
resulting in:
  Failed to load interface for ‘GHC.Integer.Type’
    There are files missing in the ‘integer-gmp’ package,

Bottom line: we make GHC.Base depend on GHC.Integer; and everything
else either depends on GHC.Base, or does not have NoImplicitPrelude
(and hence depends on Prelude).



Note [Depend on GHC.Tuple]
~~~~~~~~~~~~~~~~~~~~~~~~~~
Similarly, tuple syntax (or ()) creates an implicit dependency on
GHC.Tuple, so we use the same rule as for Integer --- see Note [Depend on
GHC.Integer] --- to explain this to the build system.  We make GHC.Base
depend on GHC.Tuple, and everything else depends on GHC.Base or Prelude.



Note [Depend on GHC.Natural]
~~~~~~~~~~~~~~~~~~~~~~~~~~
Similar to GHC.Integer.


Note [Recursive bindings for Applicative/Monad]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The original Applicative/Monad proposal stated that after
implementation, the designated implementation of (>>) would become

.. code-block:: haskell

  (>>) :: forall a b. m a -> m b -> m b
  (>>) = (*>)

by default. You might be inclined to change this to reflect the stated
proposal, but you really shouldn't! Why? Because people tend to define
such instances the /other/ way around: in particular, it is perfectly
legitimate to define an instance of Applicative (*>) in terms of (>>),
which would lead to an infinite loop for the default implementation of
Monad! And people do this in the wild.

This turned into a nasty bug that was tricky to track down, and rather
than eliminate it everywhere upstream, it's easier to just retain the
original default.



Note [The rules for map]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
The rules for map work like this.

* Up to (but not including) phase 1, we use the "map" rule to
  rewrite all saturated applications of map with its build/fold
  form, hoping for fusion to happen.

.. code-block:: haskell

  In phase 1 and 0, we switch off that rule, inline build, and
  switch on the "mapList" rule, which rewrites the foldr/mapFB
  thing back into plain map.

.. code-block:: haskell

  It's important that these two rules aren't both active at once
  (along with build's unfolding) else we'd get an infinite loop
  in the rules.  Hence the activation control below.

* This same pattern is followed by many other functions:
  e.g. append, filter, iterate, repeat, etc. in GHC.List

.. code-block:: haskell

  See also Note [Inline FB functions] in GHC.List

* The "mapFB" rule optimises compositions of map

* The "mapFB/id" rule gets rid of 'map id' calls.
  You might think that (mapFB c id) will turn into c simply
  when mapFB is inlined; but before that happens the "mapList"
  rule turns
     (foldr (mapFB (:) id) [] a
  back into
     map id
  Which is not very clever.

* Any similarity to the Functor laws for [] is expected.

