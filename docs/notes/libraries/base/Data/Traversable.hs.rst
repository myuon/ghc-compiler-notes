`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/libraries/base/Data/Traversable.hs>`_

libraries/base/Data/Traversable.hs
==================================


Note [Inline default methods]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/libraries/base/Data/Traversable.hs#L174>`__

Consider

::

   class ... => Traversable t where
       ...
       mapM :: Monad m => (a -> m b) -> t a -> m (t b)
       mapM = traverse   -- Default method

::

   instance Traversable [] where
       {-# INLINE traverse #-}
       traverse = ...code for traverse on lists ...

This gives rise to a list-instance of mapM looking like this

::

  $fTraversable[]_$ctraverse = ...code for traverse on lists...
       {-# INLINE $fTraversable[]_$ctraverse #-}
  $fTraversable[]_$cmapM    = $fTraversable[]_$ctraverse

Now the $ctraverse obediently inlines into the RHS of $cmapM, /but/
that's all!  We get

::

  $fTraversable[]_$cmapM = ...code for traverse on lists...

with NO INLINE pragma!  This happens even though 'traverse' had an
INLINE pragma because the author knew it should be inlined pretty
vigorously.

Indeed, it turned out that the rhs of $cmapM was just too big to
inline, so all uses of mapM on lists used a terribly inefficient
dictionary-passing style, because of its 'Monad m =>' type.  Disaster!

Solution: add an INLINE pragma on the default method:

   class ... => Traversable t where
       ...
       mapM :: Monad m => (a -> m b) -> t a -> m (t b)
       {-# INLINE mapM #-}     -- VERY IMPORTANT!
       mapM = traverse
instances for Prelude types

