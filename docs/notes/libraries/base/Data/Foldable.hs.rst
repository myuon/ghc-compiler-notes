`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/libraries/base/Data/Foldable.hs>`_

libraries/base/Data/Foldable.hs
===============================


Note [List fusion and continuations in 'c']
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/libraries/base/Data/Foldable.hs#L693>`__

Suppose we define
  mapM_ f = foldr ((>>) . f) (return ())
(this is the way it used to be).

Now suppose we want to optimise the call

::

  mapM_ <big> (build g)
    where
  g c n = ...(c x1 y1)...(c x2 y2)....n...

GHC used to proceed like this:

::

  mapM_ <big> (build g)

::

  = { Definition of mapM_ }
    foldr ((>>) . <big>) (return ()) (build g)

::

  = { foldr/build rule }
    g ((>>) . <big>) (return ())

::

  = { Inline g }
    let c = (>>) . <big>
        n = return ()
    in ...(c x1 y1)...(c x2 y2)....n...

The trouble is that `c`, being big, will not be inlined.  And that can
be absolutely terrible for performance, as we saw in #8763.

It's much better to define

::

  mapM_ f = foldr c (return ())
    where
      c x k = f x >> k
      {-# INLINE c #-}

Now we get
  mapM_ <big> (build g)

::

  = { inline mapM_ }
    foldr c (return ()) (build g)
      where c x k = f x >> k
            {-# INLINE c #-}
            f = <big>

Notice that `f` does not inline into the RHS of `c`,
because the INLINE pragma stops it; see
Note [Simplifying inside stable unfoldings] in SimplUtils.
Continuing:

::

  = { foldr/build rule }
    g c (return ())
      where ...
         c x k = f x >> k
         {-# INLINE c #-}
            f = <big>

::

  = { inline g }
    ...(c x1 y1)...(c x2 y2)....n...
      where c x k = f x >> k
            {-# INLINE c #-}
            f = <big>
            n = return ()

::

      Now, crucially, `c` does inline

::

  = { inline c }
    ...(f x1 >> y1)...(f x2 >> y2)....n...
      where f = <big>
            n = return ()

And all is well!  The key thing is that the fragment
`(f x1 >> y1)` is inlined into the body of the builder
`g`.



Note [maximumBy/minimumBy space usage]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/libraries/base/Data/Foldable.hs#L771>`__

When the type signatures of maximumBy and minimumBy were generalized to work
over any Foldable instance (instead of just lists), they were defined using
foldr1. This was problematic for space usage, as the semantics of maximumBy
and minimumBy essentially require that they examine every element of the
data structure. Using foldr1 to examine every element results in space usage
proportional to the size of the data structure. For the common case of lists,
this could be particularly bad (see #10830).

For the common case of lists, switching the implementations of maximumBy and
minimumBy to foldl1 solves the issue, as GHC's strictness analysis can then
make these functions only use O(1) stack space. It is perhaps not the optimal
way to fix this problem, as there are other conceivable data structures
(besides lists) which might benefit from specialized implementations for
maximumBy and minimumBy (see
https://gitlab.haskell.org/ghc/ghc/issues/10830#note_129843 for a further
discussion). But using foldl1 is at least always better than using foldr1, so
GHC has chosen to adopt that approach for now.

