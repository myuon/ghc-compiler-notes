`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/libraries/base/GHC/List.hs>`_

====================
libraries/base/GHC/List.hs.rst
====================

Note [Inline FB functions]
~~~~~~~~~~~~~~~~~~~~~~~~~~
After fusion rules successfully fire, we are usually left with one or more calls
to list-producing functions abstracted over cons and nil. Here we call them
FB functions because their names usually end with 'FB'. It's a good idea to
inline FB functions because:

* They are higher-order functions and therefore benefits from inlining.

* When the final consumer is a left fold, inlining the FB functions is the only
  way to make arity expansion to happen. See Note [Left fold via right fold].

For this reason we mark all FB functions INLINE [0]. The [0] phase-specifier
ensures that calls to FB functions can be written back to the original form
when no fusion happens.

Without these inline pragmas, the loop in perf/should_run/T13001 won't be
allocation-free. Also see #13001.
----------------------------------------------------------------------------


Note [scanl rewrite rules]
~~~~~~~~~~~~~~~~~~~~~~~~~~

In most cases, when we rewrite a form to one that can fuse, we try to rewrite it
back to the original form if it does not fuse. For scanl, we do something a
little different. In particular, we rewrite

scanl f a bs

to

build (\c n -> a `c` foldr (scanlFB f c) (constScanl n) bs a)

When build is inlined, this becomes

a : foldr (scanlFB f (:)) (constScanl []) bs a

To rewrite this form back to scanl, we would need a rule that looked like

forall f a bs. a : foldr (scanlFB f (:)) (constScanl []) bs a = scanl f a bs

The problem with this rule is that it has (:) at its head. This would have the
effect of changing the way the inliner looks at (:), not only here but
everywhere.  In most cases, this makes no difference, but in some cases it
causes it to come to a different decision about whether to inline something.
Based on nofib benchmarks, this is bad for performance. Therefore, we instead
match on everything past the :, which is just the tail of scanl.
foldr, foldr1, scanr, and scanr1 are the right-to-left duals of the
above functions.


Note [Fusion for foldrN]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
We arrange that foldr2, foldr3, etc is a good consumer for its first
(left) list argument. Here's how. See below for the second, third
etc list arguments

* The rule "foldr2/left" (active only before phase 1) does this:
     foldr2 k z (build g) ys = g (foldr2_left  k z) (\_ -> z) ys
  thereby fusing away the 'build' on the left argument

* To ensure this rule has a chance to fire, foldr2 has a NOINLINE[1] pragma

There used to be a "foldr2/right" rule, allowing foldr2 to fuse with a build
form on the right. However, this causes trouble if the right list ends in
a bottom that is only avoided by the left list ending at that spot. That is,
foldr2 f z [a,b,c] (d:e:f:_|_), where the right list is produced by a build
form, would cause the foldr2/right rule to introduce bottom. Example:
  zip [1,2,3,4] (unfoldr (\s -> if s > 4 then undefined else Just (s,s+1)) 1)
should produce
  [(1,1),(2,2),(3,3),(4,4)]
but with the foldr2/right rule it would instead produce
  (1,1):(2,2):(3,3):(4,4):_|_



Note [Fusion for zipN/zipWithN]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We arrange that zip, zip3, etc, and zipWith, zipWit3 etc, are all
good consumers for their first (left) argument, and good producers.
Here's how.  See Note [Fusion for foldr2] for why it can't fuse its
second (right) list argument.

NB: Zips for larger tuples are in the List module.

* Rule "zip" (active only before phase 1) rewrites
    zip xs ys = build (\c n -> foldr2 (zipFB c) n xs ys)
  See also Note [Inline FB functions]

.. code-block:: haskell

  Ditto rule "zipWith".

* To give this rule a chance to fire, we give zip a NOLINLINE[1]
  pragma (although since zip is recursive it might not need it)

* Now the rules for foldr2 (see Note [Fusion for foldr2]) may fire,
  or rules that fuse the build-produced output of zip.

* If none of these fire, rule "zipList" (active only in phase 1)
  rewrites the foldr2 call back to zip
     foldr2 (zipFB (:)) []   = zip
  This rule will only fire when build has inlined, which also
  happens in phase 1.

  Ditto rule "zipWithList".
--------------------------------------------

