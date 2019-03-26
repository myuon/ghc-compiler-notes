`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/cmm/CmmLayoutStack.hs>`_

compiler/cmm/CmmLayoutStack.hs
==============================


Note [Always false stack check]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/cmm/CmmLayoutStack.hs#L943>`__

We can optimise stack checks of the form

::

  if ((Sp + x) - y < SpLim) then .. else ..

where are non-negative integer byte offsets.  Since we know that
SpLim <= Sp (remember the stack grows downwards), this test must
yield False if (x >= y), so we can rewrite the comparison to False.
A subsequent sinking pass will later drop the dead code.
Optimising this away depends on knowing that SpLim <= Sp, so it is
really the job of the stack layout algorithm, hence we do it now.

The control flow optimiser may negate a conditional to increase
the likelihood of a fallthrough if the branch is not taken.  But
not every conditional is inverted as the control flow optimiser
places some requirements on the predecessors of both branch targets.
So we better look for the inverted comparison too.

