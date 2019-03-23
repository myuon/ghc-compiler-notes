Note [Inlining and hs-boot files]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this example (#10083, #12789):

    ---------- RSR.hs-boot ------------
    module RSR where
      data RSR
      eqRSR :: RSR -> RSR -> Bool

    ---------- SR.hs ------------
    module SR where
      import {-# SOURCE #-} RSR
      data SR = MkSR RSR
      eqSR (MkSR r1) (MkSR r2) = eqRSR r1 r2

    ---------- RSR.hs ------------
    module RSR where
      import SR
      data RSR = MkRSR SR -- deriving( Eq )
      eqRSR (MkRSR s1) (MkRSR s2) = (eqSR s1 s2)
      foo x y = not (eqRSR x y)

When compiling RSR we get this code

    RSR.eqRSR :: RSR -> RSR -> Bool
    RSR.eqRSR = \ (ds1 :: RSR.RSR) (ds2 :: RSR.RSR) ->
                case ds1 of _ { RSR.MkRSR s1 ->
                case ds2 of _ { RSR.MkRSR s2 ->
                SR.eqSR s1 s2 }}

    RSR.foo :: RSR -> RSR -> Bool
    RSR.foo = \ (x :: RSR) (y :: RSR) -> not (RSR.eqRSR x y)

Now, when optimising foo:
    Inline eqRSR (small, non-rec)
    Inline eqSR  (small, non-rec)
but the result of inlining eqSR from SR is another call to eqRSR, so
everything repeats.  Neither eqSR nor eqRSR are (apparently) loop
breakers.

Solution: in the unfolding of eqSR in SR.hi, replace `eqRSR` in SR
with `noinline eqRSR`, so that eqRSR doesn't get inlined.  This means
that when GHC inlines `eqSR`, it will not also inline `eqRSR`, exactly
as would have been the case if `foo` had been defined in SR.hs (and
marked as a loop-breaker).

But how do we arrange for this to happen?  There are two ingredients:

    1. When we serialize out unfoldings to IfaceExprs (toIfaceVar),
    for every variable reference we see if we are referring to an
    'Id' that came from an hs-boot file.  If so, we add a `noinline`
    to the reference.

    2. But how do we know if a reference came from an hs-boot file
    or not?  We could record this directly in the 'IdInfo', but
    actually we deduce this by looking at the unfolding: 'Id's
    that come from boot files are given a special unfolding
    (upon typechecking) 'BootUnfolding' which say that there is
    no unfolding, and the reason is because the 'Id' came from
    a boot file.

Here is a solution that doesn't work: when compiling RSR,
add a NOINLINE pragma to every function exported by the boot-file
for RSR (if it exists).  Doing so makes the bootstrapped GHC itself
slower by 8% overall (on #9872a-d, and T1969: the reason
is that these NOINLINE'd functions now can't be profitably inlined
outside of the hs-boot loop.

