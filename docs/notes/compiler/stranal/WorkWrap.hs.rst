`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/stranal/WorkWrap.hs>`_

Note [Don't w/w INLINE things]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It's very important to refrain from w/w-ing an INLINE function (ie one
with a stable unfolding) because the wrapper will then overwrite the
old stable unfolding with the wrapper code.

Furthermore, if the programmer has marked something as INLINE,
we may lose by w/w'ing it.

If the strictness analyser is run twice, this test also prevents
wrappers (which are INLINEd) from being re-done.  (You can end up with
several liked-named Ids bouncing around at the same time---absolute
mischief.)

Notice that we refrain from w/w'ing an INLINE function even if it is
in a recursive group.  It might not be the loop breaker.  (We could
test for loop-breaker-hood, but I'm not sure that ever matters.)



Note [Worker-wrapper for INLINABLE functions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If we have
  {-# INLINABLE f #-}
  f :: Ord a => [a] -> Int -> a
  f x y = ....f....

where f is strict in y, we might get a more efficient loop by w/w'ing
f.  But that would make a new unfolding which would overwrite the old
one! So the function would no longer be INLNABLE, and in particular
will not be specialised at call sites in other modules.

This comes in practice (#6056).

Solution: do the w/w for strictness analysis, but transfer the Stable
unfolding to the *worker*.  So we will get something like this:

  {-# INLINE[0] f #-}
  f :: Ord a => [a] -> Int -> a
  f d x y = case y of I# y' -> fw d x y'

  {-# INLINABLE[0] fw #-}
  fw :: Ord a => [a] -> Int# -> a
  fw d x y' = let y = I# y' in ...f...

How do we "transfer the unfolding"? Easy: by using the old one, wrapped
in work_fn! See CoreUnfold.mkWorkerUnfolding.



Note [Worker-wrapper for NOINLINE functions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We used to disable worker/wrapper for NOINLINE things, but it turns out
this can cause unnecessary reboxing of values. Consider

  {-# NOINLINE f #-}
  f :: Int -> a
  f x = error (show x)

  g :: Bool -> Bool -> Int -> Int
  g True  True  p = f p
  g False True  p = p + 1
  g b     False p = g b True p

the strictness analysis will discover f and g are strict, but because f
has no wrapper, the worker for g will rebox p. So we get

  $wg x y p# =
    let p = I# p# in  -- Yikes! Reboxing!
    case x of
      False ->
        case y of
          False -> $wg False True p#
          True -> +# p# 1#
      True ->
        case y of
          False -> $wg True True p#
          True -> case f p of { }

  g x y p = case p of (I# p#) -> $wg x y p#

Now, in this case the reboxing will float into the True branch, and so
the allocation will only happen on the error path. But it won't float
inwards if there are multiple branches that call (f p), so the reboxing
will happen on every call of g. Disaster.

Solution: do worker/wrapper even on NOINLINE things; but move the
NOINLINE pragma to the worker.

(See #13143 for a real-world example.)

It is crucial that we do this for *all* NOINLINE functions. #10069
demonstrates what happens when we promise to w/w a (NOINLINE) leaf function, but
fail to deliver:

  data C = C Int# Int#

  {-# NOINLINE c1 #-}
  c1 :: C -> Int#
  c1 (C _ n) = n

  {-# NOINLINE fc #-}
  fc :: C -> Int#
  fc c = 2 *# c1 c

Failing to w/w `c1`, but still w/wing `fc` leads to the following code:

  c1 :: C -> Int#
  c1 (C _ n) = n

  $wfc :: Int# -> Int#
  $wfc n = let c = C 0# n in 2 #* c1 c

  fc :: C -> Int#
  fc (C _ n) = $wfc n

Yikes! The reboxed `C` in `$wfc` can't cancel out, so we are in a bad place.
This generalises to any function that derives its strictness signature from
its callees, so we have to make sure that when a function announces particular
strictness properties, we have to w/w them accordingly, even if it means
splitting a NOINLINE function.



Note [Worker activation]
~~~~~~~~~~~~~~~~~~~~~~~~
Follows on from Note [Worker-wrapper for INLINABLE functions]

It is *vital* that if the worker gets an INLINABLE pragma (from the
original function), then the worker has the same phase activation as
the wrapper (or later).  That is necessary to allow the wrapper to
inline into the worker's unfolding: see SimplUtils
Note [Simplifying inside stable unfoldings].

If the original is NOINLINE, it's important that the work inherit the
original activation. Consider

  {-# NOINLINE expensive #-}
  expensive x = x + 1

  f y = let z = expensive y in ...

If expensive's worker inherits the wrapper's activation,
we'll get this (because of the compromise in point (2) of
Note [Wrapper activation])

  {-# NOINLINE[0] $wexpensive #-}
  $wexpensive x = x + 1
  {-# INLINE[0] expensive #-}
  expensive x = $wexpensive x

  f y = let z = expensive y in ...

and $wexpensive will be immediately inlined into expensive, followed by
expensive into f. This effectively removes the original NOINLINE!

Otherwise, nothing is lost by giving the worker the same activation as the
wrapper, because the worker won't have any chance of inlining until the
wrapper does; there's no point in giving it an earlier activation.



Note [Don't w/w inline small non-loop-breaker things]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In general, we refrain from w/w-ing *small* functions, which are not
loop breakers, because they'll inline anyway.  But we must take care:
it may look small now, but get to be big later after other inlining
has happened.  So we take the precaution of adding an INLINE pragma to
any such functions.

I made this change when I observed a big function at the end of
compilation with a useful strictness signature but no w-w.  (It was
small during demand analysis, we refrained from w/w, and then got big
when something was inlined in its rhs.) When I measured it on nofib,
it didn't make much difference; just a few percent improved allocation
on one benchmark (bspt/Euclid.space).  But nothing got worse.

There is an infelicity though.  We may get something like
      f = g val
==>
      g x = case gw x of r -> I# r

      f {- InlineStable, Template = g val -}
      f = case gw x of r -> I# r

The code for f duplicates that for g, without any real benefit. It
won't really be executed, because calls to f will go via the inlining.



Note [Don't CPR join points]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

There's no point in doing CPR on a join point. If the whole function is getting
CPR'd, then the case expression around the worker function will get pushed into
the join point by the simplifier, which will have the same effect that CPR would
have - the result will be returned in an unboxed tuple.

  f z = let join j x y = (x+1, y+1)
        in case z of A -> j 1 2
                     B -> j 2 3

  =>

  f z = case $wf z of (# a, b #) -> (a, b)
  $wf z = case (let join j x y = (x+1, y+1)
                in case z of A -> j 1 2
                             B -> j 2 3) of (a, b) -> (# a, b #)

  =>

  f z = case $wf z of (# a, b #) -> (a, b)
  $wf z = let join j x y = (# x+1, y+1 #)
          in case z of A -> j 1 2
                       B -> j 2 3

Doing CPR on a join point would be tricky anyway, as the worker could not be
a join point because it would not be tail-called. However, doing the *argument*
part of W/W still works for join points, since the wrapper body will make a tail
call:

  f z = let join j x y = x + y
        in ...

  =>

  f z = let join $wj x# y# = x# +# y#
                 j x y = case x of I# x# ->
                         case y of I# y# ->
                         $wj x# y#
        in ...



Note [Wrapper activation]
~~~~~~~~~~~~~~~~~~~~~~~~~
When should the wrapper inlining be active?

1. It must not be active earlier than the current Activation of the
   Id

2. It should be active at some point, despite (1) because of
   Note [Worker-wrapper for NOINLINE functions]

3. For ordinary functions with no pragmas we want to inline the
   wrapper as early as possible (#15056).  Suppose another module
   defines    f x = g x x
   and suppose there is some RULE for (g True True).  Then if we have
   a call (f True), we'd expect to inline 'f' and the RULE will fire.
   But if f is w/w'd (which it might be), we want the inlining to
   occur just as if it hadn't been.

   (This only matters if f's RHS is big enough to w/w, but small
   enough to inline given the call site, but that can happen.)

4. We do not want to inline the wrapper before specialisation.
         module Foo where
           f :: Num a => a -> Int -> a
           f n 0 = n              -- Strict in the Int, hence wrapper
           f n x = f (n+n) (x-1)

           g :: Int -> Int
           g x = f x x            -- Provokes a specialisation for f

         module Bar where
           import Foo

           h :: Int -> Int
           h x = f 3 x

   In module Bar we want to give specialisations a chance to fire
   before inlining f's wrapper.

Reminder: Note [Don't w/w INLINE things], so we don't need to worry
          about INLINE things here.

Conclusion:
  - If the user said NOINLINE[n], respect that
  - If the user said NOINLINE, inline the wrapper as late as
    poss (phase 0). This is a compromise driven by (2) above
  - Otherwise inline wrapper in phase 2.  That allows the
    'gentle' simplification pass to apply specialisation rules

Historical note: At one stage I tried making the wrapper inlining
always-active, and that had a very bad effect on nofib/imaginary/x2n1;
a wrapper was inlined before the specialisation fired.



Note [Wrapper NoUserInline]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
The use an inl_inline of NoUserInline on the wrapper distinguishes
this pragma from one that was given by the user. In particular, CSE
will not happen if there is a user-specified pragma, but should happen
for w/w’ed things (#14186).


Note [Zapping DmdEnv after Demand Analyzer]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In the worker-wrapper pass we zap the DmdEnv.  Why?
 (a) it is never used again
 (b) it wastes space
 (c) it becomes incorrect as things are cloned, because
     we don't push the substitution into it

Why here?
 * Because we don’t want to do it in the Demand Analyzer, as we never know
   there when we are doing the last pass.
 * We want them to be still there at the end of DmdAnal, so that
   -ddump-str-anal contains them.
 * We don’t want a second pass just for that.
 * WorkWrap looks at all bindings anyway.

We also need to do it in TidyCore.tidyLetBndr to clean up after the
final, worker/wrapper-less run of the demand analyser (see
Note [Final Demand Analyser run] in DmdAnal).



Note [Zapping Used Once info in WorkWrap]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In the worker-wrapper pass we zap the used once info in demands and in
strictness signatures.

Why?
 * The simplifier may happen to transform code in a way that invalidates the
   data (see #11731 for an example).
 * It is not used in later passes, up to code generation.

So as the data is useless and possibly wrong, we want to remove it. The most
convenient place to do that is the worker wrapper phase, as it runs after every
run of the demand analyser besides the very last one (which is the one where we
want to _keep_ the info for the code generator).

We do not do it in the demand analyser for the same reasons outlined in
Note [Zapping DmdEnv after Demand Analyzer] above.
-------------------


Note [Demand on the worker]
~~~~~~~~~~~~~~~~~~~~~~~~~~~

If the original function is called once, according to its demand info, then
so is the worker. This is important so that the occurrence analyser can
attach OneShot annotations to the worker’s lambda binders.


Example:

  -- Original function
  f [Demand=<L,1*C1(U)>] :: (a,a) -> a
  f = \p -> ...

  -- Wrapper
  f [Demand=<L,1*C1(U)>] :: a -> a -> a
  f = \p -> case p of (a,b) -> $wf a b

  -- Worker
  $wf [Demand=<L,1*C1(C1(U))>] :: Int -> Int
  $wf = \a b -> ...

We need to check whether the original function is called once, with
sufficiently many arguments. This is done using saturatedByOneShots, which
takes the arity of the original function (resp. the wrapper) and the demand on
the original function.

The demand on the worker is then calculated using mkWorkerDemand, and always of
the form [Demand=<L,1*(C1(...(C1(U))))>]




Note [Do not split void functions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this rather common form of binding:
        $j = \x:Void# -> ...no use of x...

Since x is not used it'll be marked as absent.  But there is no point
in w/w-ing because we'll simply add (\y:Void#), see WwLib.mkWorerArgs.

If x has a more interesting type (eg Int, or Int#), there *is* a point
in w/w so that we don't pass the argument at all.



Note [Thunk splitting]
~~~~~~~~~~~~~~~~~~~~~~
Suppose x is used strictly (never mind whether it has the CPR
property).

      let
        x* = x-rhs
      in body

splitThunk transforms like this:

      let
        x* = case x-rhs of { I# a -> I# a }
      in body

Now simplifier will transform to

      case x-rhs of
        I# a -> let x* = I# a
                in body

which is what we want. Now suppose x-rhs is itself a case:

        x-rhs = case e of { T -> I# a; F -> I# b }

The join point will abstract over a, rather than over (which is
what would have happened before) which is fine.

Notice that x certainly has the CPR property now!

In fact, splitThunk uses the function argument w/w splitting
function, so that if x's demand is deeper (say U(U(L,L),L))
then the splitting will go deeper too.
See Note [Thunk splitting]
splitThunk converts the *non-recursive* binding
     x = e
into
     x = let x = e
         in case x of
              I# y -> let x = I# y in x }
See comments above. Is it not beautifully short?
Moreover, it works just as well when there are
several binders, and if the binders are lifted
E.g.     x = e
    -->  x = let x = e in
             case x of (a,b) -> let x = (a,b)  in x

