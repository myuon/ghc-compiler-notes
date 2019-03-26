`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/simplCore/SimplUtils.hs>`_

compiler/simplCore/SimplUtils.hs
================================


Note [StaticEnv invariant]
~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/simplCore/SimplUtils.hs#L175>`__

We pair up an InExpr or InAlts with a StaticEnv, which establishes the
lexical scope for that InExpr.  When we simplify that InExpr/InAlts, we
use
  - Its captured StaticEnv
  - Overriding its InScopeSet with the larger one at the
    simplification point.

Why override the InScopeSet?  Example:
      (let y = ey in f) ex
By the time we simplify ex, 'y' will be in scope.

However the InScopeSet in the StaticEnv is not irrelevant: it should
include all the free vars of applying the substitution to the InExpr.
Reason: contHoleType uses perhapsSubstTy to apply the substitution to
the expression, and that (rightly) gives ASSERT failures if the InScopeSet
isn't big enough.



Note [DupFlag invariants]
~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/simplCore/SimplUtils.hs#L194>`__

In both (ApplyToVal dup _ env k)
   and  (Select dup _ _ env k)
the following invariants hold

::

  (a) if dup = OkToDup, then continuation k is also ok-to-dup
  (b) if dup = OkToDup or Simplified, the subst-env is empty
      (and and hence no need to re-simplify)



Note [The hole type in ApplyToTy]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/simplCore/SimplUtils.hs#L228>`__

The sc_hole_ty field of ApplyToTy records the type of the "hole" in the
continuation.  It is absolutely necessary to compute contHoleType, but it is
not used for anything else (and hence may not be evaluated).

Why is it necessary for contHoleType?  Consider the continuation
     ApplyToType Int (Stop Int)
corresponding to
     (<hole> @Int) :: Int
What is the type of <hole>?  It could be (forall a. Int) or (forall a. a),
and there is no way to know which, so we must record it.

In a chain of applications  (f @t1 @t2 @t3) we'll lazily compute exprType
for (f @t1) and (f @t1 @t2), which is potentially non-linear; but it probably
doesn't matter because we'll never compute them all.



Note [Do not expose strictness if sm_inline=False]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/simplCore/SimplUtils.hs#L543>`__

#15163 showed a case in which we had

::

  {-# INLINE [1] zip #-}
  zip = undefined

::

  {-# RULES "foo" forall as bs. stream (zip as bs) = ..blah... #-}

If we expose zip's bottoming nature when simplifing the LHS of the
RULE we get
  {-# RULES "foo" forall as bs.
                   stream (case zip of {}) = ..blah... #-}
discarding the arguments to zip.  Usually this is fine, but on the
LHS of a rule it's not, because 'as' and 'bs' are now not bound on
the LHS.

This is a pretty pathalogical example, so I'm not losing sleep over
it, but the simplest solution was to check sm_inline; if it is False,
which it is on the LHS of a rule (see updModeForRules), then don't
make use of the strictness info for the function.



Note [Interesting call context]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/simplCore/SimplUtils.hs#L574>`__

We want to avoid inlining an expression where there can't possibly be
any gain, such as in an argument position.  Hence, if the continuation
is interesting (eg. a case scrutinee, application etc.) then we
inline, otherwise we don't.

Previously some_benefit used to return True only if the variable was
applied to some value arguments.  This didn't work:

::

        let x = _coerce_ (T Int) Int (I# 3) in
        case _coerce_ Int (T Int) x of
                I# y -> ....

we want to inline x, but can't see that it's a constructor in a case
scrutinee position, and some_benefit is False.

Another example:

dMonadST = _/\_ t -> :Monad (g1 _@_ t, g2 _@_ t, g3 _@_ t)

....  case dMonadST _@_ x0 of (a,b,c) -> ....

we'd really like to inline dMonadST here, but we *don't* want to
inline if the case expression is just

::

        case x of y { DEFAULT -> ... }

since we can just eliminate this case instead (x is in WHNF).  Similar
applies when x is bound to a lambda expression.  Hence
contIsInteresting looks for case expressions with just a single
default case.



Note [No case of case is boring]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/simplCore/SimplUtils.hs#L607>`__

If we see
   case f x of <alts>

we'd usually treat the context as interesting, to encourage 'f' to
inline.  But if case-of-case is off, it's really not so interesting
after all, because we are unlikely to be able to push the case
expression into the branches of any case in f's unfolding.  So, to
reduce unnecessary code expansion, we just make the context look boring.
This made a small compile-time perf improvement in perf/compiler/T6048,
and it looks plausible to me.



Note [Interesting arguments]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/simplCore/SimplUtils.hs#L697>`__

An argument is interesting if it deserves a discount for unfoldings
with a discount in that argument position.  The idea is to avoid
unfolding a function that is applied only to variables that have no
unfolding (i.e. they are probably lambda bound): f x y z There is
little point in inlining f here.

Generally, *values* (like (C a b) and (\x.e)) deserve discounts.  But
we must look through lets, eg (let x = e in C a b), because the let will
float, exposing the value, if we inline.  That makes it different to
exprIsHNF.

Before 2009 we said it was interesting if the argument had *any* structure
at all; i.e. (hasSomeUnfolding v).  But does too much inlining; see #3016.

But we don't regard (f x y) as interesting, unless f is unsaturated.
If it's saturated and f hasn't inlined, then it's probably not going
to now!



Note [Conlike is interesting]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/simplCore/SimplUtils.hs#L717>`__

Consider
        f d = ...((*) d x y)...
        ... f (df d')...
where df is con-like. Then we'd really like to inline 'f' so that the
rule for (*) (df d) can fire.  To do this
  a) we give a discount for being an argument of a class-op (eg (*) d)
  b) we say that a con-like argument (eg (df d)) is interesting



Note [Simplifying rules]
~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/simplCore/SimplUtils.hs#L819>`__

When simplifying a rule LHS, refrain from /any/ inlining or applying
of other RULES.

Doing anything to the LHS is plain confusing, because it means that what the
rule matches is not what the user wrote. c.f. #10595, and #10528.
Moreover, inlining (or applying rules) on rule LHSs risks introducing
Ticks into the LHS, which makes matching trickier. #10665, #10745.

Doing this to either side confounds tools like HERMIT, which seek to reason
about and apply the RULES as originally written. See #10829.



Note [No eta expansion in stable unfoldings]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/simplCore/SimplUtils.hs#L832>`__

If we have a stable unfolding

::

  f :: Ord a => a -> IO ()
  -- Unfolding template
  --    = /\a \(d:Ord a) (x:a). bla

we do not want to eta-expand to

::

  f :: Ord a => a -> IO ()
  -- Unfolding template
  --    = (/\a \(d:Ord a) (x:a) (eta:State#). bla eta) |> co

because not specialisation of the overloading doesn't work properly
(see Note [Specialisation shape] in Specialise), #9509.

So we disable eta-expansion in stable unfoldings.



Note [Inlining in gentle mode]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/simplCore/SimplUtils.hs#L851>`__

Something is inlined if
   (i)   the sm_inline flag is on, AND
   (ii)  the thing has an INLINE pragma, AND
   (iii) the thing is inlinable in the earliest phase.

Example of why (iii) is important:
  {-# INLINE [~1] g #-}
  g = ...

::

  {-# INLINE f #-}
  f x = g (g x)

If we were to inline g into f's inlining, then an importing module would
never be able to do
        f e --> g (g e) ---> RULE fires
because the stable unfolding for f has had g inlined into it.

On the other hand, it is bad not to do ANY inlining into an
stable unfolding, because then recursive knots in instance declarations
don't get unravelled.

However, *sometimes* SimplGently must do no call-site inlining at all
(hence sm_inline = False).  Before full laziness we must be careful
not to inline wrappers, because doing so inhibits floating
    e.g. ...(case f x of ...)...
    ==> ...(case (case x of I# x# -> fw x#) of ...)...
    ==> ...(case x of I# x# -> case fw x# of ...)...
and now the redex (f x) isn't floatable any more.

The no-inlining thing is also important for Template Haskell.  You might be
compiling in one-shot mode with -O2; but when TH compiles a splice before
running it, we don't want to use -O2.  Indeed, we don't want to inline
anything, because the byte-code interpreter might get confused about
unboxed tuples and suchlike.



Note [Simplifying inside stable unfoldings]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/simplCore/SimplUtils.hs#L888>`__

We must take care with simplification inside stable unfoldings (which come from
INLINE pragmas).

First, consider the following example
        let f = \pq -> BIG
        in
        let g = \y -> f y y
            {-# INLINE g #-}
        in ...g...g...g...g...g...
Now, if that's the ONLY occurrence of f, it might be inlined inside g,
and thence copied multiple times when g is inlined. HENCE we treat
any occurrence in a stable unfolding as a multiple occurrence, not a single
one; see OccurAnal.addRuleUsage.

Second, we do want *do* to some modest rules/inlining stuff in stable
unfoldings, partly to eliminate senseless crap, and partly to break
the recursive knots generated by instance declarations.

However, suppose we have
        {-# INLINE <act> f #-}
        f = <rhs>
meaning "inline f in phases p where activation <act>(p) holds".
Then what inlinings/rules can we apply to the copy of <rhs> captured in
f's stable unfolding?  Our model is that literally <rhs> is substituted for
f when it is inlined.  So our conservative plan (implemented by
updModeForStableUnfoldings) is this:

::

  -------------------------------------------------------------
  When simplifying the RHS of a stable unfolding, set the phase
  to the phase in which the stable unfolding first becomes active
  -------------------------------------------------------------

That ensures that

::

  a) Rules/inlinings that *cease* being active before p will
     not apply to the stable unfolding, consistent with it being
     inlined in its *original* form in phase p.

::

  b) Rules/inlinings that only become active *after* p will
     not apply to the stable unfolding, again to be consistent with
     inlining the *original* rhs in phase p.

For example,
        {-# INLINE f #-}
        f x = ...g...

::

        {-# NOINLINE [1] g #-}
        g y = ...

        {-# RULE h g = ... #-}
Here we must not inline g into f's RHS, even when we get to phase 0,
because when f is later inlined into some other module we want the
rule for h to fire.

Similarly, consider
        {-# INLINE f #-}
        f x = ...g...

        g y = ...
and suppose that there are auto-generated specialisations and a strictness
wrapper for g.  The specialisations get activation AlwaysActive, and the
strictness wrapper get activation (ActiveAfter 0).  So the strictness
wrepper fails the test and won't be inlined into f's stable unfolding. That
means f can inline, expose the specialised call to g, so the specialisation
rules can fire.

A note about wrappers
~~~~~~~~~~~~~~~~~~~~~
It's also important not to inline a worker back into a wrapper.
A wrapper looks like
        wraper = inline_me (\x -> ...worker... )
Normally, the inline_me prevents the worker getting inlined into
the wrapper (initially, the worker's only call site!).  But,
if the wrapper is sure to be called, the strictness analyser will
mark it 'demanded', so when the RHS is simplified, it'll get an ArgOf
continuation.



Note [pre/postInlineUnconditionally in gentle mode]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/simplCore/SimplUtils.hs#L1081>`__

Even in gentle mode we want to do preInlineUnconditionally.  The
reason is that too little clean-up happens if you don't inline
use-once things.  Also a bit of inlining is *good* for full laziness;
it can expose constant sub-expressions.  Example in
spectral/mandel/Mandel.hs, where the mandelset function gets a useful
let-float if you inline windowToViewport

However, as usual for Gentle mode, do not inline things that are
inactive in the initial stages.  See Note [Gentle mode].



Note [Stable unfoldings and preInlineUnconditionally]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/simplCore/SimplUtils.hs#L1093>`__

Surprisingly, do not pre-inline-unconditionally Ids with INLINE pragmas!
Example

::

   {-# INLINE f #-}
   f :: Eq a => a -> a
   f x = ...

::

   fInt :: Int -> Int
   fInt = f Int dEqInt

::

   ...fInt...fInt...fInt...

Here f occurs just once, in the RHS of fInt. But if we inline it there
it might make fInt look big, and we'll lose the opportunity to inline f
at each of fInt's call sites.  The INLINE pragma will only inline when
the application is saturated for exactly this reason; and we don't
want PreInlineUnconditionally to second-guess it.  A live example is
#3736.
    c.f. Note [Stable unfoldings and postInlineUnconditionally]

NB: if the pragma is INLINEABLE, then we don't want to behave in
this special way -- an INLINEABLE pragma just says to GHC "inline this
if you like".  But if there is a unique occurrence, we want to inline
the stable unfolding, not the RHS.



Note [Top-level bottoming Ids]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/simplCore/SimplUtils.hs#L1120>`__

Don't inline top-level Ids that are bottoming, even if they are used just
once, because FloatOut has gone to some trouble to extract them out.
Inlining them won't make the program run faster!



Note [Do not inline CoVars unconditionally]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/simplCore/SimplUtils.hs#L1126>`__

Coercion variables appear inside coercions, and the RHS of a let-binding
is a term (not a coercion) so we can't necessarily inline the latter in
the former.



Note [Top level and postInlineUnconditionally]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/simplCore/SimplUtils.hs#L1339>`__

We don't do postInlineUnconditionally for top-level things (even for
ones that are trivial):

  * Doing so will inline top-level error expressions that have been
    carefully floated out by FloatOut.  More generally, it might
    replace static allocation with dynamic.

  * Even for trivial expressions there's a problem.  Consider
      {-# RULE "foo" forall (xs::[T]). reverse xs = ruggle xs #-}
      blah xs = reverse xs
      ruggle = sort
    In one simplifier pass we might fire the rule, getting
      blah xs = ruggle xs
    but in *that* simplifier pass we must not do postInlineUnconditionally
    on 'ruggle' because then we'll have an unbound occurrence of 'ruggle'

::

    If the rhs is trivial it'll be inlined by callSiteInline, and then
    the binding will be dead and discarded by the next use of OccurAnal

  * There is less point, because the main goal is to get rid of local
    bindings used in multiple case branches.

  * The inliner should inline trivial things at call sites anyway.

  * The Id might be exported.  We could check for that separately,
    but since we aren't going to postInlineUnconditionally /any/
    top-level bindings, we don't need to test.



Note [Stable unfoldings and postInlineUnconditionally]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/simplCore/SimplUtils.hs#L1369>`__

Do not do postInlineUnconditionally if the Id has a stable unfolding,
otherwise we lose the unfolding.  Example

::

     -- f has stable unfolding with rhs (e |> co)
     --   where 'e' is big
     f = e |> co

Then there's a danger we'll optimise to

::

     f' = e
     f = f' |> co

and now postInlineUnconditionally, losing the stable unfolding on f.  Now f'
won't inline because 'e' is too big.

::

    c.f. Note [Stable unfoldings and preInlineUnconditionally]



Note [Eta expanding lambdas]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/simplCore/SimplUtils.hs#L1447>`__

In general we *do* want to eta-expand lambdas. Consider
   f (\x -> case x of (a,b) -> \s -> blah)
where 's' is a state token, and hence can be eta expanded.  This
showed up in the code for GHc.IO.Handle.Text.hPutChar, a rather
important function!

The eta-expansion will never happen unless we do it now.  (Well, it's
possible that CorePrep will do it, but CorePrep only has a half-baked
eta-expander that can't deal with casts.  So it's much better to do it
here.)

However, when the lambda is let-bound, as the RHS of a let, we have a
better eta-expander (in the form of tryEtaExpandRhs), so we don't
bother to try expansion in mkLam in that case; hence the contIsRhs
guard.

NB: We check the SimplEnv (sm_eta_expand), not DynFlags.
    See Note [No eta expansion in stable unfoldings]



Note [Casts and lambdas]
~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/simplCore/SimplUtils.hs#L1468>`__

Consider
        (\x. (\y. e) `cast` g1) `cast` g2
There is a danger here that the two lambdas look separated, and the
full laziness pass might float an expression to between the two.

So this equation in mkLam' floats the g1 out, thus:
        (\x. e `cast` g1)  -->  (\x.e) `cast` (tx -> g1)
where x:tx.

In general, this floats casts outside lambdas, where (I hope) they
might meet and cancel with some other cast:
        \x. e `cast` co   ===>   (\x. e) `cast` (tx -> co)
        /\a. e `cast` co  ===>   (/\a. e) `cast` (/\a. co)
        /\g. e `cast` co  ===>   (/\g. e) `cast` (/\g. co)
                          (if not (g `in` co))

Notice that it works regardless of 'e'.  Originally it worked only
if 'e' was itself a lambda, but in some cases that resulted in
fruitless iteration in the simplifier.  A good example was when
compiling Text.ParserCombinators.ReadPrec, where we had a definition
like    (\x. Get `cast` g)
where Get is a constructor with nonzero arity.  Then mkLam eta-expanded
the Get, and the next iteration eta-reduced it, and then eta-expanded
it again.

Note also the side condition for the case of coercion binders.
It does not make sense to transform
        /\g. e `cast` g  ==>  (/\g.e) `cast` (/\g.g)
because the latter is not well-kinded.



Note [Eta-expanding at let bindings]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/simplCore/SimplUtils.hs#L1551>`__

We now eta expand at let-bindings, which is where the payoff comes.
The most significant thing is that we can do a simple arity analysis
(in CoreArity.findRhsArity), which we can't do for free-floating lambdas

One useful consequence of not eta-expanding lambdas is this example:
   genMap :: C a => ...
   {-# INLINE genMap #-}
   genMap f xs = ...

::

   myMap :: D a => ...
   {-# INLINE myMap #-}
   myMap = genMap

Notice that 'genMap' should only inline if applied to two arguments.
In the stable unfolding for myMap we'll have the unfolding
    (\d -> genMap Int (..d..))
We do not want to eta-expand to
    (\d f xs -> genMap Int (..d..) f xs)
because then 'genMap' will inline, and it really shouldn't: at least
as far as the programmer is concerned, it's not applied to two
arguments!



Note [Do not eta-expand join points]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/simplCore/SimplUtils.hs#L1575>`__

Similarly to CPR (see Note [Don't CPR join points] in WorkWrap), a join point
stands well to gain from its outer binding's eta-expansion, and eta-expanding a
join point is fraught with issues like how to deal with a cast:

::

    let join $j1 :: IO ()
             $j1 = ...
             $j2 :: Int -> IO ()
             $j2 n = if n > 0 then $j1
                              else ...

::

    =>

::

    let join $j1 :: IO ()
             $j1 = (\eta -> ...)
                     `cast` N:IO :: State# RealWorld -> (# State# RealWorld, ())
                                 ~  IO ()
             $j2 :: Int -> IO ()
             $j2 n = (\eta -> if n > 0 then $j1
                                       else ...)
                     `cast` N:IO :: State# RealWorld -> (# State# RealWorld, ())
                                 ~  IO ()

The cast here can't be pushed inside the lambda (since it's not casting to a
function type), so the lambda has to stay, but it can't because it contains a
reference to a join point. In fact, $j2 can't be eta-expanded at all. Rather
than try and detect this situation (and whatever other situations crop up!), we
don't bother; again, any surrounding eta-expansion will improve these join
points anyway, since an outer cast can *always* be pushed inside. By the time
CorePrep comes around, the code is very likely to look more like this:

::

    let join $j1 :: State# RealWorld -> (# State# RealWorld, ())
             $j1 = (...) eta
             $j2 :: Int -> State# RealWorld -> (# State# RealWorld, ())
             $j2 = if n > 0 then $j1
                            else (...) eta



Note [Do not eta-expand PAPs]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/simplCore/SimplUtils.hs#L1613>`__

We used to have old_arity = manifestArity rhs, which meant that we
would eta-expand even PAPs.  But this gives no particular advantage,
and can lead to a massive blow-up in code size, exhibited by #9020.
Suppose we have a PAP
    foo :: IO ()
    foo = returnIO ()
Then we can eta-expand do
    foo = (\eta. (returnIO () |> sym g) eta) |> g
where
    g :: IO () ~ State# RealWorld -> (# State# RealWorld, () #)

But there is really no point in doing this, and it generates masses of
coercions and whatnot that eventually disappear again. For T9020, GHC
allocated 6.6G beore, and 0.8G afterwards; and residency dropped from
1.8G to 45M.

But note that this won't eta-expand, say
  f = \g -> map g
Does it matter not eta-expanding such functions?  I'm not sure.  Perhaps
strictness analysis will have less to bite on?



Note [Floating and type abstraction]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/simplCore/SimplUtils.hs#L1643>`__

Consider this:
        x = /\a. C e1 e2
We'd like to float this to
        y1 = /\a. e1
        y2 = /\a. e2
        x  = /\a. C (y1 a) (y2 a)
for the usual reasons: we want to inline x rather vigorously.

You may think that this kind of thing is rare.  But in some programs it is
common.  For example, if you do closure conversion you might get:

::

        data a :-> b = forall e. (e -> a -> b) :$ e

::

        f_cc :: forall a. a :-> a
        f_cc = /\a. (\e. id a) :$ ()

Now we really want to inline that f_cc thing so that the
construction of the closure goes away.

So I have elaborated simplLazyBind to understand right-hand sides that look
like
        /\ a1..an. body

and treat them specially. The real work is done in SimplUtils.abstractFloats,
but there is quite a bit of plumbing in simplLazyBind as well.

The same transformation is good when there are lets in the body:

::

        /\abc -> let(rec) x = e in b
   ==>
        let(rec) x' = /\abc -> let x = x' a b c in e
        in
        /\abc -> let x = x' a b c in b

This is good because it can turn things like:

        let f = /\a -> letrec g = ... g ... in g
into
        letrec g' = /\a -> ... g' a ...
        in
        let f = /\ a -> g' a

which is better.  In effect, it means that big lambdas don't impede
let-floating.

This optimisation is CRUCIAL in eliminating the junk introduced by
desugaring mutually recursive definitions.  Don't eliminate it lightly!

[May 1999]  If we do this transformation *regardless* then we can
end up with some pretty silly stuff.  For example,

        let
            st = /\ s -> let { x1=r1 ; x2=r2 } in ...
        in ..
becomes
        let y1 = /\s -> r1
            y2 = /\s -> r2
            st = /\s -> ...[y1 s/x1, y2 s/x2]
        in ..

Unless the "..." is a WHNF there is really no point in doing this.
Indeed it can make things worse.  Suppose x1 is used strictly,
and is of the form

::

        x1* = case f y of { (a,b) -> e }

If we abstract this wrt the tyvar we then can't do the case inline
as we would normally do.

That's why the whole transformation is part of the same process that
floats let-bindings and constructor arguments out of RHSs.  In particular,
it is guarded by the doFloatFromRhs call in simplLazyBind.



Note [Which type variables to abstract over]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/simplCore/SimplUtils.hs#L1718>`__

Abstract only over the type variables free in the rhs wrt which the
new binding is abstracted.  Note that

  * The naive approach of abstracting wrt the
    tyvars free in the Id's /type/ fails. Consider:
        /\ a b -> let t :: (a,b) = (e1, e2)
                      x :: a     = fst t
                  in ...
    Here, b isn't free in x's type, but we must nevertheless
    abstract wrt b as well, because t's type mentions b.
    Since t is floated too, we'd end up with the bogus:
         poly_t = /\ a b -> (e1, e2)
         poly_x = /\ a   -> fst (poly_t a *b*)

  * We must do closeOverKinds.  Example (#10934):
       f = /\k (f:k->*) (a:k). let t = AccFailure @ (f a) in ...
    Here we want to float 't', but we must remember to abstract over
    'k' as well, even though it is not explicitly mentioned in the RHS,
    otherwise we get
       t = /\ (f:k->*) (a:k). AccFailure @ (f a)
    which is obviously bogus.



Note [Abstract over coercions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/simplCore/SimplUtils.hs#L1833>`__

If a coercion variable (g :: a ~ Int) is free in the RHS, then so is the
type variable a.  Rather than sort this mess out, we simply bale out and abstract
wrt all the type variables if any of them are coercion variables.


Historical note: if you use let-bindings instead of a substitution, beware of this:

::

                -- Suppose we start with:
                --
                --      x = /\ a -> let g = G in E
                --
                -- Then we'll float to get
                --
                --      x = let poly_g = /\ a -> G
                --          in /\ a -> let g = poly_g a in E
                --
                -- But now the occurrence analyser will see just one occurrence
                -- of poly_g, not inside a lambda, so the simplifier will
                -- PreInlineUnconditionally poly_g back into g!  Badk to square 1!
                -- (I used to think that the "don't inline lone occurrences" stuff
                --  would stop this happening, but since it's the *only* occurrence,
                --  PreInlineUnconditionally kicks in first!)
                --
                -- Solution: put an INLINE note on g's RHS, so that poly_g seems
                --           to appear many times.  (NB: mkInlineMe eliminates
                --           such notes on trivial RHSs, so do it manually.)



Note [Merge Nested Cases]
~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/simplCore/SimplUtils.hs#L1949>`__

case e of b {             ==>   case e of b {
         p1 -> rhs1                      p1 -> rhs1
         ...                             ...
         pm -> rhsm                      pm -> rhsm
         _  -> case b of b' {            pn -> let b'=b in rhsn
                     pn -> rhsn          ...
                     ...                 po -> let b'=b in rhso
                     po -> rhso          _  -> let b'=b in rhsd
                     _  -> rhsd
       }

which merges two cases in one case when -- the default alternative of
the outer case scrutises the same variable as the outer case. This
transformation is called Case Merging.  It avoids that the same
variable is scrutinised multiple times.



Note [Eliminate Identity Case]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/simplCore/SimplUtils.hs#L1967>`__

case e of               ===> e
                True  -> True;
                False -> False

and similar friends.



Note [Scrutinee Constant Folding]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/simplCore/SimplUtils.hs#L1975>`__

case x op# k# of _ {  ===> case x of _ {
        a1# -> e1                  (a1# inv_op# k#) -> e1
        a2# -> e2                  (a2# inv_op# k#) -> e2
        ...                        ...
        DEFAULT -> ed              DEFAULT -> ed

::

     where (x op# k#) inv_op# k# == x

And similarly for commuted arguments and for some unary operations.

The purpose of this transformation is not only to avoid an arithmetic
operation at runtime but to allow other transformations to apply in cascade.

Example with the "Merge Nested Cases" optimization (from #12877):

::

      main = case t of t0
         0##     -> ...
         DEFAULT -> case t0 `minusWord#` 1## of t1
            0##    -> ...
            DEFAUT -> case t1 `minusWord#` 1## of t2
               0##     -> ...
               DEFAULT -> case t2 `minusWord#` 1## of _
                  0##     -> ...
                  DEFAULT -> ...

::

  becomes:

      main = case t of _
      0##     -> ...
      1##     -> ...
      2##     -> ...
      3##     -> ...
      DEFAULT -> ...

There are some wrinkles

* Do not apply caseRules if there is just a single DEFAULT alternative
     case e +# 3# of b { DEFAULT -> rhs }
  If we applied the transformation here we would (stupidly) get
     case a of b' { DEFAULT -> let b = e +# 3# in rhs }
  and now the process may repeat, because that let will really
  be a case.

* The type of the scrutinee might change.  E.g.
        case tagToEnum (x :: Int#) of (b::Bool)
          False -> e1
          True -> e2
  ==>
        case x of (b'::Int#)
          DEFAULT -> e1
          1#      -> e2

* The case binder may be used in the right hand sides, so we need
  to make a local binding for it, if it is alive.  e.g.
         case e +# 10# of b
           DEFAULT -> blah...b...
           44#     -> blah2...b...
  ===>
         case e of b'
           DEFAULT -> let b = b' +# 10# in blah...b...
           34#     -> let b = 44# in blah2...b...

::

  Note that in the non-DEFAULT cases we know what to bind 'b' to,
  whereas in the DEFAULT case we must reconstruct the original value.
  But NB: we use b'; we do not duplicate 'e'.

* In dataToTag we might need to make up some fake binders;
  see Note [caseRules for dataToTag] in PrelRules



Note [Literal cases]
~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/simplCore/SimplUtils.hs#L2215>`__

If we have
  case tagToEnum (a ># b) of
     False -> e1
     True  -> e2

then caseRules for TagToEnum will turn it into
  case tagToEnum (a ># b) of
     0# -> e1
     1# -> e2

Since the case is exhaustive (all cases are) we can convert it to
  case tagToEnum (a ># b) of
     DEFAULT -> e1
     1#      -> e2

This may generate sligthtly better code (although it should not, since
all cases are exhaustive) and/or optimise better.  I'm not certain that
it's necessary, but currenty we do make this change.  We do it here,
NOT in the TagToEnum rules (see "Beware" in Note [caseRules for tagToEnum]
in PrelRules)
------------------------------------------------
      Catch-all
------------------------------------------------



Note [Dead binders]
~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/simplCore/SimplUtils.hs#L2253>`__

Note that dead-ness is maintained by the simplifier, so that it is
accurate after simplification as well as before.



Note [Cascading case merge]
~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/simplCore/SimplUtils.hs#L2259>`__

Case merging should cascade in one sweep, because it
happens bottom-up

      case e of a {
        DEFAULT -> case a of b
                      DEFAULT -> case b of c {
                                     DEFAULT -> e
                                     A -> ea
                      B -> eb
        C -> ec
==>
      case e of a {
        DEFAULT -> case a of b
                      DEFAULT -> let c = b in e
                      A -> let c = b in ea
                      B -> eb
        C -> ec
==>
      case e of a {
        DEFAULT -> let b = a in let c = b in e
        A -> let b = a in let c = b in ea
        B -> let b = a in eb
        C -> ec


However here's a tricky case that we still don't catch, and I don't
see how to catch it in one pass:

::

  case x of c1 { I# a1 ->
  case a1 of c2 ->
    0 -> ...
    DEFAULT -> case x of c3 { I# a2 ->
               case a2 of ...

After occurrence analysis (and its binder-swap) we get this

::

  case x of c1 { I# a1 ->
  let x = c1 in         -- Binder-swap addition
  case a1 of c2 ->
    0 -> ...
    DEFAULT -> case x of c3 { I# a2 ->
               case a2 of ...

When we simplify the inner case x, we'll see that
x=c1=I# a1.  So we'll bind a2 to a1, and get

::

  case x of c1 { I# a1 ->
  case a1 of c2 ->
    0 -> ...
    DEFAULT -> case a1 of ...

This is corect, but we can't do a case merge in this sweep
because c2 /= a1.  Reason: the binding c1=I# a1 went inwards
without getting changed to c1=I# c2.

I don't think this is worth fixing, even if I knew how. It'll
all come out in the next pass anyway.

