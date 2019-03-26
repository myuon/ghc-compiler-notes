`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/simplCore/SetLevels.hs>`_

compiler/simplCore/SetLevels.hs
===============================


Note [FloatOut inside INLINE]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/simplCore/SetLevels.hs#L161>`__

@InlineCtxt@ very similar to @Level 0 0@, but is used for one purpose:
to say "don't float anything out of here".  That's exactly what we
want for the body of an INLINE, where we don't want to float anything
out at all.  See notes with lvlMFE below.

But, check this out:

-- At one time I tried the effect of not floating anything out of an InlineMe,
-- but it sometimes works badly.  For example, consider PrelArr.done.  It
-- has the form         __inline (\d. e)
-- where e doesn't mention d.  If we float this to
--      __inline (let x = e in \d. x)
-- things are bad.  The inliner doesn't even inline it because it doesn't look
-- like a head-normal form.  So it seems a lesser evil to let things float.
-- In SetLevels we do set the context to (Level 0 0) when we get to an InlineMe
-- which discourages floating out.

So the conclusion is: don't do any floating at all inside an InlineMe.
(In the above example, don't float the {x=e} out of the \d.)

One particular case is that of workers: we don't want to float the
call to the worker outside the wrapper, otherwise the worker might get
inlined into the floated expression, and an importing module won't see
the worker at all.



Note [Join ceiling]
~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/simplCore/SetLevels.hs#L188>`__

Join points can't float very far; too far, and they can't remain join points
So, suppose we have:

::

  f x = (joinrec j y = ... x ... in jump j x) + 1

One may be tempted to float j out to the top of f's RHS, but then the jump
would not be a tail call. Thus we keep track of a level called the *join
ceiling* past which join points are not allowed to float.

The troublesome thing is that, unlike most levels to which something might
float, there is not necessarily an identifier to which the join ceiling is
attached. Fortunately, if something is to be floated to a join ceiling, it must
be dropped at the *nearest* join ceiling. Thus each level is marked as to
whether it is a join ceiling, so that FloatOut can tell which binders are being
floated to the nearest join ceiling and which to a particular binder (or set of
binders).



Note [Floating over-saturated applications]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/simplCore/SetLevels.hs#L303>`__

If we see (f x y), and (f x) is a redex (ie f's arity is 1),
we call (f x) an "over-saturated application"

Should we float out an over-sat app, if can escape a value lambda?
It is sometimes very beneficial (-7% runtime -4% alloc over nofib -O2).
But we don't want to do it for class selectors, because the work saved
is minimal, and the extra local thunks allocated cost money.

Arguably we could float even class-op applications if they were going to
top level -- but then they must be applied to a constant dictionary and
will almost certainly be optimised away anyway.



Note [Floating single-alternative cases]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/simplCore/SetLevels.hs#L494>`__

Consider this:
  data T a = MkT !a
  f :: T Int -> blah
  f x vs = case x of { MkT y ->
             let f vs = ...(case y of I# w -> e)...f..
             in f vs

Here we can float the (case y ...) out, because y is sure
to be evaluated, to give
  f x vs = case x of { MkT y ->
           caes y of I# w ->
             let f vs = ...(e)...f..
             in f vs

That saves unboxing it every time round the loop.  It's important in
some DPH stuff where we really want to avoid that repeated unboxing in
the inner loop.

Things to note:

 * The test we perform is exprIsHNF, and /not/ exprOkForSpeculation.

     - exrpIsHNF catches the key case of an evaluated variable

     - exprOkForSpeculation is /false/ of an evaluated variable;
       See Note [exprOkForSpeculation and evaluated variables] in CoreUtils
       So we'd actually miss the key case!

     - Nothing is gained from the extra generality of exprOkForSpeculation
       since we only consider floating a case whose single alternative
       is a DataAlt   K a b -> rhs

 * We can't float a case to top level

 * It's worth doing this float even if we don't float
   the case outside a value lambda.  Example
     case x of {
       MkT y -> (case y of I# w2 -> ..., case y of I# w2 -> ...)
   If we floated the cases out we could eliminate one of them.

 * We only do this with a single-alternative case



Note [Check the output scrutinee for exprIsHNF]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/simplCore/SetLevels.hs#L538>`__

Consider this:
  case x of y {
    A -> ....(case y of alts)....
  }

Because of the binder-swap, the inner case will get substituted to
(case x of ..).  So when testing whether the scrutinee is in HNF we
must be careful to test the *result* scrutinee ('x' in this case), not
the *input* one 'y'.  The latter *is* in HNF here (because y is
evaluated), but the former is not -- and indeed we can't float the
inner case out, at least not unless x is also evaluated at its binding
site.  See #5453.

That's why we apply exprIsHNF to scrut' and not to scrut.

See Note [Floating single-alternative cases] for why
we use exprIsHNF in the first place.



Note [Floating to the top]
~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/simplCore/SetLevels.hs#L699>`__

We are keen to float something to the top level, even if it does not
escape a value lambda (and hence save work), for two reasons:

  * Doing so makes the function smaller, by floating out
    bottoming expressions, or integer or string literals.  That in
    turn makes it easier to inline, with less duplication.

  * (Minor) Doing so may turn a dynamic allocation (done by machine
    instructions) into a static one. Minor because we are assuming
    we are not escaping a value lambda.

But do not so if:
     - the context is a strict, and
     - the expression is not a HNF, and
     - the expression is not bottoming

Exammples:

* Bottoming
      f x = case x of
              0 -> error <big thing>
              _ -> x+1
  Here we want to float (error <big thing>) to top level, abstracting
  over 'x', so as to make f's RHS smaller.

* HNF
      f = case y of
            True  -> p:q
            False -> blah
  We may as well float the (p:q) so it becomes a static data structure.

* Case scrutinee
      f = case g True of ....
  Don't float (g True) to top level; then we have the admin of a
  top-level thunk to worry about, with zero gain.

* Case alternative
      h = case y of
             True  -> g True
             False -> False
  Don't float (g True) to the top level

* Arguments
     t = f (g True)
  If f is lazy, we /do/ float (g True) because then we can allocate
  the thunk statically rather than dynamically.  But if f is strict
  we don't (see the use of idStrictness in lvlApp).  It's not clear
  if this test is worth the bother: it's only about CAFs!

It's controlled by a flag (floatConsts), because doing this too
early loses opportunities for RULES which (needless to say) are
important in some nofib programs (gcd is an example).  [SPJ note:
I think this is obselete; the flag seems always on.]



Note [Floating join point bindings]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/simplCore/SetLevels.hs#L755>`__

Mostly we only float a join point if it can /stay/ a join point.  But
there is one exception: if it can go to the top level (#13286).
Consider
  f x = joinrec j y n = <...j y' n'...>
        in jump j x 0

Here we may just as well produce
  j y n = <....j y' n'...>
  f x = j x 0

and now there is a chance that 'f' will be inlined at its call sites.
It shouldn't make a lot of difference, but thes tests
  perf/should_run/MethSharing
  simplCore/should_compile/spec-inline
and one nofib program, all improve if you do float to top, because
of the resulting inlining of f.  So ok, let's do it.



Note [Free join points]
~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/simplCore/SetLevels.hs#L774>`__

We never float a MFE that has a free join-point variable.  You mght think
this can never occur.  After all, consider
     join j x = ...
     in ....(jump j x)....
How might we ever want to float that (jump j x)?
  * If it would escape a value lambda, thus
        join j x = ... in (\y. ...(jump j x)... )
    then 'j' isn't a valid join point in the first place.

But consider
     join j x = .... in
     joinrec j2 y =  ...(jump j x)...(a+b)....

Since j2 is recursive, it /is/ worth floating (a+b) out of the joinrec.
But it is emphatically /not/ good to float the (jump j x) out:
 (a) 'j' will stop being a join point
 (b) In any case, jumping to 'j' must be an exit of the j2 loop, so no
     work would be saved by floating it out of the \y.

Even if we floated 'j' to top level, (b) would still hold.

Bottom line: never float a MFE that has a free JoinId.



Note [Floating MFEs of unlifted type]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/simplCore/SetLevels.hs#L799>`__

Suppose we have
   case f x of (r::Int#) -> blah
we'd like to float (f x). But it's not trivial because it has type
Int#, and we don't want to evaluate it too early.  But we can instead
float a boxed version
   y = case f x of r -> I# r
and replace the original (f x) with
   case (case y of I# r -> r) of r -> blah

Being able to float unboxed expressions is sometimes important; see
#12603.  I'm not sure how /often/ it is important, but it's
not hard to achieve.

We only do it for a fixed collection of types for which we have a
convenient boxing constructor (see boxingDataCon_maybe).  In
particular we /don't/ do it for unboxed tuples; it's better to float
the components of the tuple individually.

I did experiment with a form of boxing that works for any type, namely
wrapping in a function.  In our example

::

   let y = case f x of r -> \v. f x
   in case y void of r -> blah

It works fine, but it's 50% slower (based on some crude benchmarking).
I suppose we could do it for types not covered by boxingDataCon_maybe,
but it's more code and I'll wait to see if anyone wants it.



Note [Test cheapness with exprOkForSpeculation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/simplCore/SetLevels.hs#L829>`__

We don't want to float very cheap expressions by boxing and unboxing.
But we use exprOkForSpeculation for the test, not exprIsCheap.
Why?  Because it's important /not/ to transform
     f (a /# 3)
to
     f (case bx of I# a -> a /# 3)
and float bx = I# (a /# 3), because the application of f no
longer obeys the let/app invariant.  But (a /# 3) is ok-for-spec
due to a special hack that says division operators can't fail
when the denominator is definitely non-zero.  And yet that
same expression says False to exprIsCheap.  Simplest way to
guarantee the let/app invariant is to use the same function!

If an expression is okay for speculation, we could also float it out
*without* boxing and unboxing, since evaluating it early is okay.
However, it turned out to usually be better not to float such expressions,
since they tend to be extremely cheap things like (x +# 1#). Even the
cost of spilling the let-bound variable to the stack across a call may
exceed the cost of recomputing such an expression. (And we can't float
unlifted bindings to top-level.)

We could try to do something smarter here, and float out expensive yet
okay-for-speculation things, such as division by non-zero constants.
But I suspect it's a narrow target.



Note [Bottoming floats]
~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/simplCore/SetLevels.hs#L856>`__

If we see
        f = \x. g (error "urk")
we'd like to float the call to error, to get
        lvl = error "urk"
        f = \x. g lvl

But, as ever, we need to be careful:

(1) We want to float a bottoming
    expression even if it has free variables:
        f = \x. g (let v = h x in error ("urk" ++ v))
    Then we'd like to abstract over 'x' can float the whole arg of g:
        lvl = \x. let v = h x in error ("urk" ++ v)
        f = \x. g (lvl x)
    To achieve this we pass is_bot to destLevel

(2) We do not do this for lambdas that return
    bottom.  Instead we treat the /body/ of such a function specially,
    via point (1).  For example:
        f = \x. ....(\y z. if x then error y else error z)....
    ===>
        lvl = \x z y. if b then error y else error z
        f = \x. ...(\y z. lvl x z y)...
    (There is no guarantee that we'll choose the perfect argument order.)

(3) If we have a /binding/ that returns bottom, we want to float it to top
    level, even if it has free vars (point (1)), and even it has lambdas.
    Example:
       ... let { v = \y. error (show x ++ show y) } in ...
    We want to abstract over x and float the whole thing to top:
       lvl = \xy. errror (show x ++ show y)
       ...let {v = lvl x} in ...

::

    Then of course we don't want to separately float the body (error ...)
    as /another/ MFE, so we tell lvlFloatRhs not to do that, via the is_bot
    argument.

See Maessen's paper 1999 "Bottom extraction: factoring error handling out
of functional programs" (unpublished I think).

When we do this, we set the strictness and arity of the new bottoming
Id, *immediately*, for three reasons:

  * To prevent the abstracted thing being immediately inlined back in again
    via preInlineUnconditionally.  The latter has a test for bottoming Ids
    to stop inlining them, so we'd better make sure it *is* a bottoming Id!

  * So that it's properly exposed as such in the interface file, even if
    this is all happening after strictness analysis.

  * In case we do CSE with the same expression that *is* marked bottom
        lvl          = error "urk"
          x{str=bot) = error "urk"
    Here we don't want to replace 'x' with 'lvl', else we may get Lint
    errors, e.g. via a case with empty alternatives:  (case x of {})
    Lint complains unless the scrutinee of such a case is clearly bottom.

::

    This was reported in #11290.   But since the whole bottoming-float
    thing is based on the cheap-and-cheerful exprIsBottom, I'm not sure
    that it'll nail all such cases.



Note [Bottoming floats: eta expansion] c.f Note [Bottoming floats]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/simplCore/SetLevels.hs#L919>`__

Tiresomely, though, the simplifier has an invariant that the manifest
arity of the RHS should be the same as the arity; but we can't call
etaExpand during SetLevels because it works over a decorated form of
CoreExpr.  So we do the eta expansion later, in FloatOut.



Note [Case MFEs]
~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/simplCore/SetLevels.hs#L926>`__

We don't float a case expression as an MFE from a strict context.  Why not?
Because in doing so we share a tiny bit of computation (the switch) but
in exchange we build a thunk, which is bad.  This case reduces allocation
by 7% in spectral/puzzle (a rather strange benchmark) and 1.2% in real/fem.
Doesn't change any other allocation at all.

We will make a separate decision for the scrutinee and alternatives.

However this can have a knock-on effect for fusion: consider
    \v -> foldr k z (case x of I# y -> build ..y..)
Perhaps we can float the entire (case x of ...) out of the \v.  Then
fusion will not happen, but we will get more sharing.  But if we don't
float the case (as advocated here) we won't float the (build ...y..)
either, so fusion will happen.  It can be a big effect, esp in some
artificial benchmarks (e.g. integer, queens), but there is no perfect
answer.



Note [Floating literals]
~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/simplCore/SetLevels.hs#L995>`__

It's important to float Integer literals, so that they get shared,
rather than being allocated every time round the loop.
Hence the litIsTrivial.

Ditto literal strings (LitString), which we'd like to float to top
level, which is now possible.



Note [Escaping a value lambda]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/simplCore/SetLevels.hs#L1005>`__

We want to float even cheap expressions out of value lambdas,
because that saves allocation.  Consider
        f = \x.  .. (\y.e) ...
Then we'd like to avoid allocating the (\y.e) every time we call f,
(assuming e does not mention x). An example where this really makes a
difference is simplrun009.

Another reason it's good is because it makes SpecContr fire on functions.
Consider
        f = \x. ....(f (\y.e))....
After floating we get
        lvl = \y.e
        f = \x. ....(f lvl)...
and that is much easier for SpecConstr to generate a robust
specialisation for.

However, if we are wrapping the thing in extra value lambdas (in
abs_vars), then nothing is saved.  E.g.
        f = \xyz. ...(e1[y],e2)....
If we float
        lvl = \y. (e1[y],e2)
        f = \xyz. ...(lvl y)...
we have saved nothing: one pair will still be allocated for each
call of 'f'.  Hence the (not float_is_lam) in float_me.



Note [Floating from a RHS]
~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/simplCore/SetLevels.hs#L1231>`__

When floating the RHS of a let-binding, we don't always want to apply
lvlMFE to the body of a lambda, as we usually do, because the entire
binding body is already going to the right place (dest_lvl).

A particular example is the top level.  Consider
   concat = /\ a -> foldr ..a.. (++) []
We don't want to float the body of the lambda to get
   lvl    = /\ a -> foldr ..a.. (++) []
   concat = /\ a -> lvl a
That would be stupid.

Previously this was avoided in a much nastier way, by testing strict_ctxt
in float_me in lvlMFE.  But that wasn't even right because it would fail
to float out the error sub-expression in
    f = \x. case x of
              True  -> error ("blah" ++ show x)
              False -> ...

But we must be careful:

* If we had
    f = \x -> factorial 20
  we /would/ want to float that (factorial 20) out!  Functions are treated
  differently: see the use of isFunction in the calls to destLevel. If
  there are only type lambdas, then destLevel will say "go to top, and
  abstract over the free tyvars" and we don't want that here.

* But if we had
    f = \x -> error (...x....)
  we would NOT want to float the bottoming expression out to give
    lvl = \x -> error (...x...)
    f = \x -> lvl x

Conclusion: use lvlMFE if there are
  * any value lambdas in the original function, and
  * this is not a bottoming function (the is_bot argument)
Use lvlExpr otherwise.  A little subtle, and I got it wrong at least twice
(e.g. #13369).



Note [Floating and kind casts]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/simplCore/SetLevels.hs#L1381>`__

Consider this
   case x of
     K (co :: * ~# k) -> let v :: Int |> co
                             v = e
                         in blah

Then, even if we are abstracting over Ids, or if e is bottom, we can't
float v outside the 'co' binding.  Reason: if we did we'd get
    v' :: forall k. (Int ~# Age) => Int |> co
and now 'co' isn't in scope in that type. The underlying reason is
that 'co' is a value-level thing and we can't abstract over that in a
type (else we'd get a dependent type).  So if v's /type/ mentions 'co'
we can't float it out beyond the binding site of 'co'.

That's why we have this as_far_as_poss stuff.  Usually as_far_as_poss
is just tOP_LEVEL; but occasionally a coercion variable (which is an
Id) mentioned in type prevents this.

Example #14270 comment:15.



Note [le_subst and le_env]
~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/simplCore/SetLevels.hs#L1456>`__

We clone let- and case-bound variables so that they are still distinct
when floated out; hence the le_subst/le_env.  (see point 3 of the
module overview comment).  We also use these envs when making a
variable polymorphic because we want to float it out past a big
lambda.

The le_subst and le_env always implement the same mapping,
     in_x :->  out_x a b
where out_x is an OutVar, and a,b are its arguments (when
we perform abstraction at the same time as floating).

::

  le_subst maps to CoreExpr
  le_env   maps to LevelledExpr

Since the range is always a variable or application, there is never
any difference between the two, but sadly the types differ.  The
le_subst is used when substituting in a variable's IdInfo; the le_env
when we find a Var.

In addition the le_env records a [OutVar] of variables free in the
OutExpr/LevelledExpr, just so we don't have to call freeVars
repeatedly.  This list is always non-empty, and the first element is
out_x

The domain of the both envs is *pre-cloned* Ids, though

The domain of the le_lvl_env is the *post-cloned* Ids



Note [Zapping the demand info]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/simplCore/SetLevels.hs#L1711>`__

VERY IMPORTANT: we must zap the demand info if the thing is going to
float out, because it may be less demanded than at its original
binding site.  Eg
   f :: Int -> Int
   f x = let v = 3*4 in v+x
Here v is strict; but if we float v to top level, it isn't any more.

Similarly, if we're floating a join point, it won't be one anymore, so we zap
join point information as well.

