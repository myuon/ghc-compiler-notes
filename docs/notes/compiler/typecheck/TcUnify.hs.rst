`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcUnify.hs>`_

compiler/typecheck/TcUnify.hs
=============================


Note [Herald for matchExpectedFunTys]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcUnify.hs#L76>`__

The 'herald' always looks like:
   "The equation(s) for 'f' have"
   "The abstraction (\x.e) takes"
   "The section (+ x) expects"
   "The function 'f' is applied to"

This is used to construct a message of form

::

   The abstraction `\Just 1 -> ...' takes two arguments
   but its type `Maybe a -> a' has only one

..

::

   The equation(s) for `f' have two arguments
   but its type `Maybe a -> a' has only one

..

::

   The section `(f 3)' requires 'f' to take two arguments
   but its type `Int -> Int' has only one

..

::

   The function 'f' is applied to two arguments
   but its type `Int -> Int' has only one

..

When visible type applications (e.g., `f @Int 1 2`, as in #13902) enter the
picture, we have a choice in deciding whether to count the type applications as
proper arguments:

   The function 'f' is applied to one visible type argument
     and two value arguments
   but its type `forall a. a -> a` has only one visible type argument
     and one value argument

Or whether to include the type applications as part of the herald itself:

::

   The expression 'f @Int' is applied to two arguments
   but its type `Int -> Int` has only one

..

The latter is easier to implement and is arguably easier to understand, so we
choose to implement that option.



Note [matchExpectedFunTys]
~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcUnify.hs#L115>`__

matchExpectedFunTys checks that a sigma has the form
of an n-ary function.  It passes the decomposed type to the
thing_inside, and returns a wrapper to coerce between the two types

It's used wherever a language construct must have a functional type,
namely:
        A lambda expression
        A function definition
     An operator section

This function must be written CPS'd because it needs to fill in the
ExpTypes produced for arguments before it can fill in the ExpType
passed in.


Use this one when you have an "expected" type.



Note [Subsumption checking: tcSubType]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcUnify.hs#L455>`__

All the tcSubType calls have the form
                tcSubType actual_ty expected_ty
which checks
                actual_ty <= expected_ty

That is, that a value of type actual_ty is acceptable in
a place expecting a value of type expected_ty.  I.e. that

::

    actual ty   is more polymorphic than   expected_ty

..

It returns a coercion function
        co_fn :: actual_ty ~ expected_ty
which takes an HsExpr of type actual_ty into one of type
expected_ty.

These functions do not actually check for subsumption. They check if
expected_ty is an appropriate annotation to use for something of type
actual_ty. This difference matters when thinking about visible type
application. For example,

   forall a. a -> forall b. b -> b
      DOES NOT SUBSUME
   forall a b. a -> b -> b

because the type arguments appear in a different order. (Neither does
it work the other way around.) BUT, these types are appropriate annotations
for one another. Because the user directs annotations, it's OK if some
arguments shuffle around -- after all, it's what the user wants.
Bottom line: none of this changes with visible type application.

There are a number of wrinkles (below).

Notice that Wrinkle 1 and 2 both require eta-expansion, which technically
may increase termination.  We just put up with this, in exchange for getting
more predictable type inference.

Wrinkle 1: Note [Deep skolemisation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We want   (forall a. Int -> a -> a)  <=  (Int -> forall a. a->a)
(see section 4.6 of "Practical type inference for higher rank types")
So we must deeply-skolemise the RHS before we instantiate the LHS.

That is why tc_sub_type starts with a call to tcSkolemise (which does the
deep skolemisation), and then calls the DS variant (which assumes
that expected_ty is deeply skolemised)

Wrinkle 2: Note [Co/contra-variance of subsumption checking]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider  g :: (Int -> Int) -> Int
  f1 :: (forall a. a -> a) -> Int
  f1 = g

  f2 :: (forall a. a -> a) -> Int
  f2 x = g x
f2 will typecheck, and it would be odd/fragile if f1 did not.
But f1 will only typecheck if we have that
    (Int->Int) -> Int  <=  (forall a. a->a) -> Int
And that is only true if we do the full co/contravariant thing
in the subsumption check.  That happens in the FunTy case of
tcSubTypeDS_NC_O, and is the sole reason for the WpFun form of
HsWrapper.

Another powerful reason for doing this co/contra stuff is visible
in #9569, involving instantiation of constraint variables,
and again involving eta-expansion.

Wrinkle 3: Note [Higher rank types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider tc150:
  f y = \ (x::forall a. a->a). blah
The following happens:
* We will infer the type of the RHS, ie with a res_ty = alpha.
* Then the lambda will split  alpha := beta -> gamma.
* And then we'll check tcSubType IsSwapped beta (forall a. a->a)

So it's important that we unify beta := forall a. a->a, rather than
skolemising the type.



Note [Don't skolemise unnecessarily]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcUnify.hs#L686>`__

Suppose we are trying to solve
    (Char->Char) <= (forall a. a->a)
We could skolemise the 'forall a', and then complain
that (Char ~ a) is insoluble; but that's a pretty obscure
error.  It's better to say that
    (Char->Char) ~ (forall a. a->a)
fails.

So roughly:
 * if the ty_expected has an outermost forall
      (i.e. skolemisation is the next thing we'd do)
 * and the ty_actual has no top-level polymorphism (but looking deeply)
then we can revert to simple equality.  But we need to be careful.
These examples are all fine:

 * (Char -> forall a. a->a) <= (forall a. Char -> a -> a)
      Polymorphism is buried in ty_actual

 * (Char->Char) <= (forall a. Char -> Char)
      ty_expected isn't really polymorphic

 * (Char->Char) <= (forall a. (a~Char) => a -> a)
      ty_expected isn't really polymorphic

 * (Char->Char) <= (forall a. F [a] Char -> Char)
                   where type instance F [x] t = t
     ty_expected isn't really polymorphic

If we prematurely go to equality we'll reject a program we should
accept (e.g. #13752).  So the test (which is only to improve
error message) is very conservative:
 * ty_actual is /definitely/ monomorphic
 * ty_expected is /definitely/ polymorphic



Note [Settting the argument context]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcUnify.hs#L811>`__

Consider we are doing the ambiguity check for the (bogus)
  f :: (forall a b. C b => a -> a) -> Int

We'll call
   tcSubType ((forall a b. C b => a->a) -> Int )
             ((forall a b. C b => a->a) -> Int )

with a UserTypeCtxt of (FunSigCtxt "f").  Then we'll do the co/contra thing
on the argument type of the (->) -- and at that point we want to switch
to a UserTypeCtxt of GenSigCtxt.  Why?

* Error messages.  If we stick with FunSigCtxt we get errors like
     * Could not deduce: C b
       from the context: C b0
        bound by the type signature for:
            f :: forall a b. C b => a->a
  But of course f does not have that type signature!
  Example tests: T10508, T7220a, Simple14

* Implications. We may decide to build an implication for the whole
  ambiguity check, but we don't need one for each level within it,
  and TcUnify.alwaysBuildImplication checks the UserTypeCtxt.
  See Note [When to build an implication]



Note [Deep instantiation of InferResult]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcUnify.hs#L922>`__

In some cases we want to deeply instantiate before filling in
an InferResult, and in some cases not.  That's why InferReult
has the ir_inst flag.

* ir_inst = True: deeply instantiate

  Consider
    f x = (*)
  We want to instantiate the type of (*) before returning, else we
  will infer the type
    f :: forall {a}. a -> forall b. Num b => b -> b -> b
  This is surely confusing for users.

::

  And worse, the monomorphism restriction won't work properly. The MR is
  dealt with in simplifyInfer, and simplifyInfer has no way of
  instantiating. This could perhaps be worked around, but it may be
  hard to know even when instantiation should happen.

..

  Another reason.  Consider
       f :: (?x :: Int) => a -> a
       g y = let ?x = 3::Int in f
  Here want to instantiate f's type so that the ?x::Int constraint
  gets discharged by the enclosing implicit-parameter binding.

* ir_inst = False: do not instantiate

::

  Consider this (which uses visible type application):

..

::

    (let { f :: forall a. a -> a; f x = x } in f) @Int

..

::

  We'll call TcExpr.tcInferFun to infer the type of the (let .. in f)
  And we don't want to instantite the type of 'f' when we reach it,
  else the outer visible type application won't work

..



Note [Promoting a type]
~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcUnify.hs#L1004>`__

Consider (#12427)

::

  data T where
    MkT :: (Int -> Int) -> a -> T

..

::

  h y = case y of MkT v w -> v

..

We'll infer the RHS type with an expected type ExpType of
  (IR { ir_lvl = l, ir_ref = ref, ... )
where 'l' is the TcLevel of the RHS of 'h'.  Then the MkT pattern
match will increase the level, so we'll end up in tcSubType, trying to
unify the type of v,
  v :: Int -> Int
with the expected type.  But this attempt takes place at level (l+1),
rightly so, since v's type could have mentioned existential variables,
(like w's does) and we want to catch that.

So we
  - create a new meta-var alpha[l+1]
  - fill in the InferRes ref cell 'ref' with alpha
  - emit an equality constraint, thus
        [W] alpha[l+1] ~ (Int -> Int)

That constraint will float outwards, as it should, unless v's
type mentions a skolem-captured variable.

This approach fails if v has a higher rank type; see
Note [Promotion and higher rank types]



Note [Promotion and higher rank types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcUnify.hs#L1036>`__

If v had a higher-rank type, say v :: (forall a. a->a) -> Int,
then we'd emit an equality
        [W] alpha[l+1] ~ ((forall a. a->a) -> Int)
which will sadly fail because we can't unify a unification variable
with a polytype.  But there is nothing really wrong with the program
here.

We could just about solve this by "promote the type" of v, to expose
its polymorphic "shape" while still leaving constraints that will
prevent existential escape.  But we must be careful!  Exposing
the "shape" of the type is precisely what we must NOT do under
a GADT pattern match!  So in this case we might promote the type
to
        (forall a. a->a) -> alpha[l+1]
and emit the constraint
        [W] alpha[l+1] ~ Int
Now the promoted type can fill the ref cell, while the emitted
equality can float or not, according to the usual rules.

But that's not quite right!  We are exposing the arrow! We could
deal with that too:
        (forall a. mu[l+1] a a) -> alpha[l+1]
with constraints
        [W] alpha[l+1] ~ Int
        [W] mu[l+1] ~ (->)
Here we abstract over the '->' inside the forall, in case that
is subject to an equality constraint from a GADT match.

Note that we kept the outer (->) because that's part of
the polymorphic "shape".  And because of impredicativity,
GADT matches can't give equalities that affect polymorphic
shape.

This reasoning just seems too complicated, so I decided not
to do it.  These higher-rank notes are just here to record
the thinking.



Note [When to build an implication]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcUnify.hs#L1263>`__

Suppose we have some 'skolems' and some 'givens', and we are
considering whether to wrap the constraints in their scope into an
implication.  We must /always/ so if either 'skolems' or 'givens' are
non-empty.  But what if both are empty?  You might think we could
always drop the implication.  Other things being equal, the fewer
implications the better.  Less clutter and overhead.  But we must
take care:

* If we have an unsolved [W] g :: a ~# b, and -fdefer-type-errors,
  we'll make a /term-level/ evidence binding for 'g = error "blah"'.
  We must have an EvBindsVar those bindings!, otherwise they end up as
  top-level unlifted bindings, which are verboten. This only matters
  at top level, so we check for that
  See also Note [Deferred errors for coercion holes] in TcErrors.
  cf #14149 for an example of what goes wrong.

* If you have
     f :: Int;  f = f_blah
     g :: Bool; g = g_blah
  If we don't build an implication for f or g (no tyvars, no givens),
  the constraints for f_blah and g_blah are solved together.  And that
  can yield /very/ confusing error messages, because we can get
      [W] C Int b1    -- from f_blah
      [W] C Int b2    -- from g_blan
  and fundpes can yield [D] b1 ~ b2, even though the two functions have
  literally nothing to do with each other.  #14185 is an example.
  Building an implication keeps them separage.



Note [Check for equality before deferring]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcUnify.hs#L1494>`__

Particularly in ambiguity checks we can get equalities like (ty ~ ty).
If ty involves a type function we may defer, which isn't very sensible.
An egregious example of this was in test T9872a, which has a type signature
       Proxy :: Proxy (Solutions Cubes)
Doing the ambiguity check on this signature generates the equality
   Solutions Cubes ~ Solutions Cubes
and currently the constraint solver normalises both sides at vast cost.
This little short-cut in 'defer' helps quite a bit.



Note [Care with type applications]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcUnify.hs#L1505>`__

Note: type applications need a bit of care!
They can match FunTy and TyConApp, so use splitAppTy_maybe
NB: we've already dealt with type variables and Notes,
so if one type is an App the other one jolly well better be too



Note [Mismatched type lists and application decomposition]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcUnify.hs#L1512>`__

When we find two TyConApps, you might think that the argument lists
are guaranteed equal length.  But they aren't. Consider matching
        w (T x) ~ Foo (T x y)
We do match (w ~ Foo) first, but in some circumstances we simply create
a deferred constraint; and then go ahead and match (T x ~ T x y).
This came up in #3950.

So either
   (a) either we must check for identical argument kinds
       when decomposing applications,

::

   (b) or we must be prepared for ill-kinded unification sub-problems

..

Currently we adopt (b) since it seems more robust -- no need to maintain
a global invariant.



Note [Expanding synonyms during unification]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcUnify.hs#L1530>`__

We expand synonyms during unification, but:
 * We expand *after* the variable case so that we tend to unify
   variables with un-expanded type synonym. This just makes it
   more likely that the inferred types will mention type synonyms
   understandable to the user

 * Similarly, we expand *after* the CastTy case, just in case the
   CastTy wraps a variable.

 * We expand *before* the TyConApp case.  For example, if we have
      type Phantom a = Int
   and are unifying
      Phantom Int ~ Phantom Char
   it is *wrong* to unify Int and Char.

 * The problem case immediately above can happen only with arguments
   to the tycon. So we check for nullary tycons *before* expanding.
   This is particularly helpful when checking (* ~ *), because * is
   now a type synonym.



Note [Deferred Unification]
~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcUnify.hs#L1552>`__

We may encounter a unification ty1 ~ ty2 that cannot be performed syntactically,
and yet its consistency is undetermined. Previously, there was no way to still
make it consistent. So a mismatch error was issued.

Now these unifications are deferred until constraint simplification, where type
family instances and given equations may (or may not) establish the consistency.
Deferred unifications are of the form
                F ... ~ ...
or              x ~ ...
where F is a type function and x is a type variable.
E.g.
        id :: x ~ y => x -> y
        id e = e

involves the unification x = y. It is deferred until we bring into account the
context x ~ y to establish that it holds.

If available, we defer original types (rather than those where closed type
synonyms have already been expanded via tcCoreView).  This is, as usual, to
improve error messages.



Note [TyVar/TyVar orientation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcUnify.hs#L1711>`__

Given (a ~ b), should we orient the CTyEqCan as (a~b) or (b~a)?
This is a surprisingly tricky question!

First note: only swap if you have to!
   See Note [Avoid unnecessary swaps]

So we look for a positive reason to swap, using a three-step test:

* Level comparison. If 'a' has deeper level than 'b',
  put 'a' on the left.  See Note [Deeper level on the left]

* Priority.  If the levels are the same, look at what kind of
  type variable it is, using 'lhsPriority'

  - FlatMetaTv: Always put on the left.
    See Note [Fmv Orientation Invariant]
    NB: FlatMetaTvs always have the current level, never an
        outer one.  So nothing can be deeper than a FlatMetaTv


  - TyVarTv/TauTv: if we have  tyv_tv ~ tau_tv, put tau_tv
                   on the left because there are fewer
                   restrictions on updating TauTvs

  - TyVarTv/TauTv:  put on the left either
     a) Because it's touchable and can be unified, or
     b) Even if it's not touchable, TcSimplify.floatEqualities
        looks for meta tyvars on the left

  - FlatSkolTv: Put on the left in preference to a SkolemTv
                See Note [Eliminate flat-skols]

* Names. If the level and priority comparisons are all
  equal, try to eliminate a TyVars with a System Name in
  favour of ones with a Name derived from a user type signature

* Age.  At one point in the past we tried to break any remaining
  ties by eliminating the younger type variable, based on their
  Uniques.  See Note [Eliminate younger unification variables]
  (which also explains why we don't do this any more)



Note [Deeper level on the left]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcUnify.hs#L1754>`__

The most important thing is that we want to put tyvars with
the deepest level on the left.  The reason to do so differs for
Wanteds and Givens, but either way, deepest wins!  Simple.

* Wanteds.  Putting the deepest variable on the left maximise the
  chances that it's a touchable meta-tyvar which can be solved.

* Givens. Suppose we have something like
     forall a[2]. b[1] ~ a[2] => beta[1] ~ a[2]

::

  If we orient the Given a[2] on the left, we'll rewrite the Wanted to
  (beta[1] ~ b[1]), and that can float out of the implication.
  Otherwise it can't.  By putting the deepest variable on the left
  we maximise our changes of eliminating skolem capture.

..

::

  See also TcSMonad Note [Let-bound skolems] for another reason
  to orient with the deepest skolem on the left.

..

  IMPORTANT NOTE: this test does a level-number comparison on
  skolems, so it's important that skolems have (accurate) level
  numbers.

See #15009 for an further analysis of why "deepest on the left"
is a good plan.



Note [Fmv Orientation Invariant]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcUnify.hs#L1781>`__

   * We always orient a constraint
        fmv ~ alpha
     with fmv on the left, even if alpha is
     a touchable unification variable

Reason: doing it the other way round would unify alpha:=fmv, but that
really doesn't add any info to alpha.  But a later constraint alpha ~
Int might unlock everything.  Comment:9 of #12526 gives a detailed
example.

WARNING: I've gone to and fro on this one several times.
I'm now pretty sure that unifying alpha:=fmv is a bad idea!
So orienting with fmvs on the left is a good thing.

This example comes from IndTypesPerfMerge. (Others include
T10226, T10009.)
    From the ambiguity check for
      f :: (F a ~ a) => a
    we get:
          [G] F a ~ a
          [WD] F alpha ~ alpha, alpha ~ a

    From Givens we get
          [G] F a ~ fsk, fsk ~ a

::

    Now if we flatten we get
          [WD] alpha ~ fmv, F alpha ~ fmv, alpha ~ a

..

    Now, if we unified alpha := fmv, we'd get
          [WD] F fmv ~ fmv, [WD] fmv ~ a
    And now we are stuck.

So instead the Fmv Orientation Invariant puts the fmv on the
left, giving
      [WD] fmv ~ alpha, [WD] F alpha ~ fmv, [WD] alpha ~ a

::

    Now we get alpha:=a, and everything works out

..



Note [Eliminate flat-skols]
~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcUnify.hs#L1821>`__

Suppose we have  [G] Num (F [a])
then we flatten to
     [G] Num fsk
     [G] F [a] ~ fsk
where fsk is a flatten-skolem (FlatSkolTv). Suppose we have
      type instance F [a] = a
then we'll reduce the second constraint to
     [G] a ~ fsk
and then replace all uses of 'a' with fsk.  That's bad because
in error messages instead of saying 'a' we'll say (F [a]).  In all
places, including those where the programmer wrote 'a' in the first
place.  Very confusing!  See #7862.

Solution: re-orient a~fsk to fsk~a, so that we preferentially eliminate
the fsk.



Note [Avoid unnecessary swaps]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcUnify.hs#L1839>`__

If we swap without actually improving matters, we can get an infinite loop.
Consider
    work item:  a ~ b
   inert item:  b ~ c
We canonicalise the work-item to (a ~ c).  If we then swap it before
adding to the inert set, we'll add (c ~ a), and therefore kick out the
inert guy, so we get
   new work item:  b ~ c
   inert item:     c ~ a
And now the cycle just repeats



Note [Eliminate younger unification variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcUnify.hs#L1852>`__

Given a choice of unifying
     alpha := beta   or   beta := alpha
we try, if possible, to eliminate the "younger" one, as determined
by `ltUnique`.  Reason: the younger one is less likely to appear free in
an existing inert constraint, and hence we are less likely to be forced
into kicking out and rewriting inert constraints.

This is a performance optimisation only.  It turns out to fix
#14723 all by itself, but clearly not reliably so!

It's simple to implement (see nicer_to_update_tv2 in swapOverTyVars).
But, to my surprise, it didn't seem to make any significant difference
to the compiler's performance, so I didn't take it any further.  Still
it seemed to too nice to discard altogether, so I'm leaving these
notes.  SLPJ Jan 18.

@trySpontaneousSolve wi@ solves equalities where one side is a
touchable unification variable.
Returns True <=> spontaneous solve happened



Note [Prevent unification with type families]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcUnify.hs#L1895>`__

We prevent unification with type families because of an uneasy compromise.
It's perfectly sound to unify with type families, and it even improves the
error messages in the testsuite. It also modestly improves performance, at
least in some cases. But it's disastrous for test case perf/compiler/T3064.
Here is the problem: Suppose we have (F ty) where we also have [G] F ty ~ a.
What do we do? Do we reduce F? Or do we use the given? Hard to know what's
best. GHC reduces. This is a disaster for T3064, where the type's size
spirals out of control during reduction. (We're not helped by the fact that
the flattener re-flattens all the arguments every time around.) If we prevent
unification with type families, then the solver happens to use the equality
before expanding the type family.

It would be lovely in the future to revisit this problem and remove this
extra, unnecessary check. But we retain it for now as it seems to work
better in practice.



Note [Refactoring hazard: checkTauTvUpdate]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcUnify.hs#L1913>`__

I (Richard E.) have a sad story about refactoring this code, retained here
to prevent others (or a future me!) from falling into the same traps.

It all started with #11407, which was caused by the fact that the TyVarTy
case of defer_me didn't look in the kind. But it seemed reasonable to
simply remove the defer_me check instead.

It referred to two Notes (since removed) that were out of date, and the
fast_check code in occurCheckExpand seemed to do just about the same thing as
defer_me. The one piece that defer_me did that wasn't repeated by
occurCheckExpand was the type-family check. (See Note [Prevent unification
with type families].) So I checked the result of occurCheckExpand for any
type family occurrences and deferred if there were any. This was done
in commit e9bf7bb5cc9fb3f87dd05111aa23da76b86a8967 .

This approach turned out not to be performant, because the expanded
type was bigger than the original type, and tyConsOfType (needed to
see if there are any type family occurrences) looks through type
synonyms. So it then struck me that we could dispense with the
defer_me check entirely. This simplified the code nicely, and it cut
the allocations in T5030 by half. But, as documented in Note [Prevent
unification with type families], this destroyed performance in
T3064. Regardless, I missed this regression and the change was
committed as 3f5d1a13f112f34d992f6b74656d64d95a3f506d .

Bottom lines:
 * defer_me is back, but now fixed w.r.t. #11407.
 * Tread carefully before you start to refactor here. There can be
   lots of hard-to-predict consequences.



Note [Type synonyms and the occur check]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcUnify.hs#L1945>`__

Generally speaking we try to update a variable with type synonyms not
expanded, which improves later error messages, unless looking
inside a type synonym may help resolve a spurious occurs check
error. Consider:
          type A a = ()

::

          f :: (A a -> a -> ()) -> ()
          f = \ _ -> ()

..

::

          x :: ()
          x = f (\ x p -> p x)

..

We will eventually get a constraint of the form t ~ A t. The ok function above will
properly expand the type (A t) to just (), which is ok to be unified with t. If we had
unified with the original type A t, we would lead the type checker into an infinite loop.

Hence, if the occurs check fails for a type synonym application, then (and *only* then),
the ok function expands the synonym to detect opportunities for occurs check success using
the underlying definition of the type synonym.

The same applies later on in the constraint interaction code; see TcInteract,
function @occ_check_ok@.



Note [Non-TcTyVars in TcUnify]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcUnify.hs#L1970>`__

Because the same code is now shared between unifying types and unifying
kinds, we sometimes will see proper TyVars floating around the unifier.
Example (from test case polykinds/PolyKinds12):

::

    type family Apply (f :: k1 -> k2) (x :: k1) :: k2
    type instance Apply g y = g y

..

When checking the instance declaration, we first *kind-check* the LHS
and RHS, discovering that the instance really should be

::

    type instance Apply k3 k4 (g :: k3 -> k4) (y :: k3) = g y

..

During this kind-checking, all the tyvars will be TcTyVars. Then, however,
as a second pass, we desugar the RHS (which is done in functions prefixed
with "tc" in TcTyClsDecls"). By this time, all the kind-vars are proper
TyVars, not TcTyVars, get some kind unification must happen.

Thus, we always check if a TyVar is a TcTyVar before asking if it's a
meta-tyvar.

This used to not be necessary for type-checking (that is, before * :: *)
because expressions get desugared via an algorithm separate from
type-checking (with wrappers, etc.). Types get desugared very differently,
causing this wibble in behavior seen here.



Note [Unifying untouchables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcUnify.hs#L2020>`__

We treat an untouchable type variable as if it was a skolem.  That
ensures it won't unify with anything.  It's a slight hack, because
we return a made-up TcTyVarDetails, but I think it works smoothly.



Note [Occurrence checking: look inside kinds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcUnify.hs#L2073>`__

Suppose we are considering unifying
   (alpha :: *)  ~  Int -> (beta :: alpha -> alpha)
This may be an error (what is that alpha doing inside beta's kind?),
but we must not make the mistake of actually unifying or we'll
build an infinite data structure.  So when looking for occurrences
of alpha in the rhs, we must look in the kinds of type variables
that occur there.

NB: we may be able to remove the problem via expansion; see
    Note [Occurs check expansion].  So we have to try that.



Note [Checking for foralls]
~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcUnify.hs#L2086>`__

Unless we have -XImpredicativeTypes (which is a totally unsupported
feature), we do not want to unify
    alpha ~ (forall a. a->a) -> Int
So we look for foralls hidden inside the type, and it's convenient
to do that at the same time as the occurs check (which looks for
occurrences of alpha).

However, it's not just a question of looking for foralls /anywhere/!
Consider
   (alpha :: forall k. k->*)  ~  (beta :: forall k. k->*)
This is legal; e.g. dependent/should_compile/T11635.

We don't want to reject it because of the forall in beta's kind,
but (see Note [Occurrence checking: look inside kinds]) we do
need to look in beta's kind.  So we carry a flag saying if a 'forall'
is OK, and sitch the flag on when stepping inside a kind.

Why is it OK?  Why does it not count as impredicative polymorphism?
The reason foralls are bad is because we reply on "seeing" foralls
when doing implicit instantiation.  But the forall inside the kind is
fine.  We'll generate a kind equality constraint
  (forall k. k->*) ~ (forall k. k->*)
to check that the kinds of lhs and rhs are compatible.  If alpha's
kind had instead been
  (alpha :: kappa)
then this kind equality would rightly complain about unifying kappa
with (forall k. k->*)

