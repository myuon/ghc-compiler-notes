`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/coreSyn/CoreUnfold.hs>`_

Note [Top-level flag on inline rules]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Slight hack: note that mk_inline_rules conservatively sets the
top-level flag to True.  It gets set more accurately by the simplifier
Simplify.simplUnfolding.


Note [Specialising unfoldings]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we specialise a function for some given type-class arguments, we use
specUnfolding to specialise its unfolding.  Some important points:

* If the original function has a DFunUnfolding, the specialised one
  must do so too!  Otherwise we lose the magic rules that make it
  interact with ClassOps

* There is a bit of hack for INLINABLE functions:
     f :: Ord a => ....
     f = <big-rhs>
     {- INLINABLE f #-}
  Now if we specialise f, should the specialised version still have
  an INLINABLE pragma?  If it does, we'll capture a specialised copy
  of <big-rhs> as its unfolding, and that probaby won't inline.  But
  if we don't, the specialised version of <big-rhs> might be small
  enough to inline at a call site. This happens with Control.Monad.liftM3,
  and can cause a lot more allocation as a result (nofib n-body shows this).

  Moreover, keeping the INLINABLE thing isn't much help, because
  the specialised function (probaby) isn't overloaded any more.

  Conclusion: drop the INLINEALE pragma.  In practice what this means is:
     if a stable unfolding has UnfoldingGuidance of UnfWhen,
        we keep it (so the specialised thing too will always inline)
     if a stable unfolding has UnfoldingGuidance of UnfIfGoodArgs
        (which arises from INLINABLE), we discard it



Note [Honour INLINE on 0-ary bindings]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider

   x = <expensive>
   {-# INLINE x #-}

   f y = ...x...

The semantics of an INLINE pragma is

  inline x at every call site, provided it is saturated;
  that is, applied to at least as many arguments as appear
  on the LHS of the Haskell source definition.

(This soure-code-derived arity is stored in the `ug_arity` field of
the `UnfoldingGuidance`.)

In the example, x's ug_arity is 0, so we should inline it at every use
site.  It's rare to have such an INLINE pragma (usually INLINE Is on
functions), but it's occasionally very important (#15578, #15519).
In #15519 we had something like
   x = case (g a b) of I# r -> T r
   {-# INLINE x #-}
   f y = ...(h x)....

where h is strict.  So we got
   f y = ...(case g a b of I# r -> h (T r))...

and that in turn allowed SpecConstr to ramp up performance.

How do we deliver on this?  By adjusting the ug_boring_ok
flag in mkInlineUnfoldingWithArity; see
Note [INLINE pragmas and boring contexts]

NB: there is a real risk that full laziness will float it right back
out again. Consider again
  x = factorial 200
  {-# INLINE x #-}
  f y = ...x...

After inlining we get
  f y = ...(factorial 200)...

but it's entirely possible that full laziness will do
  lvl23 = factorial 200
  f y = ...lvl23...

That's a problem for another day.



Note [INLINE pragmas and boring contexts]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
An INLINE pragma uses mkInlineUnfoldingWithArity to build the
unfolding.  That sets the ug_boring_ok flag to False if the function
is not tiny (inlineBoringOK), so that even INLINE functions are not
inlined in an utterly boring context.  E.g.
     \x y. Just (f y x)
Nothing is gained by inlining f here, even if it has an INLINE
pragma.

But for 0-ary bindings, we want to inline regardless; see
Note [Honour INLINE on 0-ary bindings].

I'm a bit worried that it's possible for the same kind of problem
to arise for non-0-ary functions too, but let's wait and see.


Note [Occurrence analysis of unfoldings]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We do occurrence-analysis of unfoldings once and for all, when the
unfolding is built, rather than each time we inline them.

But given this decision it's vital that we do
*always* do it.  Consider this unfolding
    \x -> letrec { f = ...g...; g* = f } in body
where g* is (for some strange reason) the loop breaker.  If we don't
occ-anal it when reading it in, we won't mark g as a loop breaker, and
we may inline g entirely in body, dropping its binding, and leaving
the occurrence in f out of scope. This happened in #8892, where
the unfolding in question was a DFun unfolding.

But more generally, the simplifier is designed on the
basis that it is looking at occurrence-analysed expressions, so better
ensure that they acutally are.

We use occurAnalyseExpr_NoBinderSwap instead of occurAnalyseExpr;
see Note [No binder swap in unfoldings].



Note [No binder swap in unfoldings]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The binder swap can temporarily violate Core Lint, by assinging
a LocalId binding to a GlobalId. For example, if A.foo{r872}
is a GlobalId with unique r872, then

 case A.foo{r872} of bar {
   K x -> ...(A.foo{r872})...
 }

gets transformed to

  case A.foo{r872} of bar {
    K x -> let foo{r872} = bar
           in ...(A.foo{r872})...

This is usually not a problem, because the simplifier will transform
this to:

  case A.foo{r872} of bar {
    K x -> ...(bar)...

However, after occurrence analysis but before simplification, this extra 'let'
violates the Core Lint invariant that we do not have local 'let' bindings for
GlobalIds.  That seems (just) tolerable for the occurrence analysis that happens
just before the Simplifier, but not for unfoldings, which are Linted
independently.
As a quick workaround, we disable binder swap in this module.
See #16288 and #16296 for further plans.



Note [Calculate unfolding guidance on the non-occ-anal'd expression]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Notice that we give the non-occur-analysed expression to
calcUnfoldingGuidance.  In some ways it'd be better to occur-analyse
first; for example, sometimes during simplification, there's a large
let-bound thing which has been substituted, and so is now dead; so
'expr' contains two copies of the thing while the occurrence-analysed
expression doesn't.

Nevertheless, we *don't* and *must not* occ-analyse before computing
the size because

a) The size computation bales out after a while, whereas occurrence
   analysis does not.

b) Residency increases sharply if you occ-anal first.  I'm not
   100% sure why, but it's a large effect.  Compiling Cabal went
   from residency of 534M to over 800M with this one change.

This can occasionally mean that the guidance is very pessimistic;
it gets fixed up next round.  And it should be rare, because large
let-bound things that are dead are usually caught by preInlineUnconditionally




Note [Computing the size of an expression]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The basic idea of sizeExpr is obvious enough: count nodes.  But getting the
heuristics right has taken a long time.  Here's the basic strategy:

    * Variables, literals: 0
      (Exception for string literals, see litSize.)

    * Function applications (f e1 .. en): 1 + #value args

    * Constructor applications: 1, regardless of #args

    * Let(rec): 1 + size of components

    * Note, cast: 0

Examples

  Size  Term
  --------------
    0     42#
    0     x
    0     True
    2     f x
    1     Just x
    4     f (g x)

Notice that 'x' counts 0, while (f x) counts 2.  That's deliberate: there's
a function call to account for.  Notice also that constructor applications
are very cheap, because exposing them to a caller is so valuable.

[25/5/11] All sizes are now multiplied by 10, except for primops
(which have sizes like 1 or 4.  This makes primops look fantastically
cheap, and seems to be almost unversally beneficial.  Done partly as a
result of #4978.



Note [Do not inline top-level bottoming functions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The FloatOut pass has gone to some trouble to float out calls to 'error'
and similar friends.  See Note [Bottoming floats] in SetLevels.
Do not re-inline them!  But we *do* still inline if they are very small
(the uncondInline stuff).



Note [INLINE for small functions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider        {-# INLINE f #-}
                f x = Just x
                g y = f y
Then f's RHS is no larger than its LHS, so we should inline it into
even the most boring context.  In general, f the function is
sufficiently small that its body is as small as the call itself, the
inline unconditionally, regardless of how boring the context is.

Things to note:

(1) We inline *unconditionally* if inlined thing is smaller (using sizeExpr)
    than the thing it's replacing.  Notice that
      (f x) --> (g 3)             -- YES, unconditionally
      (f x) --> x : []            -- YES, *even though* there are two
                                  --      arguments to the cons
      x     --> g 3               -- NO
      x     --> Just v            -- NO

    It's very important not to unconditionally replace a variable by
    a non-atomic term.

(2) We do this even if the thing isn't saturated, else we end up with the
    silly situation that
       f x y = x
       ...map (f 3)...
    doesn't inline.  Even in a boring context, inlining without being
    saturated will give a lambda instead of a PAP, and will be more
    efficient at runtime.

(3) However, when the function's arity > 0, we do insist that it
    has at least one value argument at the call site.  (This check is
    made in the UnfWhen case of callSiteInline.) Otherwise we find this:
         f = /\a \x:a. x
         d = /\b. MkD (f b)
    If we inline f here we get
         d = /\b. MkD (\x:b. x)
    and then prepareRhs floats out the argument, abstracting the type
    variables, so we end up with the original again!

(4) We must be much more cautious about arity-zero things. Consider
       let x = y +# z in ...
    In *size* terms primops look very small, because the generate a
    single instruction, but we do not want to unconditionally replace
    every occurrence of x with (y +# z).  So we only do the
    unconditional-inline thing for *trivial* expressions.

    NB: you might think that PostInlineUnconditionally would do this
    but it doesn't fire for top-level things; see SimplUtils
    Note [Top level and postInlineUnconditionally]


Note [Constructor size and result discount]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Treat a constructors application as size 10, regardless of how many
arguments it has; we are keen to expose them (and we charge separately
for their args).  We can't treat them as size zero, else we find that
(Just x) has size 0, which is the same as a lone variable; and hence
'v' will always be replaced by (Just x), where v is bound to Just x.

The "result discount" is applied if the result of the call is
scrutinised (say by a case).  For a constructor application that will
mean the constructor application will disappear, so we don't need to
charge it to the function.  So the discount should at least match the
cost of the constructor application, namely 10.  But to give a bit
of extra incentive we give a discount of 10*(1 + n_val_args).

Simon M tried a MUCH bigger discount: (10 * (10 + n_val_args)),
and said it was an "unambiguous win", but its terribly dangerous
because a function with many many case branches, each finishing with
a constructor, can have an arbitrarily large discount.  This led to
terrible code bloat: see #6099.



Note [Unboxed tuple size and result discount]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
However, unboxed tuples count as size zero. I found occasions where we had
        f x y z = case op# x y z of { s -> (# s, () #) }
and f wasn't getting inlined.

I tried giving unboxed tuples a *result discount* of zero (see the
commented-out line).  Why?  When returned as a result they do not
allocate, so maybe we don't want to charge so much for them If you
have a non-zero discount here, we find that workers often get inlined
back into wrappers, because it look like
    f x = case $wf x of (# a,b #) -> (a,b)
and we are keener because of the case.  However while this change
shrank binary sizes by 0.5% it also made spectral/boyer allocate 5%
more. All other changes were very small. So it's not a big deal but I
didn't adopt the idea.



Note [Function and non-function discounts]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We want a discount if the function is applied. A good example is
monadic combinators with continuation arguments, where inlining is
quite important.

But we don't want a big discount when a function is called many times
(see the detailed comments with #6048) because if the function is
big it won't be inlined at its many call sites and no benefit results.
Indeed, we can get exponentially big inlinings this way; that is what
#6048 is about.

On the other hand, for data-valued arguments, if there are lots of
case expressions in the body, each one will get smaller if we apply
the function to a constructor application, so we *want* a big discount
if the argument is scrutinised by many case expressions.

Conclusion:
  - For functions, take the max of the discounts
  - For data values, take the sum of the discounts




Note [Literal integer size]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
Literal integers *can* be big (mkInteger [...coefficients...]), but
need not be (S# n).  We just use an arbitrary big-ish constant here
so that, in particular, we don't inline top-level defns like
   n = S# 5
There's no point in doing so -- any optimisations will see the S#
through n's unfolding.  Nor will a big size inhibit unfoldings functions
that mention a literal Integer, because the float-out pass will float
all those constants to top level.


Note [addAltSize result discounts]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When adding the size of alternatives, we *add* the result discounts
too, rather than take the *maximum*.  For a multi-branch case, this
gives a discount for each branch that returns a constructor, making us
keener to inline.  I did try using 'max' instead, but it makes nofib
'rewrite' and 'puzzle' allocate significantly more, and didn't make
binary sizes shrink significantly either.



Note [Discounts and thresholds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Constants for discounts and thesholds are defined in main/DynFlags,
all of form ufXxxx.   They are:

ufCreationThreshold
     At a definition site, if the unfolding is bigger than this, we
     may discard it altogether

ufUseThreshold
     At a call site, if the unfolding, less discounts, is smaller than
     this, then it's small enough inline

ufKeenessFactor
     Factor by which the discounts are multiplied before
     subtracting from size

ufDictDiscount
     The discount for each occurrence of a dictionary argument
     as an argument of a class method.  Should be pretty small
     else big functions may get inlined

ufFunAppDiscount
     Discount for a function argument that is applied.  Quite
     large, because if we inline we avoid the higher-order call.

ufDearOp
     The size of a foreign call or not-dupable PrimOp

ufVeryAggressive
     If True, the compiler ignores all the thresholds and inlines very
     aggressively. It still adheres to arity, simplifier phase control and
     loop breakers.




Note [Function applications]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In a function application (f a b)

  - If 'f' is an argument to the function being analysed,
    and there's at least one value arg, record a FunAppDiscount for f

  - If the application if a PAP (arity > 2 in this example)
    record a *result* discount (because inlining
    with "extra" args in the call may mean that we now
    get a saturated application)

Code for manipulating sizes


Note [certainlyWillInline: be careful of thunks]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Don't claim that thunks will certainly inline, because that risks work
duplication.  Even if the work duplication is not great (eg is_cheap
holds), it can make a big difference in an inner loop In #5623 we
found that the WorkWrap phase thought that
       y = case x of F# v -> F# (v +# v)
was certainlyWillInline, so the addition got duplicated.



Note [certainlyWillInline: INLINABLE]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
certainlyWillInline /must/ return Nothing for a large INLINABLE thing,
even though we have a stable inlining, so that strictness w/w takes
place.  It makes a big difference to efficiency, and the w/w pass knows
how to transfer the INLINABLE info to the worker; see WorkWrap
Note [Worker-wrapper for INLINABLE functions]



Note [Unfold into lazy contexts], Note [RHS of lets]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When the call is the argument of a function with a RULE, or the RHS of a let,
we are a little bit keener to inline.  For example
     f y = (y,y,y)
     g y = let x = f y in ...(case x of (a,b,c) -> ...) ...
We'd inline 'f' if the call was in a case context, and it kind-of-is,
only we can't see it.  Also
     x = f v
could be expensive whereas
     x = case v of (a,b) -> a
is patently cheap and may allow more eta expansion.
So we treat the RHS of a let as not-totally-boring.



Note [Unsaturated applications]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When a call is not saturated, we *still* inline if one of the
arguments has interesting structure.  That's sometimes very important.
A good example is the Ord instance for Bool in Base:

 Rec {
    $fOrdBool =GHC.Classes.D:Ord
                 @ Bool
                 ...
                 $cmin_ajX

    $cmin_ajX [Occ=LoopBreaker] :: Bool -> Bool -> Bool
    $cmin_ajX = GHC.Classes.$dmmin @ Bool $fOrdBool
  }

But the defn of GHC.Classes.$dmmin is:

  $dmmin :: forall a. GHC.Classes.Ord a => a -> a -> a
    {- Arity: 3, HasNoCafRefs, Strictness: SLL,
       Unfolding: (\ @ a $dOrd :: GHC.Classes.Ord a x :: a y :: a ->
                   case @ a GHC.Classes.<= @ a $dOrd x y of wild {
                     GHC.Types.False -> y GHC.Types.True -> x }) -}

We *really* want to inline $dmmin, even though it has arity 3, in
order to unravel the recursion.




Note [Things to watch]
~~~~~~~~~~~~~~~~~~~~~~
*   { y = I# 3; x = y `cast` co; ...case (x `cast` co) of ... }
    Assume x is exported, so not inlined unconditionally.
    Then we want x to inline unconditionally; no reason for it
    not to, and doing so avoids an indirection.

*   { x = I# 3; ....f x.... }
    Make sure that x does not inline unconditionally!
    Lest we get extra allocation.



Note [Inlining an InlineRule]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
An InlineRules is used for
  (a) programmer INLINE pragmas
  (b) inlinings from worker/wrapper

For (a) the RHS may be large, and our contract is that we *only* inline
when the function is applied to all the arguments on the LHS of the
source-code defn.  (The uf_arity in the rule.)

However for worker/wrapper it may be worth inlining even if the
arity is not satisfied (as we do in the CoreUnfolding case) so we don't
require saturation.



Note [Nested functions]
~~~~~~~~~~~~~~~~~~~~~~~
At one time we treated a call of a non-top-level function as
"interesting" (regardless of how boring the context) in the hope
that inlining it would eliminate the binding, and its allocation.
Specifically, in the default case of interesting_call we had
   _other -> not is_top && uf_arity > 0

But actually postInlineUnconditionally does some of this and overall
it makes virtually no difference to nofib.  So I simplified away this
special case



Note [Cast then apply]
~~~~~~~~~~~~~~~~~~~~~~
Consider
   myIndex = __inline_me ( (/\a. <blah>) |> co )
   co :: (forall a. a -> a) ~ (forall a. T a)
     ... /\a.\x. case ((myIndex a) |> sym co) x of { ... } ...

We need to inline myIndex to unravel this; but the actual call (myIndex a) has
no value arguments.  The ValAppCtxt gives it enough incentive to inline.



Note [Inlining in ArgCtxt]
~~~~~~~~~~~~~~~~~~~~~~~~~~
The condition (arity > 0) here is very important, because otherwise
we end up inlining top-level stuff into useless places; eg
   x = I# 3#
   f = \y.  g x
This can make a very big difference: it adds 16% to nofib 'integer' allocs,
and 20% to 'power'.

At one stage I replaced this condition by 'True' (leading to the above
slow-down).  The motivation was test eyeball/inline1.hs; but that seems
to work ok now.

NOTE: arguably, we should inline in ArgCtxt only if the result of the
call is at least CONLIKE.  At least for the cases where we use ArgCtxt
for the RHS of a 'let', we only profit from the inlining if we get a
CONLIKE thing (modulo lets).

Note [Lone variables]   See also Note [Interaction of exprIsWorkFree and lone variables]
~~~~~~~~~~~~~~~~~~~~~   which appears below
The "lone-variable" case is important.  I spent ages messing about
with unsatisfactory variants, but this is nice.  The idea is that if a
variable appears all alone

        as an arg of lazy fn, or rhs    BoringCtxt
        as scrutinee of a case          CaseCtxt
        as arg of a fn                  ArgCtxt
AND
        it is bound to a cheap expression

then we should not inline it (unless there is some other reason,
e.g. it is the sole occurrence).  That is what is happening at
the use of 'lone_variable' in 'interesting_call'.

Why?  At least in the case-scrutinee situation, turning
        let x = (a,b) in case x of y -> ...
into
        let x = (a,b) in case (a,b) of y -> ...
and thence to
        let x = (a,b) in let y = (a,b) in ...
is bad if the binding for x will remain.

Another example: I discovered that strings
were getting inlined straight back into applications of 'error'
because the latter is strict.
        s = "foo"
        f = \x -> ...(error s)...

Fundamentally such contexts should not encourage inlining because, provided
the RHS is "expandable" (see Note [exprIsExpandable] in CoreUtils) the
context can ``see'' the unfolding of the variable (e.g. case or a
RULE) so there's no gain.

However, watch out:

 * Consider this:
        foo = _inline_ (\n. [n])
        bar = _inline_ (foo 20)
        baz = \n. case bar of { (m:_) -> m + n }
   Here we really want to inline 'bar' so that we can inline 'foo'
   and the whole thing unravels as it should obviously do.  This is
   important: in the NDP project, 'bar' generates a closure data
   structure rather than a list.

   So the non-inlining of lone_variables should only apply if the
   unfolding is regarded as cheap; because that is when exprIsConApp_maybe
   looks through the unfolding.  Hence the "&& is_wf" in the
   InlineRule branch.

 * Even a type application or coercion isn't a lone variable.
   Consider
        case $fMonadST @ RealWorld of { :DMonad a b c -> c }
   We had better inline that sucker!  The case won't see through it.

   For now, I'm treating treating a variable applied to types
   in a *lazy* context "lone". The motivating example was
        f = /\a. \x. BIG
        g = /\a. \y.  h (f a)
   There's no advantage in inlining f here, and perhaps
   a significant disadvantage.  Hence some_val_args in the Stop case



Note [Interaction of exprIsWorkFree and lone variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The lone-variable test says "don't inline if a case expression
scrutinises a lone variable whose unfolding is cheap".  It's very
important that, under these circumstances, exprIsConApp_maybe
can spot a constructor application. So, for example, we don't
consider
        let x = e in (x,x)
to be cheap, and that's good because exprIsConApp_maybe doesn't
think that expression is a constructor application.

In the 'not (lone_variable && is_wf)' test, I used to test is_value
rather than is_wf, which was utterly wrong, because the above
expression responds True to exprIsHNF, which is what sets is_value.

This kind of thing can occur if you have

        {-# INLINE foo #-}
        foo = let x = e in (x,x)

which Roman did.



