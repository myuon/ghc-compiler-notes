[[src]](https://github.com/ghc/ghc/tree/master/compiler/coreSyn/CoreUnfold.hs)

(c) The University of Glasgow 2006
(c) The AQUA Project, Glasgow University, 1994-1998


Core-syntax unfoldings

Unfoldings (which can travel across module boundaries) are in Core
syntax (namely @CoreExpr@s).

The type @Unfolding@ sits ``above'' simply-Core-expressions
unfoldings, capturing ``higher-level'' things we know about a binding,
usually things that the simplifier found out (e.g., ``it's a
literal'').  In the corner of a @CoreUnfolding@ unfolding, you will
find, unsurprisingly, a Core expression.


# \subsection{Making unfoldings}


### Note: Specialising unfoldings

When we specialise a function for some given type-class arguments, we use
specUnfolding to specialise its unfolding.  Some important points:

* If the original function has a DFunUnfolding, the specialised one
  must do so too!  Otherwise we lose the magic rules that make it
  interact with ClassOps

* There is a bit of hack for INLINABLE functions:
     f :: Ord a => ....
     f = <big-rhs>
     {- INLINABLE f #

### Note: Occurrence analysis of unfoldings

We do occurrence-analysis of unfoldings once and for all, when the
unfolding is built, rather than each time we inline them.

But given this decision it's vital that we do
*always* do it.  Consider this unfolding
    \x -> letrec { f = ...g...; g* = f } in body
where g* is (for some strange reason) the loop breaker.  If we don't
occ-anal it when reading it in, we won't mark g as a loop breaker, and
we may inline g entirely in body, dropping its binding, and leaving
the occurrence in f out of scope. This happened in Trac #8892, where
the unfolding in question was a DFun unfolding.

But more generally, the simplifier is designed on the
basis that it is looking at occurrence-analysed expressions, so better
ensure that they acutally are.

### Note: Calculate unfolding guidance on the non-occ-anal'd expression

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

# \subsection{The UnfoldingGuidance type}


### Note: Computing the size of an expression

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

### Note: Do not inline top-level bottoming functions

### Note: Bottoming floats

### Note: INLINE for small functions

### Note: Constructor size and result discount

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
terrible code bloat: see Trac #6099.

### Note: Unboxed tuple size and result discount

### Note: Function and non-function discounts

We want a discount if the function is applied. A good example is
monadic combinators with continuation arguments, where inlining is
quite important.

But we don't want a big discount when a function is called many times
(see the detailed comments with Trac #6048) because if the function is
big it won't be inlined at its many call sites and no benefit results.
Indeed, we can get exponentially big inlinings this way; that is what
Trac #6048 is about.

On the other hand, for data-valued arguments, if there are lots of
case expressions in the body, each one will get smaller if we apply
the function to a constructor application, so we *want* a big discount
if the argument is scrutinised by many case expressions.

Conclusion:
  - For functions, take the max of the discounts
  - For data values, take the sum of the discounts

### Note: Literal integer size

Literal integers *can* be big (mkInteger [...coefficients...]), but
need not be (S# n).  We just use an arbitrary big-ish constant here
so that, in particular, we don't inline top-level defns like
   n = S# 5
There's no point in doing so -- any optimisations will see the S#
through n's unfolding.  Nor will a big size inhibit unfoldings functions
that mention a literal Integer, because the float-out pass will float
all those constants to top level.


### Note: addAltSize result discounts

When adding the size of alternatives, we *add* the result discounts
too, rather than take the *maximum*.  For a multi-branch case, this
gives a discount for each branch that returns a constructor, making us
keener to inline.  I did try using 'max' instead, but it makes nofib
'rewrite' and 'puzzle' allocate significantly more, and didn't make
binary sizes shrink significantly either.

### Note: Discounts and thresholds

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

### Note: Function applications

In a function application (f a b)

  - If 'f' is an argument to the function being analysed,
    and there's at least one value arg, record a FunAppDiscount for f

  - If the application if a PAP (arity > 2 in this example)
    record a *result* discount (because inlining
    with "extra" args in the call may mean that we now
    get a saturated application)

Code for manipulating sizes


# \subsection[considerUnfolding]{Given all the info, do (not) do the unfolding}


We use 'couldBeSmallEnoughToInline' to avoid exporting inlinings that
we ``couldn't possibly use'' on the other side.  Can be overridden w/
flaggery.  Just the same as smallEnoughToInline, except that it has no
actual arguments.


### Note: certainlyWillInline: be careful of thunks

Don't claim that thunks will certainly inline, because that risks work
duplication.  Even if the work duplication is not great (eg is_cheap
holds), it can make a big difference in an inner loop In Trac #5623 we
found that the WorkWrap phase thought that
       y = case x of F# v -> F# (v +# v)
was certainlyWillInline, so the addition got duplicated.

### Note: certainlyWillInline: INLINABLE

### Note: Worker-wrapper for INLINABLE functions

# \subsection{callSiteInline}


This is the key function.  It decides whether to inline a variable at a call site

callSiteInline is used at call sites, so it is a bit more generous.
It's a very important function that embodies lots of heuristics.
A non-WHNF can be inlined if it doesn't occur inside a lambda,
and occurs exactly once or
    occurs once in each branch of a case and is small

If the thing is in WHNF, there's no danger of duplicating work,
so we can inline if it occurs once, or is small

NOTE: we don't want to inline top-level functions that always diverge.
It just makes the code bigger.  Tt turns out that the convenient way to prevent
them inlining is to give them a NOINLINE pragma, which we do in
StrictAnal.addStrictnessInfoToTopId


### Note: Unfold into lazy contexts], Note [RHS of lets

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

### Note: Unsaturated applications

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
                     GHC.Types.False -> y GHC.Types.True -> x }) 