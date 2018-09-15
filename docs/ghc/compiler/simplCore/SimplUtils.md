[[src]](https://github.com/ghc/ghc/tree/master/compiler/simplCore/SimplUtils.hs)

(c) The AQUA Project, Glasgow University, 1993-1998

# The simplifier utilities

# The SimplCont and DupFlag types


A SimplCont allows the simplifier to traverse the expression in a
zipper-like fashion.  The SimplCont represents the rest of the expression,
"above" the point of interest.

You can also think of a SimplCont as an "evaluation context", using
that term in the way it is used for operational semantics. This is the
way I usually think of it, For example you'll often see a syntax for
evaluation context looking like
        C ::= []  |  C e   |  case C of alts  |  C `cast` co
That's the kind of thing we are doing here, and I use that syntax in
the comments.


Key points:
  * A SimplCont describes a *strict* context (just like
    evaluation contexts do).  E.g. Just [] is not a SimplCont

  * A SimplCont describes a context that *does not* bind
    any variables.  E.g. \x. [] is not a SimplCont


### Note: StaticEnv invariant

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

### Note: DupFlag invariants

In both (ApplyToVal dup _ env k)
   and  (Select dup _ _ env k)
the following invariants hold

  (a) if dup = OkToDup, then continuation k is also ok-to-dup
  (b) if dup = OkToDup or Simplified, the subst-env is empty
      (and and hence no need to re-simplify)


### Note: The hole type in ApplyToTy

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

# ArgInfo and ArgSpec


# Functions on SimplCont


### Note: Unsaturated functions

Consider (test eyeball/inline4)
        x = a:as
        y = f x
where f has arity 2.  Then we do not want to inline 'x', because
it'll just be floated out again.  Even if f has lots of discounts
on its first argument -- it must be saturated for these to kick in


# Interesting arguments


### Note: Interesting call context

We want to avoid inlining an expression where there can't possibly be
any gain, such as in an argument position.  Hence, if the continuation
is interesting (eg. a case scrutinee, application etc.) then we
inline, otherwise we don't.

Previously some_benefit used to return True only if the variable was
applied to some value arguments.  This didn't work:

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

        case x of y { DEFAULT -> ... }

since we can just eliminate this case instead (x is in WHNF).  Similar
applies when x is bound to a lambda expression.  Hence
contIsInteresting looks for case expressions with just a single
default case.

### Note: No case of case is boring

If we see
   case f x of <alts>

we'd usually treat the context as interesting, to encourage 'f' to
inline.  But if case-of-case is off, it's really not so interesting
after all, because we are unlikely to be able to push the case
expression into the branches of any case in f's unfolding.  So, to
reduce unnecessary code expansion, we just make the context look boring.
This made a small compile-time perf improvement in perf/compiler/T6048,
and it looks plausible to me.


### Note: Interesting arguments

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
at all; i.e. (hasSomeUnfolding v).  But does too much inlining; see Trac #3016.

But we don't regard (f x y) as interesting, unless f is unsaturated.
If it's saturated and f hasn't inlined, then it's probably not going
to now!

### Note: Conlike is interesting

Consider
        f d = ...((*) d x y)...
        ... f (df d')...
where df is con-like. Then we'd really like to inline 'f' so that the
rule for (*) (df d) can fire.  To do this
  a) we give a discount for being an argument of a class-op (eg (*) d)
  b) we say that a con-like argument (eg (df d)) is interesting


# SimplMode


The SimplMode controls several switches; see its definition in
CoreMonad
        sm_rules      :: Bool     -- Whether RULES are enabled
        sm_inline     :: Bool     -- Whether inlining is enabled
        sm_case_case  :: Bool     -- Whether case-of-case is enabled
        sm_eta_expand :: Bool     -- Whether eta-expansion is enabled


### Note: Simplifying rules

When simplifying a rule LHS, refrain from /any/ inlining or applying
of other RULES.

Doing anything to the LHS is plain confusing, because it means that what the
rule matches is not what the user wrote. c.f. Trac #10595, and #10528.
Moreover, inlining (or applying rules) on rule LHSs risks introducing
Ticks into the LHS, which makes matching trickier. Trac #10665, #10745.

Doing this to either side confounds tools like HERMIT, which seek to reason
about and apply the RULES as originally written. See Trac #10829.

### Note: No eta expansion in stable unfoldings

If we have a stable unfolding

  f :: Ord a => a -> IO ()
  -- Unfolding template
  --    = /\a \(d:Ord a) (x:a). bla

we do not want to eta-expand to

  f :: Ord a => a -> IO ()
  -- Unfolding template
  --    = (/\a \(d:Ord a) (x:a) (eta:State#). bla eta) |> co

### Note: Specialisation shape

So we disable eta-expansion in stable unfoldings.

### Note: Inlining in gentle mode

Something is inlined if
   (i)   the sm_inline flag is on, AND
   (ii)  the thing has an INLINE pragma, AND
   (iii) the thing is inlinable in the earliest phase.

# preInlineUnconditionally


preInlineUnconditionally
~~~~~~~~~~~~~~~~~~~~~~~~

@preInlineUnconditionally@ examines a bndr to see if it is used just
once in a completely safe way, so that it is safe to discard the
binding inline its RHS at the (unique) usage site, REGARDLESS of how
big the RHS might be.  If this is the case we don't simplify the RHS
first, but just inline it un-simplified.

This is much better than first simplifying a perhaps-huge RHS and then
inlining and re-simplifying it.  Indeed, it can be at least quadratically
better.  Consider

        x1 = e1
        x2 = e2[x1]
        x3 = e3[x2]
        ...etc...
        xN = eN[xN-1]

We may end up simplifying e1 N times, e2 N-1 times, e3 N-3 times etc.
This can happen with cascades of functions too:

        f1 = \x1.e1
        f2 = \xs.e2[f1]
        f3 = \xs.e3[f3]
        ...etc...

THE MAIN INVARIANT is this:

        ----  preInlineUnconditionally invariant -----
   IF preInlineUnconditionally chooses to inline x = <rhs>
   THEN doing the inlining should not change the occurrence
        info for the free vars of <rhs>
        ----------------------------------------------

For example, it's tempting to look at trivial binding like
        x = y
and inline it unconditionally.  But suppose x is used many times,
but this is the unique occurrence of y.  Then inlining x would change
y's occurrence info, which breaks the invariant.  It matters: y
might have a BIG rhs, which will now be dup'd at every occurrenc of x.


Even RHSs labelled InlineMe aren't caught here, because there might be
no benefit from inlining at the call site.

[Sept 01] Don't unconditionally inline a top-level thing, because that
can simply make a static thing into something built dynamically.  E.g.
        x = (a,b)
        main = \s -> h x

[Remember that we treat \s as a one-shot lambda.]  No point in
inlining x unless there is something interesting about the call site.

But watch out: if you aren't careful, some useful foldr/build fusion
can be lost (most notably in spectral/hartel/parstof) because the
foldr didn't see the build.  Doing the dynamic allocation isn't a big
deal, in fact, but losing the fusion can be.  But the right thing here
seems to be to do a callSiteInline based on the fact that there is
something interesting about the call site (it's strict).  Hmm.  That
seems a bit fragile.

Conclusion: inline top level things gaily until Phase 0 (the last
phase), at which point don't.

### Note: pre/postInlineUnconditionally in gentle mode

Even in gentle mode we want to do preInlineUnconditionally.  The
reason is that too little clean-up happens if you don't inline
use-once things.  Also a bit of inlining is *good* for full laziness;
it can expose constant sub-expressions.  Example in
spectral/mandel/Mandel.hs, where the mandelset function gets a useful
let-float if you inline windowToViewport

### Note: Gentle mode

### Note: Stable unfoldings and preInlineUnconditionally

Surprisingly, do not pre-inline-unconditionally Ids with INLINE pragmas!
Example

# postInlineUnconditionally


postInlineUnconditionally
~~~~~~~~~~~~~~~~~~~~~~~~~

@postInlineUnconditionally@ decides whether to unconditionally inline
a thing based on the form of its RHS; in particular if it has a
trivial RHS.  If so, we can inline and discard the binding altogether.

NB: a loop breaker has must_keep_binding = True and non-loop-breakers
only have *forward* references. Hence, it's safe to discard the binding

NOTE: This isn't our last opportunity to inline.  We're at the binding
site right now, and we'll get another opportunity when we get to the
occurrence(s)

Note that we do this unconditional inlining only for trival RHSs.
Don't inline even WHNFs inside lambdas; doing so may simply increase
allocation when the function is called. This isn't the last chance; see
NOTE above.

NB: Even inline pragmas (e.g. IMustBeINLINEd) are ignored here Why?
Because we don't even want to inline them into the RHS of constructor
arguments. See NOTE above

NB: At one time even NOINLINE was ignored here: if the rhs is trivial
it's best to inline it anyway.  We often get a=E; b=a from desugaring,
with both a and b marked NOINLINE.  But that seems incompatible with
our new view that inlining is like a RULE, so I'm sticking to the 'active'
story for now.


### Note: Top level and postInlineUnconditionally

We don't do postInlineUnconditionally for top-level things (even for
ones that are trivial):

  * Doing so will inline top-level error expressions that have been
    carefully floated out by FloatOut.  More generally, it might
    replace static allocation with dynamic.

### Note: Eta expanding lambdas

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

### Note: No eta expansion in stable unfoldings

### Note: Casts and lambdas

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

# Eta expansion


### Note: Eta-expanding at let bindings

We now eta expand at let-bindings, which is where the payoff comes.
The most significant thing is that we can do a simple arity analysis
(in CoreArity.findRhsArity), which we can't do for free-floating lambdas

### Note: Abstract over coercions

If a coercion variable (g :: a ~ Int) is free in the RHS, then so is the
type variable a.  Rather than sort this mess out, we simply bale out and abstract
wrt all the type variables if any of them are coercion variables.


Historical note: if you use let-bindings instead of a substitution, beware of this:

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

# prepareAlts


prepareAlts tries these things:

1.  Eliminate alternatives that cannot match, including the
    DEFAULT alternative.

2.  If the DEFAULT alternative can match only one possible constructor,
    then make that constructor explicit.
    e.g.
        case e of x { DEFAULT -> rhs }
     ===>
        case e of x { (a,b) -> rhs }
    where the type is a single constructor type.  This gives better code
    when rhs also scrutinises x or e.

3. Returns a list of the constructors that cannot holds in the
   DEFAULT alternative (if there is one)

Here "cannot match" includes knowledge from GADTs

It's a good idea to do this stuff before simplifying the alternatives, to
avoid simplifying alternatives we know can't happen, and to come up with
the list of constructors that are handled, to put into the IdInfo of the
case binder, for use when simplifying the alternatives.

Eliminating the default alternative in (1) isn't so obvious, but it can
happen:

data Colour = Red | Green | Blue

f x = case x of
        Red -> ..
        Green -> ..
        DEFAULT -> h x

h y = case y of
        Blue -> ..
        DEFAULT -> [ case y of ... ]

If we inline h into f, the default case of the inlined h can't happen.
If we don't notice this, we may end up filtering out *all* the cases
of the inner case y, which give us nowhere to go!


# mkCase


mkCase tries these things

### Note: Nerge nested cases

### Note: Merge Nested Cases

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

### Note: Eliminate Identity Case

        case e of               ===> e
                True  -> True;
                False -> False

and similar friends.

### Note: Scrutinee Constant Folding

     case x op# k# of _ {  ===> case x of _ {
        a1# -> e1                  (a1# inv_op# k#) -> e1
        a2# -> e2                  (a2# inv_op# k#) -> e2
        ...                        ...
        DEFAULT -> ed              DEFAULT -> ed

     where (x op# k#) inv_op# k# == x

And similarly for commuted arguments and for some unary operations.

The purpose of this transformation is not only to avoid an arithmetic
operation at runtime but to allow other transformations to apply in cascade.

Example with the "Merge Nested Cases" optimization (from #12877):

      main = case t of t0
         0##     -> ...
         DEFAULT -> case t0 `minusWord#` 1## of t1
            0##    -> ...
            DEFAUT -> case t1 `minusWord#` 1## of t2
               0##     -> ...
               DEFAULT -> case t2 `minusWord#` 1## of _
                  0##     -> ...
                  DEFAULT -> ...

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

  Note that in the non-DEFAULT cases we know what to bind 'b' to,
  whereas in the DEFAULT case we must reconstruct the original value.
  But NB: we use b'; we do not duplicate 'e'.

### Note: caseRules for dataToTag

### Note: Literal cases

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

### Note: caseRules for tagToEnum

### Note: Dead binders

Note that dead-ness is maintained by the simplifier, so that it is
accurate after simplification as well as before.

### Note: Cascading case merge

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

  case x of c1 { I# a1 ->
  case a1 of c2 ->
    0 -> ...
    DEFAULT -> case x of c3 { I# a2 ->
               case a2 of ...

After occurrence analysis (and its binder-swap) we get this

  case x of c1 { I# a1 ->
  let x = c1 in         -- Binder-swap addition
  case a1 of c2 ->
    0 -> ...
    DEFAULT -> case x of c3 { I# a2 ->
               case a2 of ...

When we simplify the inner case x, we'll see that
x=c1=I# a1.  So we'll bind a2 to a1, and get

  case x of c1 { I# a1 ->
  case a1 of c2 ->
    0 -> ...
    DEFAULT -> case a1 of ...

This is corect, but we can't do a case merge in this sweep
because c2 /= a1.  Reason: the binding c1=I# a1 went inwards
without getting changed to c1=I# c2.

I don't think this is worth fixing, even if I knew how. It'll
all come out in the next pass anyway.
