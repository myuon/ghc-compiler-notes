[[src]](https://github.com/ghc/ghc/tree/master/compiler/simplCore/Simplify.hs)

(c) The AQUA Project, Glasgow University, 1993-1998

# The main module of the simplifier


The guts of the simplifier is in this module, but the driver loop for
the simplifier is in SimplCore.hs.

### Note: The big picture

The general shape of the simplifier is this:

  simplExpr :: SimplEnv -> InExpr -> SimplCont -> SimplM (SimplFloats, OutExpr)
  simplBind :: SimplEnv -> InBind -> SimplM (SimplFloats, SimplEnv)

 * SimplEnv contains
     - Simplifier mode (which includes DynFlags for convenience)
     - Ambient substitution
     - InScopeSet

 * SimplFloats contains
     - Let-floats (which includes ok-for-spec case-floats)
     - Join floats
     - InScopeSet (including all the floats)

 * Expressions
      simplExpr :: SimplEnv -> InExpr -> SimplCont
                -> SimplM (SimplFloats, OutExpr)
   The result of simplifying an /expression/ is (floats, expr)
      - A bunch of floats (let bindings, join bindings)
      - A simplified expression.
   The overall result is effectively (let floats in expr)

 * Bindings
      simplBind :: SimplEnv -> InBind -> SimplM (SimplFloats, SimplEnv)
   The result of simplifying a binding is
     - A bunch of floats, the last of which is the simplified binding
       There may be auxiliary bindings too; see prepareRhs
     - An environment suitable for simplifying the scope of the binding

   The floats may also be empty, if the binding is inlined unconditionally;
   in that case the returned SimplEnv will have an augmented substitution.

   The returned floats and env both have an in-scope set, and they are
   guaranteed to be the same.

### Note: Shadowing

The simplifier used to guarantee that the output had no shadowing, but
it does not do so any more.   (Actually, it never did!)  The reason is
documented with simplifyArgs.

# Eta expansion

For eta expansion, we want to catch things like

        case e of (a,b) -> \x -> case a of (p,q) -> \y -> r

If the \x was on the RHS of a let, we'd eta expand to bring the two
lambdas together.  And in general that's a good thing to do.  Perhaps
we should eta expand wherever we find a (value) lambda?  Then the eta
expansion at a let RHS can concentrate solely on the PAP case.

# \subsection{Bindings}


# Lazy bindings


simplRecBind is used for
        * recursive bindings only



simplOrTopPair is used for
        * recursive bindings (whether top level or not)
        * top-level non-recursive bindings

It assumes the binder has already been simplified, but not its IdInfo.


# prepareRhs, makeTrivial


### Note: prepareRhs

prepareRhs takes a putative RHS, checks whether it's a PAP or
constructor application and, if so, converts it to ANF, so that the
resulting thing can be inlined more easily.  Thus
        x = (f a, g b)
becomes
        t1 = f a
        t2 = g b
        x = (t1,t2)

We also want to deal well cases like this
        v = (f e1 `cast` co) e2
Here we want to make e1,e2 trivial and get
        x1 = e1; x2 = e2; v = (f x1 `cast` co) v2
That's what the 'go' loop in prepareRhs does


### Note: Float coercions

When we find the binding
        x = e `cast` co
we'd like to transform it to
        x' = e
        x = x `cast` co         -- A trivial binding
There's a chance that e will be a constructor application or function, or something
like that, so moving the coercion to the usage site may well cancel the coercions
and lead to further optimisation.  Example:

     data family T a :: *
     data instance T Int = T Int

     foo :: Int -> Int -> Int
     foo m n = ...
        where
          x = T m
          go 0 = 0
          go n = case x of { T m -> go (n-m) }
                -- This case should optimise

### Note: Preserve strictness when floating coercions

### Note: Float coercions

Its not wrong to drop it on the floor, but better to keep it.

### Note: Float coercions (unlifted)

BUT don't do [Float coercions] if 'e' has an unlifted type.
This *can* happen:

### Note: Trivial after prepareRhs

If we call makeTrival on (e |> co), the recursive use of prepareRhs
may leave us with
   { a1 = e }  and   (a1 |> co)
Now the latter is trivial, so we don't want to let-bind it.

### Note: Cannot trivialise

Consider:
   f :: Int -> Addr#

   foo :: Bar
   foo = Bar (f 3)

Then we can't ANF-ise foo, even though we'd like to, because
we can't make a top-level binding for the Addr# (f 3). And if
so we don't want to turn it into
   foo = let x = f 3 in Bar x
because we'll just end up inlining x back, and that makes the
simplifier loop.  Better not to ANF-ise it at all.

Literal strings are an exception.

   foo = Ptr "blob"#

We want to turn this into:

   foo1 = "blob"#
   foo = Ptr foo1

### Note: CoreSyn top-level string literals

# Completing a lazy binding


completeBind
  * deals only with Ids, not TyVars
  * takes an already-simplified binder and RHS
  * is used for both recursive and non-recursive bindings
  * is used for both top-level and non-top-level bindings

It does the following:
  - tries discarding a dead binding
  - tries PostInlineUnconditionally
  - add unfolding [this is the only place we add an unfolding]
  - add arity

It does *not* attempt to do let-to-case.  Why?  Because it is used for
  - top-level bindings (when let-to-case is impossible)
  - many situations where the "rhs" is known to be a WHNF
                (so let-to-case is inappropriate).

Nor does it do the atomic-argument thing


### Note: Arity decrease

Generally speaking the arity of a binding should not decrease.  But it *can*
legitimately happen because of RULES.  Eg
        f = g Int
where g has arity 2, will have arity 2.  But if there's a rewrite rule
        g Int --> h
where h has arity 1, then f's arity will decrease.  Here's a real-life example,
which is in the output of Specialise:

 $$ ppr (seIdSubst env) 

 pprTrace "simplExprF" (vcat
      [ ppr e
      , text "cont =" <+> ppr cont
      , text "inscope =" <+> ppr (seInScope env)
      , text "tvsubst =" <+> ppr (seTvSubst env)
      , text "idsubst =" <+> ppr (seIdSubst env)
      , text "cvsubst =" <+> ppr (seCvSubst env)
      ]) $ 

### Note: Avoiding space leaks in OutType

Since the simplifier is run for multiple iterations, we need to ensure
that any thunks in the output of one simplifier iteration are forced
by the evaluation of the next simplifier iteration. Otherwise we may
retain multiple copies of the Core program and leak a terrible amount
of memory (as in #13426).

The simplifier is naturally strict in the entire "Expr part" of the
input Core program, because any expression may contain binders, which
we must find in order to extend the SimplEnv accordingly. But types
do not contain binders and so it is tempting to write things like

    simplExpr env (Type ty) = return (Type (substTy env ty))   -- Bad!

This is Bad because the result includes a thunk (substTy env ty) which
retains a reference to the whole simplifier environment; and the next
simplifier iteration will not force this thunk either, because the
line above is not strict in ty.

So instead our strategy is for the simplifier to fully evaluate
OutTypes when it emits them into the output Core program, for example

    simplExpr env (Type ty) = do { ty' <- simplType env ty     -- Good
                                 ; return (Type ty') }

where the only difference from above is that simplType calls seqType
on the result of substTy.

However, SimplCont can also contain OutTypes and it's not necessarily
a good idea to force types on the way in to SimplCont, because they
may end up not being used and forcing them could be a lot of wasted
work. T5631 is a good example of this.

- For ApplyToTy's sc_arg_ty, we force the type on the way in because
  the type will almost certainly appear as a type argument in the
  output program.

- For the hole types in Stop and ApplyToTy, we force the type when we
  emit it into the output program, after obtaining it from
  contResultType. (The hole type in ApplyToTy is only directly used
  to form the result type in a new Stop continuation.)


# \subsection{The main rebuilder}


# \subsection{Lambdas}


# \subsection{Lambdas}


### Note: Avoiding exponential behaviour

One way in which we can get exponential behaviour is if we simplify a
big expression, and the re-simplify it -- and then this happens in a
deeply-nested way.  So we must be jolly careful about re-simplifying
an expression.  That is why completeNonRecX does not try
preInlineUnconditionally.

Example:
  f BIG, where f has a RULE
Then
 * We simplify BIG before trying the rule; but the rule does not fire
 * We inline f = \x. x True
 * So if we did preInlineUnconditionally we'd re-simplify (BIG True)

However, if BIG has /not/ already been simplified, we'd /like/ to
simplify BIG True; maybe good things happen.  That is why

* simplLam has
    - a case for (isSimplified dup), which goes via simplNonRecX, and
    - a case for the un-simplified case, which goes via simplNonRecE

### Note: Trying rewrite rules

### Note: Zap unfolding when beta-reducing

### Note: Case binders and join points

# Join points


### Note: Join points and case-of-case

When we perform the case-of-case transform (or otherwise push continuations
inward), we want to treat join points specially. Since they're always
tail-called and we want to maintain this invariant, we can do this (for any
evaluation context E):

  E[join j = e
    in case ... of
         A -> jump j 1
         B -> jump j 2
         C -> f 3]

    -->

  join j = E[e]
  in case ... of
       A -> jump j 1
       B -> jump j 2
       C -> E[f 3]

As is evident from the example, there are two components to this behavior:

  1. When entering the RHS of a join point, copy the context inside.
  2. When a join point is invoked, discard the outer context.

We need to be very careful here to remain consistent---neither part is
optional!

We need do make the continuation E duplicable (since we are duplicating it)
with mkDuableCont.

### Note: Join points wih -fno-case-of-case

Supose case-of-case is switched off, and we are simplifying

    case (join j x = <j-rhs> in
          case y of
             A -> j 1
             B -> j 2
             C -> e) of <outer-alts>

Usually, we'd push the outer continuation (case . of <outer-alts>) into
both the RHS and the body of the join point j.  But since we aren't doing
case-of-case we may then end up with this totally bogus result

    join x = case <j-rhs> of <outer-alts> in
    case (case y of
             A -> j 1
             B -> j 2
             C -> e) of <outer-alts>

This would be OK in the language of the paper, but not in GHC: j is no longer
a join point.  We can only do the "push contination into the RHS of the
join point j" if we also push the contination right down to the /jumps/ to
j, so that it can evaporate there.  If we are doing case-of-case, we'll get to

    join x = case <j-rhs> of <outer-alts> in
    case y of
      A -> j 1
      B -> j 2
      C -> case e of <outer-alts>

which is great.

Bottom line: if case-of-case is off, we must stop pushing the continuation
inwards altogether at any join point.  Instead simplify the (join ... in ...)
with a Stop continuation, and wrap the original continuation around the
outside.  Surprisingly tricky!

# Variables


### Note: Trying rewrite rules

Consider an application (f e1 e2 e3) where the e1,e2,e3 are not yet
simplified.  We want to simplify enough arguments to allow the rules
to apply, but it's more efficient to avoid simplifying e2,e3 if e1 alone
is sufficient.  Example: class ops
   (+) dNumInt e2 e3
If we rewrite ((+) dNumInt) to plusInt, we can take advantage of the
latter's strictness when simplifying e2, e3.  Moreover, suppose we have
  RULE  f Int = \x. x True

### Note: Avoiding exponential behaviour

So we try to apply rules if either
  (a) no_more_args: we've run out of argument that the rules can "see"
  (b) nr_wanted: none of the rules wants any more arguments

### Note: RULES apply to simplified arguments

It's very desirable to try RULES once the arguments have been simplified, because
doing so ensures that rule cascades work in one pass.  Consider
   {-# RULES g (h x) = k x
             f (k x) = x #

### Note: Optimising tagToEnum#

             new_alts = (DEFAULT, [], rhs1) : map enum_to_tag rest_alts
             new_bndr = setIdType bndr intPrimTy
                 -- The binder is dead, but should have the right type
      ; return (Just (val_arg, Select dup new_bndr new_alts se cont)) }


### Note: User-defined RULES for seq

Given
   case (scrut |> co) of _ -> rhs
look for rules that match the expression
   seq @t1 @t2 scrut
where scrut :: t1
      rhs   :: t2

If you find a match, rewrite it, and apply to 'rhs'.

Notice that we can simply drop casts on the fly here, which
makes it more likely that a rule will match.

### Note: User-defined RULES for seq

### Note: Occurrence-analyse after rule firing

After firing a rule, we occurrence-analyse the instantiated RHS before
simplifying it.  Usually this doesn't make much difference, but it can
be huge.  Here's an example (simplCore/should_compile/T7785)

  map f (map f (map f xs)

= -- Use build/fold form of map, twice
  map f (build (\cn. foldr (mapFB c f) n
                           (build (\cn. foldr (mapFB c f) n xs))))

= -- Apply fold/build rule
  map f (build (\cn. (\cn. foldr (mapFB c f) n xs) (mapFB c f) n))

= -- Beta-reduce
  -- Alas we have no occurrence-analysed, so we don't know
  -- that c is used exactly once
  map f (build (\cn. let c1 = mapFB c f in
                     foldr (mapFB c1 f) n xs))

= -- Use mapFB rule:   mapFB (mapFB c f) g = mapFB c (f.g)
  -- We can do this because (mapFB c n) is a PAP and hence expandable
  map f (build (\cn. let c1 = mapFB c n in
                     foldr (mapFB c (f.f)) n x))

This is not too bad.  But now do the same with the outer map, and
we get another use of mapFB, and t can interact with /both/ remaining
mapFB calls in the above expression.  This is stupid because actually
that 'c1' binding is dead.  The outer map introduces another c2. If
there is a deep stack of maps we get lots of dead bindings, and lots
of redundant work as we repeatedly simplify the result of firing rules.

The easy thing to do is simply to occurrence analyse the result of
the rule firing.  Note that this occ-anals not only the RHS of the
rule, but also the function arguments, which by now are OutExprs.
E.g.
      RULE f (g x) = x+1

Call   f (g BIG)  -->   (\x. x+1) BIG

The rule binders are lambda-bound and applied to the OutExpr arguments
(here BIG) which lack all internal occurrence info.

Is this inefficient?  Not really: we are about to walk over the result
of the rule firing to simplify it, so occurrence analysis is at most
a constant factor.

Possible improvement: occ-anal the rules when putting them in the
database; and in the simplifier just occ-anal the OutExpr arguments.
But that's more complicated and the rule RHS is usually tiny; so I'm
just doing the simple thing.

Historical note: previously we did occ-anal the rules in Rule.hs,
but failed to occ-anal the OutExpr arguments, which led to the
nasty performance problem described above.

### Note: Optimising tagToEnum#

If we have an enumeration data type:

  data Foo = A | B | C

Then we want to transform

   case tagToEnum# x of   ==>    case x of
     A -> e1                       DEFAULT -> e1
     B -> e2                       1#      -> e2
     C -> e3                       2#      -> e3

thereby getting rid of the tagToEnum# altogether.  If there was a DEFAULT
alternative we retain it (remember it comes first).  If not the case must
be exhaustive, and we reflect that in the transformed version by adding
a DEFAULT.  Otherwise Lint complains that the new case is not exhaustive.
See #8317.

### Note: Rules for recursive functions

You might think that we shouldn't apply rules for a loop breaker:
doing so might give rise to an infinite loop, because a RULE is
rather like an extra equation for the function:
     RULE:           f (g x) y = x+y
     Eqn:            f a     y = a-y

But it's too drastic to disable rules for loop breakers.
Even the foldr/build rule would be disabled, because foldr
is recursive, and hence a loop breaker:
     foldr k z (build g) = g k z
So it's up to the programmer: rules can cause divergence

# Rebuilding a case expression


### Note: Case elimination

The case-elimination transformation discards redundant case expressions.
Start with a simple situation:

        case x# of      ===>   let y# = x# in e
          y# -> e

(when x#, y# are of primitive type, of course).  We can't (in general)
do this for algebraic cases, because we might turn bottom into
non-bottom!

The code in SimplUtils.prepareAlts has the effect of generalise this
idea to look for a case where we're scrutinising a variable, and we
know that only the default case can match.  For example:

        case x of
          0#      -> ...
          DEFAULT -> ...(case x of
                         0#      -> ...
                         DEFAULT -> ...) ...

Here the inner case is first trimmed to have only one alternative, the
DEFAULT, after which it's an instance of the previous case.  This
really only shows up in eliminating error-checking code.

Note that SimplUtils.mkCase combines identical RHSs.  So

        case e of       ===> case e of DEFAULT -> r
           True  -> r
           False -> r

Now again the case may be elminated by the CaseElim transformation.
This includes things like (==# a# b#)::Bool so that we simplify
      case ==# a# b# of { True -> x; False -> x }
to just
      x
This particular example shows up in default methods for
comparison operations (e.g. in (>=) for Int.Int32)

### Note: Case to let transformation

If a case over a lifted type has a single alternative, and is being
used as a strict 'let' (all isDeadBinder bndrs), we may want to do
this transformation:

    case e of r       ===>   let r = e in ...r...
      _ -> ...r...

We treat the unlifted and lifted cases separately:

* Unlifted case: 'e' satisfies exprOkForSpeculation
  (ok-for-spec is needed to satisfy the let/app invariant).
  This turns     case a +# b of r -> ...r...
  into           let r = a +# b in ...r...
  and thence     .....(a +# b)....

  However, if we have
      case indexArray# a i of r -> ...r...
  we might like to do the same, and inline the (indexArray# a i).
  But indexArray# is not okForSpeculation, so we don't build a let
  in rebuildCase (lest it get floated *out*), so the inlining doesn't
  happen either.  Annoying.

* Lifted case: we need to be sure that the expression is already
  evaluated (exprIsHNF).  If it's not already evaluated
      - we risk losing exceptions, divergence or
        user-specified thunk-forcing
      - even if 'e' is guaranteed to converge, we don't want to
        create a thunk (call by need) instead of evaluating it
        right away (call by value)

### Note: Eliminating redundant seqs

### Note: aBSENT_ERROR_ID

### Note: Eliminating redundant seqs

If we have this:
   case x of r { _ -> ..r.. }
where 'r' is used strictly in (..r..), the case is effectively a 'seq'
on 'x', but since 'r' is used strictly anyway, we can safely transform to
   (...x...)

Note that this can change the error behaviour.  For example, we might
transform
    case x of { _ -> error "bad" }
    --> error "bad"
which is might be puzzling if 'x' currently lambda-bound, but later gets
let-bound to (error "good").

Nevertheless, the paper "A semantics for imprecise exceptions" allows
this transformation. If you want to fix the evaluation order, use
'pseq'.  See Trac #8900 for an example where the loss of this
transformation bit us in practice.

### Note: Empty case alternatives

Just for reference, the original code (added Jan 13) looked like this:
     || case_bndr_evald_next rhs

### Note: Case binder next

(This came up when fixing Trac #7542. See also Note [Eta reduction of
an eval'd function] in CoreUtils.)

# Further notes about case elimination

Consider:       test :: Integer -> IO ()
                test = print

Notice the strange '<' which has no effect at all. This is a funny one.
It started like this:

f x y = if x < 0 then jtos x
          else if y==0 then "" else jtos x

At a particular call site we have (f v 1).  So we inline to get

        if v < 0 then jtos x
        else if 1==0 then "" else jtos x

Now simplify the 1==0 conditional:

        if v<0 then jtos v else jtos v

Now common-up the two branches of the case:

        case (v<0) of DEFAULT -> jtos v

Why don't we drop the case?  Because it's strict in v.  It's technically
wrong to drop even unnecessary evaluations, and in practice they
may be a result of 'seq' so we *definitely* don't want to drop those.
I don't really know how to improve this situation.



simplCaseBinder checks whether the scrutinee is a variable, v.  If so,
try to eliminate uses of v in the RHSs in favour of case_bndr; that
way, there's a chance that v will now only be used once, and hence
inlined.

### Note: Binder swap

### Note: knownCon occ info

If the case binder is not dead, then neither are the pattern bound
variables:
        case <any> of x { (a,b) ->
        case x of { (p,q) -> p } }
Here (a,b) both look dead, but come alive after the inner case is eliminated.
The point is that we bring into the envt a binding
        let x = (a,b)
after the outer case, and that makes (a,b) alive.  At least we do unless
the case binder is guaranteed dead.

### Note: Case alternative occ info

### Note: Add unfolding for scrutinee

### Note: Improving seq

Consider
        type family F :: * -> *
        type instance F Int = Int

We'd like to transform
        case e of (x :: F Int) { DEFAULT -> rhs }
===>
        case e `cast` co of (x'::Int)
           I# x# -> let x = x' `cast` sym co
                    in rhs

so that 'rhs' can take advantage of the form of x'.  Notice that Note
[Case of cast] (in OccurAnal) may then apply to the result.

We'd also like to eliminate empty types (Trac #13468). So if

    data Void
    type instance F Bool = Void

then we'd like to transform
        case (x :: F Bool) of { _ -> error "urk" }
===>
        case (x |> co) of (x' :: Void) of {}

Nota Bene: we used to have a built-in rule for 'seq' that dropped
casts, so that
    case (x |> co) of { _ -> blah }
dropped the cast; in order to improve the chances of trySeqRules
firing.  But that works in the /opposite/ direction to Note [Improving
seq] so there's a danger of flip/flopping.  Better to make trySeqRules
insensitive to the cast, which is now is.

The need for [Improving seq] showed up in Roman's experiments.  Example:
  foo :: F Int -> Int -> Int
  foo t n = t `seq` bar n
     where
       bar 0 = 0
       bar n = bar (n - case t of TI i -> i)
Here we'd like to avoid repeated evaluating t inside the loop, by
taking advantage of the `seq`.

At one point I did transformation in LiberateCase, but it's more
robust here.  (Otherwise, there's a danger that we'll simply drop the
'seq' altogether, before LiberateCase gets to see it.)


### Note: Case binder evaluated-ness

We pin on a (OtherCon []) unfolding to the case-binder of a Case,
even though it'll be over-ridden in every case alternative with a more
informative unfolding.  Why?  Because suppose a later, less clever, pass
simply replaces all occurrences of the case binder with the binder itself;
then Lint may complain about the let/app invariant.  Example
    case e of b { DEFAULT -> let v = reallyUnsafePtrEq# b y in ....
                ; K       -> blah }

The let/app invariant requires that y is evaluated in the call to
reallyUnsafePtrEq#, which it is.  But we still want that to be true if we
propagate binders to occurrences.

This showed up in Trac #13027.

### Note: Add unfolding for scrutinee

### Note: Binder swap

BUT it is still VERY IMPORTANT to add a suitable unfolding for a
variable scrutinee, in simplAlt.  Here's why
   case x of y
     (a,b) -> case b of c
                I# v -> ...(f y)...
There is no occurrence of 'b' in the (...(f y)...).  But y gets
the unfolding (a,b), and *that* mentions b.  If f has a RULE
    RULE f (p, I# q) = ...
we want that rule to match, so we must extend the in-scope env with a
suitable unfolding for 'y'.  It's *essential* for rule matching; but
it's also good for case-elimintation -- suppose that 'f' was inlined
and did multi-level case analysis, then we'd solve it in one
simplifier sweep instead of two.

### Note: Add scrutinee to ValueEnv too

HOWEVER, given
  case x of y { Just a -> r1; Nothing -> r2 }
we do not want to add the unfolding x -> y to 'x', which might seem cool,
since 'y' itself has different unfoldings in r1 and r2.  Reason: if we
did that, we'd have to zap y's deadness info and that is a very useful
piece of information.

So instead we add the unfolding x -> Just a, and x -> Nothing in the
respective RHSs.

# \subsection{Known constructor}


We are a bit careful with occurrence info.  Here's an example

        (\x* -> case x of (a*, b) -> f a) (h v, e)

where the * means "occurs once".  This effectively becomes
        case (h v, e) of (a*, b) -> f a)
and then
        let a* = h v; b = e in f a
and then
        f (h v)

All this should happen in one sweep.


# \subsection{Duplicating continuations}


Consider
  let x* = case e of { True -> e1; False -> e2 }
  in b
where x* is a strict binding.  Then mkDupableCont will be given
the continuation
   case [] of { True -> e1; False -> e2 } ; let x* = [] in b ; stop
and will split it into
   dupable:      case [] of { True -> $j1; False -> $j2 } ; stop
   join floats:  $j1 = e1, $j2 = e2
   non_dupable:  let x* = [] in b; stop

Putting this back together would give
   let x* = let { $j1 = e1; $j2 = e2 } in
            case e of { True -> $j1; False -> $j2 }
   in b
(Of course we only do this if 'e' wants to duplicate that continuation.)
Note how important it is that the new join points wrap around the
inner expression, and not around the whole thing.

In contrast, any let-bindings introduced by mkDupableCont can wrap
around the entire thing.

### Note: Bottom alternatives

When we have
     case (case x of { A -> error .. ; B -> e; C -> error ..)
       of alts
then we can just duplicate those alts because the A and C cases
will disappear immediately.  This is more direct than creating
join points and inlining them away.  See Trac #4930.


### Note: Fusing case continuations

It's important to fuse two successive case continuations when the
first has one alternative.  That's why we call prepareCaseCont here.
Consider this, which arises from thunk splitting (see Note [Thunk
splitting] in WorkWrap):

      let
        x* = case (case v of {pn -> rn}) of
               I# a -> I# a
      in body

The simplifier will find
    (Var v) with continuation
            Select (pn -> rn) (
            Select [I# a -> I# a] (
            StrictBind body Stop

So we'll call mkDupableCont on
   Select [I# a -> I# a] (StrictBind body Stop)
There is just one alternative in the first Select, so we want to
simplify the rhs (I# a) with continuation (StrictBind body Stop)
Supposing that body is big, we end up with
          let $j a = <let x = I# a in body>
          in case v of { pn -> case rn of
                                 I# a -> $j a }
This is just what we want because the rn produces a box that
the case rn cancels with.

See Trac #4957 a fuller example.

### Note: Case binders and join points

Consider this
   case (case .. ) of c {
     I# c# -> ....c....

If we make a join point with c but not c# we get
  $j = \c -> ....c....

But if later inlining scrutinises the c, thus

  $j = \c -> ... case c of { I# y -> ... } ...

we won't see that 'c' has already been scrutinised.  This actually
happens in the 'tabulate' function in wave4main, and makes a significant
difference to allocation.

An alternative plan is this:

   $j = \c# -> let c = I# c# in ...c....

but that is bad if 'c' is *not* later scrutinised.

So instead we do both: we pass 'c' and 'c#' , and record in c's inlining
(a stable unfolding) that it's really I# c#, thus

   $j = \c# -> \c[=I# c#] -> ...c....

Absence analysis may later discard 'c'.

### Note: Lambda-bound unfoldings

Also note that we can still end up passing stuff that isn't used.  Before
strictness analysis we have
   let $j x y c{=(x,y)} = (h c, ...)
   in ...
After strictness analysis we see that h is strict, we end up with
   let $j x y c{=(x,y)} = ($wh x y, ...)
and c is unused.

### Note: Duplicated env

Some of the alternatives are simplified, but have not been turned into a join point
So they *must* have a zapped subst-env.  So we can't use completeNonRecX to
bind the join point, because it might to do PostInlineUnconditionally, and
we'd lose that when zapping the subst-env.  We could have a per-alt subst-env,
but zapping it (as we do in mkDupableCont, the Select case) is safe, and
at worst delays the join-point inlining.

### Note: Small alternative rhs

It is worth checking for a small RHS because otherwise we
get extra let bindings that may cause an extra iteration of the simplifier to
inline back in place.  Quite often the rhs is just a variable or constructor.
The Ord instance of Maybe in PrelMaybe.hs, for example, took several extra
iterations because the version with the let bindings looked big, and so wasn't
inlined, but after the join points had been inlined it looked smaller, and so
was inlined.

NB: we have to check the size of rhs', not rhs.
Duplicating a small InAlt might invalidate occurrence information
However, if it *is* dupable, we return the *un* simplified alternative,
because otherwise we'd need to pair it up with an empty subst-env....
but we only have one env shared between all the alts.
(Remember we must zap the subst-env before re-simplifying something).
Rather than do this we simply agree to re-simplify the original (small) thing later.

### Note: Funky mkLamTypes

Notice the funky mkLamTypes.  If the constructor has existentials
it's possible that the join point will be abstracted over
type variables as well as term variables.
 Example:  Suppose we have
        data T = forall t.  C [t]
 Then faced with
        case (case e of ...) of
            C t xs::[t] -> rhs
 We get the join point
        let j :: forall t. [t] -> ...
            j = /\t \xs::[t] -> rhs
        in
        case (case e of ...) of
            C t xs::[t] -> j t xs

### Note: Duplicating StrictArg

We make a StrictArg duplicable simply by making all its
stored-up arguments (in sc_fun) trivial, by let-binding
them.  Thus:
        f E [..hole..]
        ==>     let a = E
                in f a [..hole..]
Now if the thing in the hole is a case expression (which is when
we'll call mkDupableCont), we'll push the function call into the
branches, which is what we want.  Now RULES for f may fire, and
call-pattern specialisation.  Here's an example from Trac #3116
     go (n+1) (case l of
                 1  -> bs'
                 _  -> Chunk p fpc (o+1) (l-1) bs')
If we can push the call for 'go' inside the case, we get
call-pattern specialisation for 'go', which is *crucial* for
this program.

Here is the (&&) example:
        && E (case x of { T -> F; F -> T })
  ==>   let a = E in
        case x of { T -> && a F; F -> && a T }
Much better!

Notice that
  * Arguments to f *after* the strict one are handled by
    the ApplyToVal case of mkDupableCont.  Eg
        f [..hole..] E

  * We can only do the let-binding of E because the function
    part of a StrictArg continuation is an explicit syntax
    tree.  In earlier versions we represented it as a function
    (CoreExpr -> CoreEpxr) which we couldn't take apart.

Historical aide: previously we did this (where E is a
big argument:
        f E [..hole..]
        ==>     let $j = \a -> f E a
                in $j [..hole..]

But this is terrible! Here's an example:
        && E (case x of { T -> F; F -> T })
Now, && is strict so we end up simplifying the case with
an ArgOf continuation.  If we let-bind it, we get
        let $j = \v -> && E v
        in simplExpr (case x of { T -> F; F -> T })
                     (ArgOf (\r -> $j r)
And after simplifying more we get
        let $j = \v -> && E v
        in case x of { T -> $j F; F -> $j T }
Which is a Very Bad Thing

### Note: Duplicating StrictBind

We make a StrictBind duplicable in a very similar way to
that for case expressions.  After all,
   let x* = e in b   is similar to    case e of x -> b

So we potentially make a join-point for the body, thus:
   let x = [] in b   ==>   join j x = b
                           in let x = [] in j x

### Note: Join point abstraction

NB: This note is now historical, describing how (in the past) we used
to add a void argument to nullary join points.  But now that "join
point" is not a fuzzy concept but a formal syntactic construct (as
distinguished by the JoinId constructor of IdDetails), each of these
concerns is handled separately, with no need for a vestigial extra
argument.

Join points always have at least one value argument,
for several reasons

* If we try to lift a primitive-typed something out
  for let-binding-purposes, we will *caseify* it (!),
  with potentially-disastrous strictness results.  So
  instead we turn it into a function: \v -> e
  where v::Void#.  The value passed to this function is void,
  which generates (almost) no code.

* CPR.  We used to say "&& isUnliftedType rhs_ty'" here, but now
  we make the join point into a function whenever used_bndrs'
  is empty.  This makes the join-point more CPR friendly.
  Consider:       let j = if .. then I# 3 else I# 4
                  in case .. of { A -> j; B -> j; C -> ... }

  Now CPR doesn't w/w j because it's a thunk, so
  that means that the enclosing function can't w/w either,
  which is a lose.  Here's the example that happened in practice:
          kgmod :: Int -> Int -> Int
          kgmod x y = if x > 0 && y < 0 || x < 0 && y > 0
                      then 78
                      else 5

### Note: What is a non-escaping let

* Floating.  Since a join point will be entered once, no sharing is
  gained by floating out, but something might be lost by doing
  so because it might be allocated.

I have seen a case alternative like this:
        True -> \v -> ...
It's a bit silly to add the realWorld dummy arg in this case, making
        $j = \s v -> ...
           True -> $j s
(the \v alone is enough to make CPR happy) but I think it's rare

There's a slight infelicity here: we pass the overall
case_bndr to all the join points if it's used in *any* RHS,
because we don't know its usage in each RHS separately



# Unfoldings


### Note: Force bottoming field

We need to force bottoming, or the new unfolding holds
on to the old unfolding (which is part of the id).

### Note: Setting the new unfolding

* If there's an INLINE pragma, we simplify the RHS gently.  Maybe we
  should do nothing at all, but simplifying gently might get rid of
  more crap.

* If not, we make an unfolding from the new RHS.  But *only* for
  non-loop-breakers. Making loop breakers not have an unfolding at all
  means that we can avoid tests in exprIsConApp, for example.  This is
  important: if exprIsConApp says 'yes' for a recursive thing, then we
  can get into an infinite loop

If there's a stable unfolding on a loop breaker (which happens for
INLINABLE), we hang on to the inlining.  It's pretty dodgy, but the
user did say 'INLINE'.  May need to revisit this choice.

# Rules


### Note: Rules in a letrec

After creating fresh binders for the binders of a letrec, we
substitute the RULES and add them back onto the binders; this is done
*before* processing any of the RHSs.  This is important.  Manuel found
cases where he really, really wanted a RULE for a recursive function
to apply in that function's own right-hand side.

### Note: Forming Rec groups