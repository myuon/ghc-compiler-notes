[[src]](https://github.com/ghc/ghc/tree/master/compiler/typecheck/TcInteract.hs)
# Main Interaction Solver                       

### Note: Basic Simplifier Plan

1. Pick an element from the WorkList if there exists one with depth
   less than our context-stack depth.

2. Run it down the 'stage' pipeline. Stages are:
      - canonicalization
      - inert reactions
      - spontaneous reactions
      - top-level intreactions
   Each stage returns a StopOrContinue and may have sideffected
   the inerts or worklist.

   The threading of the stages is as follows:
      - If (Stop) is returned by a stage then we start again from Step 1.
      - If (ContinueWith ct) is returned by a stage, we feed 'ct' on to
        the next stage in the pipeline.
4. If the element has survived (i.e. ContinueWith x) the last stage
   then we add him in the inerts and jump back to Step 1.

If in Step 1 no such element exists, we have exceeded our context-stack
depth and will simply fail.

### Note: Unflatten after solving the simple wanteds

We unflatten after solving the wc_simples of an implication, and before attempting
to float. This means that

 * The fsk/fmv flatten-skolems only survive during solveSimples.  We don't
   need to worry about them across successive passes over the constraint tree.
   (E.g. we don't need the old ic_fsk field of an implication.

 * When floating an equality outwards, we don't need to worry about floating its
   associated flattening constraints.

 * Another tricky case becomes easy: Trac #4935
       type instance F True a b = a
       type instance F False a b = b

       [w] F c a b ~ gamma
       (c ~ True) => a ~ gamma
       (c ~ False) => b ~ gamma

   Obviously this is soluble with gamma := F c a b, and unflattening
   will do exactly that after solving the simple constraints and before
   attempting the implications.  Before, when we were not unflattening,
   we had to push Wanted funeqs in as new givens.  Yuk!

   Another example that becomes easy: indexed_types/should_fail/T7786
      [W] BuriedUnder sub k Empty ~ fsk
      [W] Intersect fsk inv ~ s
      [w] xxx[1] ~ s
      [W] forall[2] . (xxx[1] ~ Empty)
                   => Intersect (BuriedUnder sub k Empty) inv ~ Empty

### Note: Running plugins on unflattened wanteds

There is an annoying mismatch between solveSimpleGivens and
solveSimpleWanteds, because the latter needs to fiddle with the inert
set, unflatten and zonk the wanteds.  It passes the zonked wanteds
to runTcPluginsWanteds, which produces a replacement set of wanteds,
some additional insolubles and a flag indicating whether to go round
the loop again.  If so, prepareInertsForImplications is used to remove
the previous wanteds (which will still be in the inert set).  Note
that prepareInertsForImplications will discard the insolubles, so we
must keep track of them separately.


### Note: The solveSimpleWanteds loop

Solving a bunch of simple constraints is done in a loop,
(the 'go' loop of 'solveSimpleWanteds'):
  1. Try to solve them; unflattening may lead to improvement that
     was not exploitable during solving
  2. Try the plugin
  3. If step 1 did improvement during unflattening; or if the plugin
     wants to run again, go back to step 1

### Note: Unflattening can force the solver to iterate


Example 1:
  Inert:   {c ~ d, F a ~ t, b ~ Int, a ~ ty} (all given)
  Reagent: a ~ [b] (given)

React with (c~d)     ==> IR (ContinueWith (a~[b]))  True    []
React with (F a ~ t) ==> IR (ContinueWith (a~[b]))  False   [F [b] ~ t]
React with (b ~ Int) ==> IR (ContinueWith (a~[Int]) True    []

Example 2:
  Inert:  {c ~w d, F a ~g t, b ~w Int, a ~w ty}
  Reagent: a ~w [b]

React with (c ~w d)   ==> IR (ContinueWith (a~[b]))  True    []
React with (F a ~g t) ==> IR (ContinueWith (a~[b]))  True    []    (can't rewrite given with wanted!)
etc.

Example 3:
  Inert:  {a ~ Int, F Int ~ b} (given)
  Reagent: F a ~ b (wanted)

React with (a ~ Int)   ==> IR (ContinueWith (F Int ~ b)) True []
React with (F Int ~ b) ==> IR Stop True []    -- after substituting we re-canonicalize and get nothing


# The interact-with-inert Stage


### Note: The Solver Invariant

We always add Givens first.  So you might think that the solver has
the invariant

   If the work-item is Given,
   then the inert item must Given

But this isn't quite true.  Suppose we have,
    c1: [W] beta ~ [alpha], c2 : [W] blah, c3 :[W] alpha ~ Int
After processing the first two, we get
     c1: [G] beta ~ [alpha], c2 : [W] blah
Now, c3 does not interact with the the given c1, so when we spontaneously
solve c3, we must re-react it with the inert set.  So we can attempt a
reaction between inert c2 [W] and work-item c3 [G].

It *is* true that [Solver Invariant]
   If the work-item is Given,
   AND there is a reaction
   then the inert item must Given
or, equivalently,
   If the work-item is Given,
   and the inert item is Wanted/Derived
   then there is no reaction


### Note: Replacement vs keeping

When we have two Given constraints both of type (C tys), say, which should
we keep?  More subtle than you might think!

  * Constraints come from different levels (different_level_strategy)

### Note: Shadowing of Implicit Parameters

### Note: Tracking redundant constraints

        It transpires that using the outermost one is reponsible for an
        8% performance improvement in nofib cryptarithm2, compared to
        just rolling the dice.  I didn't investigate why.

  * Constraints coming from the same level (i.e. same implication)

### Note: Solving superclass constraints

### Note: Tracking redundant constraints

  * Finally, when there is still a choice, use IRKeep rather than
    IRReplace, to avoid unnecessary munging of the inert set.

Doing the depth-check for implicit parameters, rather than making the work item
always override, is important.  Consider

    data T a where { T1 :: (?x::Int) => T Int; T2 :: T a }

    f :: (?x::a) => T a -> Int
    f T1 = ?x
    f T2 = 3

We have a [G] (?x::a) in the inert set, and at the pattern match on T1 we add
two new givens in the work-list:  [G] (?x::Int)
                                  [G] (a ~ Int)
Now consider these steps
  - process a~Int, kicking out (?x::a)
  - process (?x::Int), the inner given, adding to inert set
  - process (?x::a), the outer given, overriding the inner given
Wrong!  The depth-check ensures that the inner implicit parameter wins.
(Actually I think that the order in which the work-list is processed means
that this chain of events won't happen, but that's very fragile.)

# interactIrred


### Note: Solving irreducible equalities

Consider (Trac #14333)
  [G] a b ~R# c d
  [W] c d ~R# a b
Clearly we should be able to solve this! Even though the constraints are
not decomposable. We solve this when looking up the work-item in the
irreducible constraints to look for an identical one.  When doing this
lookup, findMatchingIrreds spots the equality case, and matches either
way around. It has to return a swap-flag so we can generate evidence
that is the right way round too.

### Note: Do not add duplicate derived insolubles

In general we *must* add an insoluble (Int ~ Bool) even if there is
one such there already, because they may come from distinct call
sites.  Not only do we want an error message for each, but with
-fdefer-type-errors we must generate evidence for each.  But for
*derived* insolubles, we only want to report each one once.  Why?

(a) A constraint (C r s t) where r -> s, say, may generate the same fundep
    equality many times, as the original constraint is successively rewritten.

(b) Ditto the successive iterations of the main solver itself, as it traverses
    the constraint tree. See example below.

Also for *given* insolubles we may get repeated errors, as we
repeatedly traverse the constraint tree.  These are relatively rare
anyway, so removing duplicates seems ok.  (Alternatively we could take
the SrcLoc into account.)

Note that the test does not need to be particularly efficient because
it is only used if the program has a type error anyway.

Example of (b): assume a top-level class and instance declaration:

  class D a b | a -> b
  instance D [a] [a]

Assume we have started with an implication:

  forall c. Eq c => { wc_simple = D [c] c [W] }

which we have simplified to:

  forall c. Eq c => { wc_simple = D [c] c [W]
                                  (c ~ [c]) [D] }

For some reason, e.g. because we floated an equality somewhere else,
we might try to re-solve this implication. If we do not do a
dropDerivedWC, then we will end up trying to solve the following
constraints the second time:

  (D [c] c) [W]
  (c ~ [c]) [D]

which will result in two Deriveds to end up in the insoluble set:

  wc_simple   = D [c] c [W]
               (c ~ [c]) [D], (c ~ [c]) [D]


# interactDict


### Note: Shortcut solving

When we interact a [W] constraint with a [G] constraint that solves it, there is
a possibility that we could produce better code if instead we solved from a
top-level instance declaration (See #12791, #5835). For example:

    class M a b where m :: a -> b

    type C a b = (Num a, M a b)

    f :: C Int b => b -> Int -> Int
    f _ x = x + 1

The body of `f` requires a [W] `Num Int` instance. We could solve this
constraint from the givens because we have `C Int b` and that provides us a
solution for `Num Int`. This would let us produce core like the following
(with -O2):

    f :: forall b. C Int b => b -> Int -> Int
    f = \ (@ b) ($d(%,%) :: C Int b) _ (eta1 :: Int) ->
        + @ Int
          (GHC.Classes.$p1(%,%) @ (Num Int) @ (M Int b) $d(%,%))
          eta1
          A.f1

This is bad! We could do /much/ better if we solved [W] `Num Int` directly
from the instance that we have in scope:

    f :: forall b. C Int b => b -> Int -> Int
    f = \ (@ b) _ _ (x :: Int) ->
        case x of { GHC.Types.I# x1 -> GHC.Types.I# (GHC.Prim.+# x1 1#) }

** NB: It is important to emphasize that all this is purely an optimization:
** exactly the same programs should typecheck with or without this
** procedure.

# fully

There is a reason why the solver does not simply try to solve such
constraints with top-level instances. If the solver finds a relevant
instance declaration in scope, that instance may require a context
that can't be solved for. A good example of this is:

    f :: Ord [a] => ...
    f x = ..Need Eq [a]...

If we have instance `Eq a => Eq [a]` in scope and we tried to use it, we would
be left with the obligation to solve the constraint Eq a, which we cannot. So we
must be conservative in our attempt to use an instance declaration to solve the
[W] constraint we're interested in.

Our rule is that we try to solve all of the instance's subgoals
recursively all at once. Precisely: We only attempt to solve
constraints of the form `C1, ... Cm => C t1 ... t n`, where all the Ci
are themselves class constraints of the form `C1', ... Cm' => C' t1'
... tn'` and we only succeed if the entire tree of constraints is
solvable from instances.

An example that succeeds:

    class Eq a => C a b | b -> a where
      m :: b -> a

    f :: C [Int] b => b -> Bool
    f x = m x == []

We solve for `Eq [Int]`, which requires `Eq Int`, which we also have. This
produces the following core:

    f :: forall b. C [Int] b => b -> Bool
    f = \ (@ b) ($dC :: C [Int] b) (x :: b) ->
        GHC.Classes.$fEq[]_$s$c==
          (m @ [Int] @ b $dC x) (GHC.Types.[] @ Int)

An example that fails:

    class Eq a => C a b | b -> a where
      m :: b -> a

    f :: C [a] b => b -> Bool
    f x = m x == []

Which, because solving `Eq [a]` demands `Eq a` which we cannot solve, produces:

    f :: forall a b. C [a] b => b -> Bool
    f = \ (@ a) (@ b) ($dC :: C [a] b) (eta :: b) ->
        ==
          @ [a]
          (A.$p1C @ [a] @ b $dC)
          (m @ [a] @ b $dC eta)
          (GHC.Types.[] @ a)

### Note: Shortcut solving: type families

# Implicit parameters


### Note: Shadowing of Implicit Parameters

Consider the following example:

f :: (?x :: Char) => Char
f = let ?x = 'a' in ?x

The "let ?x = ..." generates an implication constraint of the form:

?x :: Char => ?x :: Char

Furthermore, the signature for `f` also generates an implication
constraint, so we end up with the following nested implication:

?x :: Char => (?x :: Char => ?x :: Char)

Note that the wanted (?x :: Char) constraint may be solved in
two incompatible ways:  either by using the parameter from the
signature, or by using the local definition.  Our intention is
that the local definition should "shadow" the parameter of the
signature, and we implement this as follows: when we add a new
*given* implicit parameter to the inert set, it replaces any existing
givens for the same implicit parameter.

Similarly, consider
   f :: (?x::a) => Bool -> a

   g v = let ?x::Int = 3
         in (f v, let ?x::Bool = True in f v)

This should probably be well typed, with
   g :: Bool -> (Int, Bool)

So the inner binding for ?x::Bool *overrides* the outer one.

All this works for the normal cases but it has an odd side effect in
some pathological programs like this:
-- This is accepted, the second parameter shadows
f1 :: (?x :: Int, ?x :: Char) => Char
f1 = ?x

-- This is rejected, the second parameter shadows
f2 :: (?x :: Int, ?x :: Char) => Int
f2 = ?x

Both of these are actually wrong:  when we try to use either one,
we'll get two incompatible wanted constraints (?x :: Int, ?x :: Char),
which would lead to an error.

I can think of two ways to fix this:

  1. Simply disallow multiple constraints for the same implicit
    parameter---this is never useful, and it can be detected completely
    syntactically.

  2. Move the shadowing machinery to the location where we nest
     implications, and add some code here that will produce an
     error if we get multiple givens for the same implicit parameter.

# interactFunEq


### Note: Type inference for type families with injectivity

Suppose we have a type family with an injectivity annotation:
    type family F a b = r | r -> b

Then if we have two CFunEqCan constraints for F with the same RHS
   F s1 t1 ~ rhs
   F s2 t2 ~ rhs
then we can use the injectivity to get a new Derived constraint on
the injective argument
  [D] t1 ~ t2

That in turn can help GHC solve constraints that would otherwise require
guessing.  For example, consider the ambiguity check for
   f :: F Int b -> Int
We get the constraint
   [W] F Int b ~ F Int beta
where beta is a unification variable.  Injectivity lets us pick beta ~ b.

Injectivity information is also used at the call sites. For example:
   g = f True
gives rise to
   [W] F Int b ~ Bool
from which we can derive b.  This requires looking at the defining equations of
a type family, ie. finding equation with a matching RHS (Bool in this example)
and infering values of type variables (b in this example) from the LHS patterns
of the matching equation.  For closed type families we have to perform
additional apartness check for the selected equation to check that the selected
is guaranteed to fire for given LHS arguments.

These new constraints are simply *Derived* constraints; they have no evidence.
We could go further and offer evidence from decomposing injective type-function
applications, but that would require new evidence forms, and an extension to
FC, so we don't do that right now (Dec 14).

### Note: Injective type families

### Note: Cache-caused loops

It is very dangerous to cache a rewritten wanted family equation as 'solved' in our
solved cache (which is the default behaviour or xCtEvidence), because the interaction
may not be contributing towards a solution. Here is an example:

Initial inert set:
  [W] g1 : F a ~ beta1
Work item:
  [W] g2 : F a ~ beta2
The work item will react with the inert yielding the _same_ inert set plus:
    (i)   Will set g2 := g1 `cast` g3
    (ii)  Will add to our solved cache that [S] g2 : F a ~ beta2
    (iii) Will emit [W] g3 : beta1 ~ beta2
Now, the g3 work item will be spontaneously solved to [G] g3 : beta1 ~ beta2
and then it will react the item in the inert ([W] g1 : F a ~ beta1). So it
will set
      g1 := g ; sym g3
and what is g? Well it would ideally be a new goal of type (F a ~ beta2) but
remember that we have this in our solved cache, and it is ... g2! In short we
created the evidence loop:

        g2 := g1 ; g3
        g3 := refl
        g1 := g2 ; sym g3

To avoid this situation we do not cache as solved any workitems (or inert)
which did not really made a 'step' towards proving some goal. Solved's are
just an optimization so we don't lose anything in terms of completeness of
solving.

### Note: Efficient Orientation

Suppose we are interacting two FunEqCans with the same LHS:
          (inert)  ci :: (F ty ~ xi_i)
          (work)   cw :: (F ty ~ xi_w)
We prefer to keep the inert (else we pass the work item on down
the pipeline, which is a bit silly).  If we keep the inert, we
will (a) discharge 'cw'
     (b) produce a new equality work-item (xi_w ~ xi_i)
Notice the orientation (xi_w ~ xi_i) NOT (xi_i ~ xi_w):
    new_work :: xi_w ~ xi_i
    cw := ci ; sym new_work
Why?  Consider the simplest case when xi1 is a type variable.  If
we generate xi1~xi2, porcessing that constraint will kick out 'ci'.
If we generate xi2~xi1, there is less chance of that happening.
Of course it can and should still happen if xi1=a, xi1=Int, say.
But we want to avoid it happening needlessly.

Similarly, if we *can't* keep the inert item (because inert is Wanted,
and work is Given, say), we prefer to orient the new equality (xi_i ~
xi_w).

### Note: Carefully solve the right CFunEqCan

   ---- OLD COMMENT, NOW NOT NEEDED
   ---- because we now allow multiple
   ---- wanted FunEqs with the same head
Consider the constraints
  c1 :: F Int ~ a      -- Arising from an application line 5
  c2 :: F Int ~ Bool   -- Arising from an application line 10
Suppose that 'a' is a unification variable, arising only from
flattening.  So there is no error on line 5; it's just a flattening
variable.  But there is (or might be) an error on line 10.

Two ways to combine them, leaving either (Plan A)
  c1 :: F Int ~ a      -- Arising from an application line 5
  c3 :: a ~ Bool       -- Arising from an application line 10
or (Plan B)
  c2 :: F Int ~ Bool   -- Arising from an application line 10
  c4 :: a ~ Bool       -- Arising from an application line 5

Plan A will unify c3, leaving c1 :: F Int ~ Bool as an error
on the *totally innocent* line 5.  An example is test SimpleFail16
where the expected/actual message comes out backwards if we use
the wrong plan.

The second is the right thing to do.  Hence the isMetaTyVarTy
test when solving pairwise CFunEqCan.

# interactTyVarEq


### Note: Avoid double unifications

The spontaneous solver has to return a given which mentions the unified unification
variable *on the left* of the equality. Here is what happens if not:
  Original wanted:  (a ~ alpha),  (alpha ~ Int)
We spontaneously solve the first wanted, without changing the order!
      given : a ~ alpha      [having unified alpha := a]
Now the second wanted comes along, but he cannot rewrite the given, so we simply continue.
At the end we spontaneously solve that guy, *reunifying*  [alpha := Int]

We avoid this problem by orienting the resulting given so that the unification
variable is on the left.  [Note that alternatively we could attempt to
enforce this at canonicalization]

### Note: No touchables as FunEq RHS

# Functional dependencies, instantiation of equations


When we spot an equality arising from a functional dependency,
we now use that equality (a "wanted") to rewrite the work-item
constraint right away.  This avoids two dangers

 Danger 1: If we send the original constraint on down the pipeline
           it may react with an instance declaration, and in delicate
           situations (when a Given overlaps with an instance) that
           may produce new insoluble goals: see Trac #4952

 Danger 2: If we don't rewrite the constraint, it may re-react
           with the same thing later, and produce the same equality
           again --> termination worries.

To achieve this required some refactoring of FunDeps.hs (nicer
now!).

### Note: FunDep and implicit parameter reactions

Currently, our story of interacting two dictionaries (or a dictionary
and top-level instances) for functional dependencies, and implicit
parameters, is that we simply produce new Derived equalities.  So for example

        class D a b | a -> b where ...
    Inert:
        d1 :g D Int Bool
    WorkItem:
        d2 :w D Int alpha

    We generate the extra work item
        cv :d alpha ~ Bool
    where 'cv' is currently unused.  However, this new item can perhaps be
    spontaneously solved to become given and react with d2,
    discharging it in favour of a new constraint d2' thus:
        d2' :w D Int Bool
        d2 := d2' |> D Int cv
    Now d2' can be discharged from d1

We could be more aggressive and try to *immediately* solve the dictionary
using those extra equalities, but that requires those equalities to carry
evidence and derived do not carry evidence.

If that were the case with the same inert set and work item we might dischard
d2 directly:

        cv :w alpha ~ Bool
        d2 := d1 |> D Int cv

But in general it's a bit painful to figure out the necessary coercion,
so we just take the first approach. Here is a better example. Consider:
    class C a b c | a -> b
And:
     [Given]  d1 : C T Int Char
     [Wanted] d2 : C T beta Int
In this case, it's *not even possible* to solve the wanted immediately.
So we should simply output the functional dependency and add this guy
[but NOT its superclasses] back in the worklist. Even worse:
     [Given] d1 : C T Int beta
     [Wanted] d2: C T beta Int
Then it is solvable, but its very hard to detect this on the spot.

It's exactly the same with implicit parameters, except that the
"aggressive" approach would be much easier to implement.

### Note: Weird fundeps

Consider   class Het a b | a -> b where
              het :: m (f c) -> a -> m b

           class GHet (a :: * -> *) (b :: * -> *) | a -> b
           instance            GHet (K a) (K [a])
           instance Het a b => GHet (K a) (K b)

The two instances don't actually conflict on their fundeps,
although it's pretty strange.  So they are both accepted. Now
try   [W] GHet (K Int) (K Bool)
This triggers fundeps from both instance decls;
      [D] K Bool ~ K [a]
      [D] K Bool ~ K beta
And there's a risk of complaining about Bool ~ [a].  But in fact
the Wanted matches the second instance, so we never get as far
as the fundeps.

Trac #7875 is a case in point.


# The top-reaction Stage


### Note: Top-level reductions for type functions

### Note: The flattening story

Suppose we have a CFunEqCan  F tys ~ fmv/fsk, and a matching axiom.
Here is what we do, in four cases:

* Wanteds: general firing rule
    (work item) [W]        x : F tys ~ fmv
    instantiate axiom: ax_co : F tys ~ rhs

   Then:
      Discharge   fmv := rhs
      Discharge   x := ax_co ; sym x2
   This is *the* way that fmv's get unified; even though they are
   "untouchable".

### Note: FunEq occurs-check principle

* Wanteds: short cut firing rule
  Applies when the RHS of the axiom is another type-function application
      (work item)        [W] x : F tys ~ fmv
      instantiate axiom: ax_co : F tys ~ G rhs_tys

  It would be a waste to create yet another fmv for (G rhs_tys).
  Instead (shortCutReduction):
      - Flatten rhs_tys (cos : rhs_tys ~ rhs_xis)
      - Add G rhs_xis ~ fmv to flat cache  (note: the same old fmv)
      - New canonical wanted   [W] x2 : G rhs_xis ~ fmv  (CFunEqCan)
      - Discharge x := ax_co ; G cos ; x2

* Givens: general firing rule
      (work item)        [G] g : F tys ~ fsk
      instantiate axiom: ax_co : F tys ~ rhs

   Now add non-canonical given (since rhs is not flat)
      [G] (sym g ; ax_co) : fsk ~ rhs  (Non-canonical)

* Givens: short cut firing rule
  Applies when the RHS of the axiom is another type-function application
      (work item)        [G] g : F tys ~ fsk
      instantiate axiom: ax_co : F tys ~ G rhs_tys

  It would be a waste to create yet another fsk for (G rhs_tys).
  Instead (shortCutReduction):
     - Flatten rhs_tys: flat_cos : tys ~ flat_tys
     - Add new Canonical given
          [G] (sym (G flat_cos) ; co ; g) : G flat_tys ~ fsk   (CFunEqCan)

### Note: FunEq occurs-check principle

I have spent a lot of time finding a good way to deal with
CFunEqCan constraints like
    F (fuv, a) ~ fuv
where flatten-skolem occurs on the LHS.  Now in principle we
might may progress by doing a reduction, but in practice its
hard to find examples where it is useful, and easy to find examples
where we fall into an infinite reduction loop.  A rule that works
very well is this:

# FunEq occurs-check principle

      Do not reduce a CFunEqCan
          F tys ~ fsk
      if fsk appears free in tys
      Instead we treat it as stuck.

Examples:

* Trac #5837 has [G] a ~ TF (a,Int), with an instance
    type instance TF (a,b) = (TF a, TF b)
  This readily loops when solving givens.  But with the FunEq occurs
  check principle, it rapidly gets stuck which is fine.

* Trac #12444 is a good example, explained in comment:2.  We have
    type instance F (Succ x) = Succ (F x)
    [W] alpha ~ Succ (F alpha)
  If we allow the reduction to happen, we get an infinite loop

### Note: Cached solved FunEqs

When trying to solve, say (FunExpensive big-type ~ ty), it's important
to see if we have reduced (FunExpensive big-type) before, lest we
simply repeat it.  Hence the lookup in inert_solved_funeqs.  Moreover
we must use `funEqCanDischarge` because both uses might (say) be Wanteds,
and we *still* want to save the re-computation.

### Note: MATCHING-SYNONYMS

When trying to match a dictionary (D tau) to a top-level instance, or a
type family equation (F taus_1 ~ tau_2) to a top-level family instance,
we do *not* need to expand type synonyms because the matcher will do that for us.

### Note: Improvement orientation

A very delicate point is the orientation of derived equalities
arising from injectivity improvement (Trac #12522).  Suppse we have
  type family F x = t | t -> x
  type instance F (a, Int) = (Int, G a)
where G is injective; and wanted constraints

  [W] TF (alpha, beta) ~ fuv
  [W] fuv ~ (Int, <some type>)

The injectivity will give rise to derived constraints

  [D] gamma1 ~ alpha
  [D] Int ~ beta

The fresh unification variable gamma1 comes from the fact that we
can only do "partial improvement" here; see Section 5.2 of
"Injective type families for Haskell" (HS'15).

Now, it's very important to orient the equations this way round,
so that the fresh unification variable will be eliminated in
favour of alpha.  If we instead had
   [D] alpha ~ gamma1
then we would unify alpha := gamma1; and kick out the wanted
constraint.  But when we grough it back in, it'd look like
   [W] TF (gamma1, beta) ~ fuv
and exactly the same thing would happen again!  Infinite loop.

This all seems fragile, and it might seem more robust to avoid
introducing gamma1 in the first place, in the case where the
actual argument (alpha, beta) partly matches the improvement
template.  But that's a bit tricky, esp when we remember that the
kinds much match too; so it's easier to let the normal machinery
handle it.  Instead we are careful to orient the new derived
equality with the template on the left.  Delicate, but it works.

### Note: No FunEq improvement for Givens

We don't do improvements (injectivity etc) for Givens. Why?

* It generates Derived constraints on skolems, which don't do us
  much good, except perhaps identify inaccessible branches.
  (They'd be perfectly valid though.)

* For type-nat stuff the derived constraints include type families;
  e.g.  (a < b), (b < c) ==> a < c If we generate a Derived for this,
  we'll generate a Derived/Wanted CFunEqCan; and, since the same
  InertCans (after solving Givens) are used for each iteration, that
  massively confused the unflattening step (TcFlatten.unflatten).

  In fact it led to some infinite loops:
     indexed-types/should_compile/T10806
     indexed-types/should_compile/T10507
     polykinds/T10742

### Note: Reduction for Derived CFunEqCans

You may wonder if it's important to use top-level instances to
simplify [D] CFunEqCan's.  But it is.  Here's an example (T10226).

   type instance F    Int = Int
   type instance FInv Int = Int

Suppose we have to solve
    [WD] FInv (F alpha) ~ alpha
    [WD] F alpha ~ Int

  --> flatten
    [WD] F alpha ~ fuv0
    [WD] FInv fuv0 ~ fuv1  -- (A)
    [WD] fuv1 ~ alpha
    [WD] fuv0 ~ Int        -- (B)

  --> Rewwrite (A) with (B), splitting it
    [WD] F alpha ~ fuv0
    [W] FInv fuv0 ~ fuv1
    [D] FInv Int ~ fuv1    -- (C)
    [WD] fuv1 ~ alpha
    [WD] fuv0 ~ Int

# This is the key step

  --> Rewrite (D) with (E)
    [WD] F alpha ~ fuv0
    [W] FInv fuv0 ~ fuv1
    [D] alpha ~ Int       -- (F)
    [WD] fuv1 ~ alpha
    [WD] fuv0 ~ Int

  --> unify (F)  alpha := Int, and that solves it

Another example is indexed-types/should_compile/T10634


# Top-level reaction for class constraints (CDictCan)


# Class lookup


### Note: Instance and Given overlap

Example, from the OutsideIn(X) paper:
       instance P x => Q [x]
       instance (x ~ y) => R y [x]

       wob :: forall a b. (Q [b], R b a) => a -> Int

       g :: forall a. Q [a] => [a] -> Int
       g x = wob x

From 'g' we get the impliation constraint:
            forall a. Q [a] => (Q [beta], R beta [a])
If we react (Q [beta]) with its top-level axiom, we end up with a
(P beta), which we have no way of discharging. On the other hand,
if we react R beta [a] with the top-level we get  (beta ~ a), which
is solvable and can help us rewrite (Q [beta]) to (Q [a]) which is
now solvable by the given Q [a].

The partial solution is that:
  In matchClassInst (and thus in topReact), we return a matching
  instance only when there is no Given in the inerts which is
  unifiable to this particular dictionary.

  We treat any meta-tyvar as "unifiable" for this purpose,
  *including* untouchable ones.  But not skolems like 'a' in
  the implication constraint above.

The end effect is that, much as we do for overlapping instances, we
delay choosing a class instance if there is a possibility of another
instance OR a given to match our constraint later on. This fixes
Trac #4981 and #5002.

Other notes:

* The check is done *first*, so that it also covers classes
  with built-in instance solving, such as
     - constraint tuples
     - natural numbers
     - Typeable

* Flatten-skolems: we do not treat a flatten-skolem as unifiable
  for this purpose.
  E.g.   f :: Eq (F a) => [a] -> [a]
         f xs = ....(xs==xs).....
  Here we get [W] Eq [a], and we don't want to refrain from solving
  it because of the given (Eq (F a)) constraint!

* The given-overlap problem is arguably not easy to appear in practice
  due to our aggressive prioritization of equality solving over other
  constraints, but it is possible. I've added a test case in
  typecheck/should-compile/GivenOverlapping.hs

* Another "live" example is Trac #10195; another is #10177.

* We ignore the overlap problem if -XIncoherentInstances is in force:
  see Trac #6002 for a worked-out example where this makes a
  difference.

* Moreover notice that our goals here are different than the goals of
  the top-level overlapping checks. There we are interested in
  validating the following principle:

      If we inline a function f at a site where the same global
      instance environment is available as the instance environment at
      the definition site of f then we should get the same behaviour.

  But for the Given Overlap check our goal is just related to completeness of
  constraint solving.

* The solution is only a partial one.  Consider the above example with
       g :: forall a. Q [a] => [a] -> Int
       g x = let v = wob x
             in v
  and suppose we have -XNoMonoLocalBinds, so that we attempt to find the most
  general type for 'v'.  When generalising v's type we'll simplify its
  Q [alpha] constraint, but we don't have Q [a] in the 'givens', so we
  will use the instance declaration after all. Trac #11948 was a case
  in point.

### Note: Simplifiable given constraints

### Note: Naturally coherent classes

### Note: The equality types story

### Note: Instance and Given overlap

### Note: Instance and Given overlap

The same reasoning applies to

* (~~)        heqTyCOn
* (~)         eqTyCon
* Coercible   coercibleTyCon

And less obviously to:

### Note: Tuples hiding implicit parameters

Exammples: T5853, T10432, T5315, T9222, T2627b, T3028b

PS: the term "naturally coherent" doesn't really seem helpful.
Perhaps "invertible" or something?  I left it for now though.


# Class lookup in the instance environment


# Class lookup for CTuples


# Class lookup for Literals


# Class lookup for Typeable


### Note: Typeable (T a b c)

For type applications we always decompose using binary application,
via doTyApp, until we get to a *kind* instantiation.  Example
   Proxy :: forall k. k -> *

To solve Typeable (Proxy (* -> *) Maybe) we
  - First decompose with doTyApp,
    to get (Typeable (Proxy (* -> *))) and Typeable Maybe
  - Then solve (Typeable (Proxy (* -> *))) with doTyConApp

If we attempt to short-cut by solving it all at once, via
doTyConApp

(this note is sadly truncated FIXME)

### Note: No Typeable for polytypes or qualified types

We do not support impredicative typeable, such as
   Typeable (forall a. a->a)
   Typeable (Eq a => a -> a)
   Typeable (() => Int)
   Typeable (((),()) => Int)

See Trac #9858.  For forall's the case is clear: we simply don't have
a TypeRep for them.  For qualified but not polymorphic types, like
(Eq a => a -> a), things are murkier.  But:

 * We don't need a TypeRep for these things.  TypeReps are for
   monotypes only.

 * Perhaps we could treat `=>` as another type constructor for `Typeable`
   purposes, and thus support things like `Eq Int => Int`, however,
   at the current state of affairs this would be an odd exception as
   no other class works with impredicative types.
   For now we leave it off, until we have a better story for impredicativity.


# Class lookup for lifted equality


# Class lookup for overloaded record fields


### Note: HasField instances

Suppose we have

    data T y = MkT { foo :: [y] }

and `foo` is in scope.  Then GHC will automatically solve a constraint like

    HasField "foo" (T Int) b

by emitting a new wanted

    T alpha -> [alpha] ~# T Int -> b

and building a HasField dictionary out of the selector function `foo`,
appropriately cast.

The HasField class is defined (in GHC.Records) thus:

    class HasField (x :: k) r a | x r -> a where
      getField :: r -> a

Since this is a one-method class, it is represented as a newtype.
Hence we can solve `HasField "foo" (T Int) b` by taking an expression
of type `T Int -> b` and casting it using the newtype coercion.
Note that

    foo :: forall y . T y -> [y]

so the expression we construct is

    foo @alpha |> co

where

    co :: (T alpha -> [alpha]) ~# HasField "foo" (T Int) b

is built from

    co1 :: (T alpha -> [alpha]) ~# (T Int -> b)

which is the new wanted, and

    co2 :: (T Int -> b) ~# HasField "foo" (T Int) b

which can be derived from the newtype coercion.

If `foo` is not in scope, or has a higher-rank or existentially
quantified type, then the constraint is not solved automatically, but
may be solved by a user-supplied HasField instance.  Similarly, if we
encounter a HasField constraint where the field is not a literal
string, or does not belong to the type, then we fall back on the
normal constraint solver behaviour.
