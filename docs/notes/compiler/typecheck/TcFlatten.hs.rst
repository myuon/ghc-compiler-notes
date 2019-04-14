`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcFlatten.hs>`_

compiler/typecheck/TcFlatten.hs
===============================


Note [The flattening story]
~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcFlatten.hs#L36>`__

* A CFunEqCan is either of form
     [G] <F xis> : F xis ~ fsk   -- fsk is a FlatSkolTv
     [W]       x : F xis ~ fmv   -- fmv is a FlatMetaTv
  where
     x is the witness variable
     xis are function-free
     fsk/fmv is a flatten skolem;
        it is always untouchable (level 0)

* CFunEqCans can have any flavour: [G], [W], [WD] or [D]

* KEY INSIGHTS:

   - A given flatten-skolem, fsk, is known a-priori to be equal to
     F xis (the LHS), with <F xis> evidence.  The fsk is still a
     unification variable, but it is "owned" by its CFunEqCan, and
     is filled in (unflattened) only by unflattenGivens.

   - A unification flatten-skolem, fmv, stands for the as-yet-unknown
     type to which (F xis) will eventually reduce.  It is filled in


   - All fsk/fmv variables are "untouchable".  To make it simple to test,
     we simply give them TcLevel=0.  This means that in a CTyVarEq, say,
       fmv ~ Int
     we NEVER unify fmv.

   - A unification flatten-skolem, fmv, ONLY gets unified when either
       a) The CFunEqCan takes a step, using an axiom
       b) By unflattenWanteds
    They are never unified in any other form of equality.
    For example [W] ffmv ~ Int  is stuck; it does not unify with fmv.

* We *never* substitute in the RHS (i.e. the fsk/fmv) of a CFunEqCan.
  That would destroy the invariant about the shape of a CFunEqCan,
  and it would risk wanted/wanted interactions. The only way we
  learn information about fsk is when the CFunEqCan takes a step.

  However we *do* substitute in the LHS of a CFunEqCan (else it
  would never get to fire!)

* Unflattening:
   - We unflatten Givens when leaving their scope (see unflattenGivens)
   - We unflatten Wanteds at the end of each attempt to simplify the
     wanteds; see unflattenWanteds, called from solveSimpleWanteds.

* Ownership of fsk/fmv.  Each canonical [G], [W], or [WD]
       CFunEqCan x : F xis ~ fsk/fmv
  "owns" a distinct evidence variable x, and flatten-skolem fsk/fmv.
  Why? We make a fresh fsk/fmv when the constraint is born;
  and we never rewrite the RHS of a CFunEqCan.

  In contrast a [D] CFunEqCan /shares/ its fmv with its partner [W],
  but does not "own" it.  If we reduce a [D] F Int ~ fmv, where
  say type instance F Int = ty, then we don't discharge fmv := ty.
  Rather we simply generate [D] fmv ~ ty (in TcInteract.reduce_top_fun_eq,
  and dischargeFmv)

* Inert set invariant: if F xis1 ~ fsk1, F xis2 ~ fsk2
                       then xis1 /= xis2
  i.e. at most one CFunEqCan with a particular LHS

* Function applications can occur in the RHS of a CTyEqCan.  No reason
  not allow this, and it reduces the amount of flattening that must occur.

* Flattening a type (F xis):
    - If we are flattening in a Wanted/Derived constraint
      then create new [W] x : F xis ~ fmv
      else create new [G] x : F xis ~ fsk
      with fresh evidence variable x and flatten-skolem fsk/fmv

    - Add it to the work list

    - Replace (F xis) with fsk/fmv in the type you are flattening

    - You can also add the CFunEqCan to the "flat cache", which
      simply keeps track of all the function applications you
      have flattened.

    - If (F xis) is in the cache already, just
      use its fsk/fmv and evidence x, and emit nothing.

    - No need to substitute in the flat-cache. It's not the end
      of the world if we start with, say (F alpha ~ fmv1) and
      (F Int ~ fmv2) and then find alpha := Int.  Athat will
      simply give rise to fmv1 := fmv2 via [Interacting rule] below

* Canonicalising a CFunEqCan [G/W] x : F xis ~ fsk/fmv
    - Flatten xis (to substitute any tyvars; there are already no functions)
                  cos :: xis ~ flat_xis
    - New wanted  x2 :: F flat_xis ~ fsk/fmv
    - Add new wanted to flat cache
    - Discharge x = F cos ; x2

* [Interacting rule]
    (inert)     [W] x1 : F tys ~ fmv1
    (work item) [W] x2 : F tys ~ fmv2
  Just solve one from the other:
    x2 := x1
    fmv2 := fmv1
  This just unites the two fsks into one.
  Always solve given from wanted if poss.

* For top-level reductions, see Note [Top-level reductions for type functions]
  in TcInteract


Why given-fsks, alone, doesn't work
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Could we get away with only flatten meta-tyvars, with no flatten-skolems? No.

::

  [W] w : alpha ~ [F alpha Int]

..

---> flatten
  w = ...w'...
  [W] w' : alpha ~ [fsk]
  [G] <F alpha Int> : F alpha Int ~ fsk

--> unify (no occurs check)
  alpha := [fsk]

But since fsk = F alpha Int, this is really an occurs check error.  If
that is all we know about alpha, we will succeed in constraint
solving, producing a program with an infinite type.

Even if we did finally get (g : fsk ~ Bool) by solving (F alpha Int ~ fsk)
using axiom, zonking would not see it, so (x::alpha) sitting in the
tree will get zonked to an infinite type.  (Zonking always only does
refl stuff.)

Why flatten-meta-vars, alone doesn't work
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Look at Simple13, with unification-fmvs only

::

  [G] g : a ~ [F a]

..

---> Flatten given
  g' = g;[x]
  [G] g'  : a ~ [fmv]
  [W] x : F a ~ fmv

--> subst a in x
  g' = g;[x]
  x = F g' ; x2
  [W] x2 : F [fmv] ~ fmv

And now we have an evidence cycle between g' and x!

If we used a given instead (ie current story)

::

  [G] g : a ~ [F a]

..

---> Flatten given
  g' = g;[x]
  [G] g'  : a ~ [fsk]
  [G] <F a> : F a ~ fsk

---> Substitute for a
  [G] g'  : a ~ [fsk]
  [G] F (sym g'); <F a> : F [fsk] ~ fsk


Why is it right to treat fmv's differently to ordinary unification vars?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  f :: forall a. a -> a -> Bool
  g :: F Int -> F Int -> Bool

Consider
  f (x:Int) (y:Bool)
This gives alpha~Int, alpha~Bool.  There is an inconsistency,
but really only one error.  SherLoc may tell you which location
is most likely, based on other occurrences of alpha.

Consider
  g (x:Int) (y:Bool)
Here we get (F Int ~ Int, F Int ~ Bool), which flattens to
  (fmv ~ Int, fmv ~ Bool)
But there are really TWO separate errors.

  ** We must not complain about Int~Bool. **

Moreover these two errors could arise in entirely unrelated parts of
the code.  (In the alpha case, there must be *some* connection (eg
v:alpha in common envt).)



Note [Unflattening can force the solver to iterate]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcFlatten.hs#L223>`__

Look at #10340:
   type family Any :: *   -- No instances
   get :: MonadState s m => m s
   instance MonadState s (State s) where ...

::

   foo :: State Any Any
   foo = get

..

For 'foo' we instantiate 'get' at types mm ss
   [WD] MonadState ss mm, [WD] mm ss ~ State Any Any
Flatten, and decompose
   [WD] MonadState ss mm, [WD] Any ~ fmv
   [WD] mm ~ State fmv, [WD] fmv ~ ss
Unify mm := State fmv:
   [WD] MonadState ss (State fmv)
   [WD] Any ~ fmv, [WD] fmv ~ ss
Now we are stuck; the instance does not match!!  So unflatten:
   fmv := Any
   ss := Any    (*)
   [WD] MonadState Any (State Any)

The unification (*) represents progress, so we must do a second
round of solving; this time it succeeds. This is done by the 'go'
loop in solveSimpleWanteds.

This story does not feel right but it's the best I can do; and the
iteration only happens in pretty obscure circumstances.



Note [The flattening work list]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcFlatten.hs#L611>`__

The "flattening work list", held in the fe_work field of FlattenEnv,
is a list of CFunEqCans generated during flattening.  The key idea
is this.  Consider flattening (Eq (F (G Int) (H Bool)):
  * The flattener recursively calls itself on sub-terms before building
    the main term, so it will encounter the terms in order
              G Int
              H Bool
              F (G Int) (H Bool)
    flattening to sub-goals
              w1: G Int ~ fuv0
              w2: H Bool ~ fuv1
              w3: F fuv0 fuv1 ~ fuv2

  * Processing w3 first is BAD, because we can't reduce i t,so it'll
    get put into the inert set, and later kicked out when w1, w2 are
    solved.  In #9872 this led to inert sets containing hundreds
    of suspended calls.

  * So we want to process w1, w2 first.

  * So you might think that we should just use a FIFO deque for the work-list,
    so that putting adding goals in order w1,w2,w3 would mean we processed
    w1 first.

  * BUT suppose we have 'type instance G Int = H Char'.  Then processing
    w1 leads to a new goal
                w4: H Char ~ fuv0
    We do NOT want to put that on the far end of a deque!  Instead we want
    to put it at the *front* of the work-list so that we continue to work
    on it.

So the work-list structure is this:

  * The wl_funeqs (in TcS) is a LIFO stack; we push new goals (such as w4) on
    top (extendWorkListFunEq), and take new work from the top
    (selectWorkItem).

  * When flattening, emitFlatWork pushes new flattening goals (like
    w1,w2,w3) onto the flattening work list, fe_work, another
    push-down stack.

  * When we finish flattening, we *reverse* the fe_work stack
    onto the wl_funeqs stack (which brings w1 to the top).

The function runFlatten initialises the fe_work stack, and reverses
it onto wl_fun_eqs at the end.



Note [Flattener EqRels]
~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcFlatten.hs#L660>`__

When flattening, we need to know which equality relation -- nominal
or representation -- we should be respecting. The only difference is
that we rewrite variables by representational equalities when fe_eq_rel
is ReprEq, and that we unwrap newtypes when flattening w.r.t.
representational equality.



Note [Flattener CtLoc]
~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcFlatten.hs#L668>`__

The flattener does eager type-family reduction.
Type families might loop, and we
don't want GHC to do so. A natural solution is to have a bounded depth
to these processes. A central difficulty is that such a solution isn't
quite compositional. For example, say it takes F Int 10 steps to get to Bool.
How many steps does it take to get from F Int -> F Int to Bool -> Bool?
10? 20? What about getting from Const Char (F Int) to Char? 11? 1? Hard to
know and hard to track. So, we punt, essentially. We store a CtLoc in
the FlattenEnv and just update the environment when recurring. In the
TyConApp case, where there may be multiple type families to flatten,
we just copy the current CtLoc into each branch. If any branch hits the
stack limit, then the whole thing fails.

A consequence of this is that setting the stack limits appropriately
will be essentially impossible. So, the official recommendation if a
stack limit is hit is to disable the check entirely. Otherwise, there
will be baffling, unpredictable errors.



Note [Lazy flattening]
~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcFlatten.hs#L688>`__

The idea of FM_Avoid mode is to flatten less aggressively.  If we have
       a ~ [F Int]
there seems to be no great merit in lifting out (F Int).  But if it was
       a ~ [G a Int]
then we *do* want to lift it out, in case (G a Int) reduces to Bool, say,
which gets rid of the occurs-check problem.  (For the flat_top Bool, see
comments above and at call sites.)

HOWEVER, the lazy flattening actually seems to make type inference go
*slower*, not faster.  perf/compiler/T3064 is a case in point; it gets
*dramatically* worse with FM_Avoid.  I think it may be because
floating the types out means we normalise them, and that often makes
them smaller and perhaps allows more re-use of previously solved
goals.  But to be honest I'm not absolutely certain, so I am leaving
FM_Avoid in the code base.  What I'm removing is the unique place
where it is *used*, namely in TcCanonical.canEqTyVar.

See also Note [Conservative unification check] in TcUnify, which gives
other examples where lazy flattening caused problems.

Bottom line: FM_Avoid is unused for now (Nov 14).
Note: T5321Fun got faster when I disabled FM_Avoid
      T5837 did too, but it's pathalogical anyway



Note [Phantoms in the flattener]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcFlatten.hs#L714>`__

Suppose we have

data Proxy p = Proxy

and we're flattening (Proxy ty) w.r.t. ReprEq. Then, we know that `ty`
is really irrelevant -- it will be ignored when solving for representational
equality later on. So, we omit flattening `ty` entirely. This may
violate the expectation of "xi"s for a bit, but the canonicaliser will
soon throw out the phantoms when decomposing a TyConApp. (Or, the
canonicaliser will emit an insoluble, in which case the unflattened version
yields a better error message anyway.)



Note [No derived kind equalities]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcFlatten.hs#L728>`__

A kind-level coercion can appear in types, via mkCastTy. So, whenever
we are generating a coercion in a dependent context (in other words,
in a kind) we need to make sure that our flavour is never Derived
(as Derived constraints have no evidence). The noBogusCoercions function
changes the flavour from Derived just for this purpose.



Note [Flattening]
~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcFlatten.hs#L792>`__

  flatten ty  ==>   (xi, co)
    where
      xi has no type functions, unless they appear under ForAlls
         has no skolems that are mapped in the inert set
         has no filled-in metavariables
      co :: xi ~ ty

Key invariants:
  (F0) co :: xi ~ zonk(ty)
  (F1) tcTypeKind(xi) succeeds and returns a fully zonked kind
  (F2) tcTypeKind(xi) `eqType` zonk(tcTypeKind(ty))

Note that it is flatten's job to flatten *every type function it sees*.
flatten is only called on *arguments* to type functions, by canEqGiven.

Flattening also:
  * zonks, removing any metavariables, and
  * applies the substitution embodied in the inert set

Because flattening zonks and the returned coercion ("co" above) is also
zonked, it's possible that (co :: xi ~ ty) isn't quite true. So, instead,
we can rely on this fact:

::

  (F1) tcTypeKind(xi) succeeds and returns a fully zonked kind

..

Note that the left-hand type of co is *always* precisely xi. The right-hand
type may or may not be ty, however: if ty has unzonked filled-in metavariables,
then the right-hand type of co will be the zonked version of ty.
It is for this reason that we
occasionally have to explicitly zonk, when (co :: xi ~ ty) is important
even before we zonk the whole program. For example, see the FTRNotFollowed
case in flattenTyVar.

Why have these invariants on flattening? Because we sometimes use tcTypeKind
during canonicalisation, and we want this kind to be zonked (e.g., see
TcCanonical.canEqTyVar).

Flattening is always homogeneous. That is, the kind of the result of flattening is
always the same as the kind of the input, modulo zonking. More formally:

::

  (F2) tcTypeKind(xi) `eqType` zonk(tcTypeKind(ty))

..

This invariant means that the kind of a flattened type might not itself be flat.

Recall that in comments we use alpha[flat = ty] to represent a
flattening skolem variable alpha which has been generated to stand in
for ty.

----- Example of flattening a constraint: ------
  flatten (List (F (G Int)))  ==>  (xi, cc)
    where
      xi  = List alpha
      cc  = { G Int ~ beta[flat = G Int],
              F beta ~ alpha[flat = F beta] }
Here
  * alpha and beta are 'flattening skolem variables'.
  * All the constraints in cc are 'given', and all their coercion terms
    are the identity.

NB: Flattening Skolems only occur in canonical constraints, which
are never zonked, so we don't need to worry about zonking doing
accidental unflattening.

Note that we prefer to leave type synonyms unexpanded when possible,
so when the flattener encounters one, it first asks whether its
transitive expansion contains any type function applications.  If so,
it expands the synonym and proceeds; if not, it simply returns the
unexpanded synonym.



Note [flatten_args performance]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcFlatten.hs#L863>`__

In programs with lots of type-level evaluation, flatten_args becomes
part of a tight loop. For example, see test perf/compiler/T9872a, which
calls flatten_args a whopping 7,106,808 times. It is thus important
that flatten_args be efficient.

Performance testing showed that the current implementation is indeed
efficient. It's critically important that zipWithAndUnzipM be
specialized to TcS, and it's also quite helpful to actually `inline`
it. On test T9872a, here are the allocation stats (Dec 16, 2014):

 * Unspecialized, uninlined:     8,472,613,440 bytes allocated in the heap
 * Specialized, uninlined:       6,639,253,488 bytes allocated in the heap
 * Specialized, inlined:         6,281,539,792 bytes allocated in the heap

To improve performance even further, flatten_args_nom is split off
from flatten_args, as nominal equality is the common case. This would
be natural to write using mapAndUnzipM, but even inlined, that function
is not as performant as a hand-written loop.

 * mapAndUnzipM, inlined:        7,463,047,432 bytes allocated in the heap
 * hand-written recursion:       5,848,602,848 bytes allocated in the heap

If you make any change here, pay close attention to the T9872{a,b,c} tests
and T5321Fun.

If we need to make this yet more performant, a possible way forward is to
duplicate the flattener code for the nominal case, and make that case
faster. This doesn't seem quite worth it, yet.



Note [flatten_exact_fam_app_fully performance]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcFlatten.hs#L894>`__

The refactor of GRefl seems to cause performance trouble for T9872x: the allocation of flatten_exact_fam_app_fully_performance increased. See note [Generalized reflexive coercion] in TyCoRep for more information about GRefl and #15192 for the current state.

The explicit pattern match in homogenise_result helps with T9872a, b, c.

Still, it increases the expected allocation of T9872d by ~2%.

TODO: a step-by-step replay of the refactor to analyze the performance.



Note [Flattening synonyms]
~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcFlatten.hs#L1260>`__

Not expanding synonyms aggressively improves error messages, and
keeps types smaller. But we need to take care.

Suppose
   type T a = a -> a
and we want to flatten the type (T (F a)).  Then we can safely flatten
the (F a) to a skolem, and return (T fsk).  We don't need to expand the
synonym.  This works because TcTyConAppCo can deal with synonyms
(unlike TyConAppCo), see Note [TcCoercions] in TcEvidence.

But (#8979) for
   type T a = (F a, a)    where F is a type function
we must expand the synonym in (say) T Int, to expose the type function
to the flattener.



Note [Flattening under a forall]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcFlatten.hs#L1278>`__

Under a forall, we
  (a) MUST apply the inert substitution
  (b) MUST NOT flatten type family applications
Hence FMSubstOnly.

For (a) consider   c ~ a, a ~ T (forall b. (b, [c]))
If we don't apply the c~a substitution to the second constraint
we won't see the occurs-check error.

For (b) consider  (a ~ forall b. F a b), we don't want to flatten
to     (a ~ forall b.fsk, F a b ~ fsk)
because now the 'b' has escaped its scope.  We'd have to flatten to
       (a ~ forall b. fsk b, forall b. F a b ~ fsk b)
and we have not begun to think about how to make that work!



Note [Reduce type family applications eagerly]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcFlatten.hs#L1470>`__

If we come across a type-family application like (Append (Cons x Nil) t),
then, rather than flattening to a skolem etc, we may as well just reduce
it on the spot to (Cons x t).  This saves a lot of intermediate steps.
Examples that are helped are tests T9872, and T5321Fun.

Performance testing indicates that it's best to try this *twice*, once
before flattening arguments and once after flattening arguments.
Adding the extra reduction attempt before flattening arguments cut
the allocation amounts for the T9872{a,b,c} tests by half.

An example of where the early reduction appears helpful:

::

  type family Last x where
    Last '[x]     = x
    Last (h ': t) = Last t

..

::

  workitem: (x ~ Last '[1,2,3,4,5,6])

..

Flattening the argument never gets us anywhere, but trying to flatten
it at every step is quadratic in the length of the list. Reducing more
eagerly makes simplifying the right-hand type linear in its length.

Testing also indicated that the early reduction should *not* use the
flat-cache, but that the later reduction *should*. (Although the
effect was not large.)  Hence the Bool argument to try_to_reduce.  To
me (SLPJ) this seems odd; I get that eager reduction usually succeeds;
and if don't use the cache for eager reduction, we will miss most of
the opportunities for using it at all.  More exploration would be good
here.

At the end, once we've got a flat rhs, we extend the flatten-cache to record
the result. Doing so can save lots of work when the same redex shows up more
than once. Note that we record the link from the redex all the way to its
*final* value, not just the single step reduction. Interestingly, using the
flat-cache for the first reduction resulted in an increase in allocations
of about 3% for the four T9872x tests. However, using the flat-cache in
the later reduction is a similar gain. I (Richard E) don't currently (Dec '14)
have any knowledge as to *why* these facts are true.



Note [An alternative story for the inert substitution]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcFlatten.hs#L1599>`__

(This entire note is just background, left here in case we ever want
 to return the previous state of affairs)

We used (GHC 7.8) to have this story for the inert substitution inert_eqs

 * 'a' is not in fvs(ty)
 * They are *inert* in the weaker sense that there is no infinite chain of
   (i1 `eqCanRewrite` i2), (i2 `eqCanRewrite` i3), etc

This means that flattening must be recursive, but it does allow
  [G] a ~ [b]
  [G] b ~ Maybe c

This avoids "saturating" the Givens, which can save a modest amount of work.
It is easy to implement, in TcInteract.kick_out, by only kicking out an inert
only if (a) the work item can rewrite the inert AND
        (b) the inert cannot rewrite the work item

This is significantly harder to think about. It can save a LOT of work
in occurs-check cases, but we don't care about them much.  #5837
is an example; all the constraints here are Givens

             [G] a ~ TF (a,Int)
    -->
    work     TF (a,Int) ~ fsk
    inert    fsk ~ a

    --->
    work     fsk ~ (TF a, TF Int)
    inert    fsk ~ a

    --->
    work     a ~ (TF a, TF Int)
    inert    fsk ~ a

    ---> (attempting to flatten (TF a) so that it does not mention a
    work     TF a ~ fsk2
    inert    a ~ (fsk2, TF Int)
    inert    fsk ~ (fsk2, TF Int)

    ---> (substitute for a)
    work     TF (fsk2, TF Int) ~ fsk2
    inert    a ~ (fsk2, TF Int)
    inert    fsk ~ (fsk2, TF Int)

    ---> (top-level reduction, re-orient)
    work     fsk2 ~ (TF fsk2, TF Int)
    inert    a ~ (fsk2, TF Int)
    inert    fsk ~ (fsk2, TF Int)

    ---> (attempt to flatten (TF fsk2) to get rid of fsk2
    work     TF fsk2 ~ fsk3
    work     fsk2 ~ (fsk3, TF Int)
    inert    a   ~ (fsk2, TF Int)
    inert    fsk ~ (fsk2, TF Int)

    --->
    work     TF fsk2 ~ fsk3
    inert    fsk2 ~ (fsk3, TF Int)
    inert    a   ~ ((fsk3, TF Int), TF Int)
    inert    fsk ~ ((fsk3, TF Int), TF Int)

Because the incoming given rewrites all the inert givens, we get more and
more duplication in the inert set.  But this really only happens in pathalogical
casee, so we don't care.



Note [Unflatten using funeqs first]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcFlatten.hs#L1837>`__

::

    [W] G a ~ Int
    [W] F (G a) ~ G a

..

do not want to end up with
    [W] F Int ~ Int
because that might actually hold!  Better to end up with the two above
unsolved constraints.  The flat form will be

::

    G a ~ fmv1     (CFunEqCan)
    F fmv1 ~ fmv2  (CFunEqCan)
    fmv1 ~ Int     (CTyEqCan)
    fmv1 ~ fmv2    (CTyEqCan)

..

Flatten using the fun-eqs first.

