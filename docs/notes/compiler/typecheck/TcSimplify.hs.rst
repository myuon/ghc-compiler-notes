Note [Fail fast if there are insoluble kind equalities]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Rather like in simplifyInfer, fail fast if there is an insoluble
constraint.  Otherwise we'll just succeed in kind-checking a nonsense
type, with a cascade of follow-up errors.

For example polykinds/T12593, T15577, and many others.

Take care to ensure that you emit the insoluble constraints before
failing, because they are what will ulimately lead to the error
messsage!


Note [Fail fast on kind errors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
solveEqualities is used to solve kind equalities when kind-checking
user-written types. If solving fails we should fail outright, rather
than just accumulate an error message, for two reasons:

  * A kind-bogus type signature may cause a cascade of knock-on
    errors if we let it pass

  * More seriously, we don't have a convenient term-level place to add
    deferred bindings for unsolved kind-equality constraints, so we
    don't build evidence bindings (by usine reportAllUnsolved). That
    means that we'll be left with with a type that has coercion holes
    in it, something like
           <type> |> co-hole
    where co-hole is not filled in.  Eeek!  That un-filled-in
    hole actually causes GHC to crash with "fvProv falls into a hole"
    See #11563, #11520, #11516, #11399

So it's important to use 'checkNoErrs' here!



Note [When to do type-class defaulting]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In GHC 7.6 and 7.8.2, we did type-class defaulting only if insolubleWC
was false, on the grounds that defaulting can't help solve insoluble
constraints.  But if we *don't* do defaulting we may report a whole
lot of errors that would be solved by defaulting; these errors are
quite spurious because fixing the single insoluble error means that
defaulting happens again, which makes all the other errors go away.
This is jolly confusing: #9033.

So it seems better to always do type-class defaulting.

However, always doing defaulting does mean that we'll do it in
situations like this (#5934):
   run :: (forall s. GenST s) -> Int
   run = fromInteger 0
We don't unify the return type of fromInteger with the given function
type, because the latter involves foralls.  So we're left with
    (Num alpha, alpha ~ (forall s. GenST s) -> Int)
Now we do defaulting, get alpha := Integer, and report that we can't
match Integer with (forall s. GenST s) -> Int.  That's not totally
stupid, but perhaps a little strange.

Another potential alternative would be to suppress *all* non-insoluble
errors if there are *any* insoluble errors, anywhere, but that seems
too drastic.



Note [Must simplify after defaulting]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We may have a deeply buried constraint
    (t:*) ~ (a:Open)
which we couldn't solve because of the kind incompatibility, and 'a' is free.
Then when we default 'a' we can solve the constraint.  And we want to do
that before starting in on type classes.  We MUST do it before reporting
errors, because it isn't an error!  #7967 was due to this.



Note [Top-level Defaulting Plan]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We have considered two design choices for where/when to apply defaulting.
   (i) Do it in SimplCheck mode only /whenever/ you try to solve some
       simple constraints, maybe deep inside the context of implications.
       This used to be the case in GHC 7.4.1.
   (ii) Do it in a tight loop at simplifyTop, once all other constraints have
        finished. This is the current story.

Option (i) had many disadvantages:
   a) Firstly, it was deep inside the actual solver.
   b) Secondly, it was dependent on the context (Infer a type signature,
      or Check a type signature, or Interactive) since we did not want
      to always start defaulting when inferring (though there is an exception to
      this, see Note [Default while Inferring]).
   c) It plainly did not work. Consider typecheck/should_compile/DfltProb2.hs:
          f :: Int -> Bool
          f x = const True (\y -> let w :: a -> a
                                      w a = const a (y+1)
                                  in w y)
      We will get an implication constraint (for beta the type of y):
               [untch=beta] forall a. 0 => Num beta
      which we really cannot default /while solving/ the implication, since beta is
      untouchable.

Instead our new defaulting story is to pull defaulting out of the solver loop and
go with option (ii), implemented at SimplifyTop. Namely:
     - First, have a go at solving the residual constraint of the whole
       program
     - Try to approximate it with a simple constraint
     - Figure out derived defaulting equations for that simple constraint
     - Go round the loop again if you did manage to get some equations

Now, that has to do with class defaulting. However there exists type variable /kind/
defaulting. Again this is done at the top-level and the plan is:
     - At the top-level, once you had a go at solving the constraint, do
       figure out /all/ the touchable unification variables of the wanted constraints.
     - Apply defaulting to their kinds

More details in Note [DefaultTyVar].



Note [Safe Haskell Overlapping Instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In Safe Haskell, we apply an extra restriction to overlapping instances. The
motive is to prevent untrusted code provided by a third-party, changing the
behavior of trusted code through type-classes. This is due to the global and
implicit nature of type-classes that can hide the source of the dictionary.

Another way to state this is: if a module M compiles without importing another
module N, changing M to import N shouldn't change the behavior of M.

Overlapping instances with type-classes can violate this principle. However,
overlapping instances aren't always unsafe. They are just unsafe when the most
selected dictionary comes from untrusted code (code compiled with -XSafe) and
overlaps instances provided by other modules.

In particular, in Safe Haskell at a call site with overlapping instances, we
apply the following rule to determine if it is a 'unsafe' overlap:

 1) Most specific instance, I1, defined in an `-XSafe` compiled module.
 2) I1 is an orphan instance or a MPTC.
 3) At least one overlapped instance, Ix, is both:
    A) from a different module than I1
    B) Ix is not marked `OVERLAPPABLE`

This is a slightly involved heuristic, but captures the situation of an
imported module N changing the behavior of existing code. For example, if
condition (2) isn't violated, then the module author M must depend either on a
type-class or type defined in N.

Secondly, when should these heuristics be enforced? We enforced them when the
type-class method call site is in a module marked `-XSafe` or `-XTrustworthy`.
This allows `-XUnsafe` modules to operate without restriction, and for Safe
Haskell inferrence to infer modules with unsafe overlaps as unsafe.

One alternative design would be to also consider if an instance was imported as
a `safe` import or not and only apply the restriction to instances imported
safely. However, since instances are global and can be imported through more
than one path, this alternative doesn't work.



Note [Safe Haskell Overlapping Instances Implementation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

How is this implemented? It's complicated! So we'll step through it all:

 1) `InstEnv.lookupInstEnv` -- Performs instance resolution, so this is where
    we check if a particular type-class method call is safe or unsafe. We do this
    through the return type, `ClsInstLookupResult`, where the last parameter is a
    list of instances that are unsafe to overlap. When the method call is safe,
    the list is null.

 2) `TcInteract.matchClassInst` -- This module drives the instance resolution
    / dictionary generation. The return type is `ClsInstResult`, which either
    says no instance matched, or one found, and if it was a safe or unsafe
    overlap.

 3) `TcInteract.doTopReactDict` -- Takes a dictionary / class constraint and
     tries to resolve it by calling (in part) `matchClassInst`. The resolving
     mechanism has a work list (of constraints) that it process one at a time. If
     the constraint can't be resolved, it's added to an inert set. When compiling
     an `-XSafe` or `-XTrustworthy` module, we follow this approach as we know
     compilation should fail. These are handled as normal constraint resolution
     failures from here-on (see step 6).

     Otherwise, we may be inferring safety (or using `-Wunsafe`), and
     compilation should succeed, but print warnings and/or mark the compiled module
     as `-XUnsafe`. In this case, we call `insertSafeOverlapFailureTcS` which adds
     the unsafe (but resolved!) constraint to the `inert_safehask` field of
     `InertCans`.

 4) `TcSimplify.simplifyTop`:
       * Call simpl_top, the top-level function for driving the simplifier for
         constraint resolution.

       * Once finished, call `getSafeOverlapFailures` to retrieve the
         list of overlapping instances that were successfully resolved,
         but unsafe. Remember, this is only applicable for generating warnings
         (`-Wunsafe`) or inferring a module unsafe. `-XSafe` and `-XTrustworthy`
         cause compilation failure by not resolving the unsafe constraint at all.

       * For unresolved constraints (all types), call `TcErrors.reportUnsolved`,
         while for resolved but unsafe overlapping dictionary constraints, call
         `TcErrors.warnAllUnsolved`. Both functions convert constraints into a
         warning message for the user.

       * In the case of `warnAllUnsolved` for resolved, but unsafe
         dictionary constraints, we collect the generated warning
         message (pop it) and call `TcRnMonad.recordUnsafeInfer` to
         mark the module we are compiling as unsafe, passing the
         warning message along as the reason.

 5) `TcErrors.*Unsolved` -- Generates error messages for constraints by
    actually calling `InstEnv.lookupInstEnv` again! Yes, confusing, but all we
    know is the constraint that is unresolved or unsafe. For dictionary, all we
    know is that we need a dictionary of type C, but not what instances are
    available and how they overlap. So we once again call `lookupInstEnv` to
    figure that out so we can generate a helpful error message.

 6) `TcRnMonad.recordUnsafeInfer` -- Save the unsafe result and reason in an
      IORef called `tcg_safeInfer`.

 7) `HscMain.tcRnModule'` -- Reads `tcg_safeInfer` after type-checking, calling
    `HscMain.markUnsafeInfer` (passing the reason along) when safe-inferrence
    failed.



Note [No defaulting in the ambiguity check]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When simplifying constraints for the ambiguity check, we use
solveWantedsAndDrop, not simpl_top, so that we do no defaulting.
#11947 was an example:
   f :: Num a => Int -> Int
This is ambiguous of course, but we don't want to default the
(Num alpha) constraint to (Num Int)!  Doing so gives a defaulting
warning, but no error.
----------------


Note [Superclasses and satisfiability]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Expand superclasses before starting, because (Int ~ Bool), has
(Int ~~ Bool) as a superclass, which in turn has (Int ~N# Bool)
as a superclass, and it's the latter that is insoluble.  See
Note [The equality types story] in TysPrim.

If we fail to prove unsatisfiability we (arbitrarily) try just once to
find superclasses, using try_harder.  Reason: we might have a type
signature
   f :: F op (Implements push) => ..
where F is a type function.  This happened in #3972.

We could do more than once but we'd have to have /some/ limit: in the
the recursive case, we would go on forever in the common case where
the constraints /are/ satisfiable (#10592 comment:12!).

For stratightforard situations without type functions the try_harder
step does nothing.



Note [tcNormalise]
~~~~~~~~~~~~~~~~~~
tcNormalise is a rather atypical entrypoint to the constraint solver. Whereas
most invocations of the constraint solver are intended to simplify a set of
constraints or to decide if a particular set of constraints is satisfiable,
the purpose of tcNormalise is to take a type, plus some local constraints, and
normalise the type as much as possible with respect to those constraints.

Why is this useful? As one example, when coverage-checking an EmptyCase
expression, it's possible that the type of the scrutinee will only reduce
if some local equalities are solved for. See "Wrinkle: Local equalities"
in Note [Type normalisation for EmptyCase] in Check.

To accomplish its stated goal, tcNormalise first feeds the local constraints
into solveSimpleGivens, then stuffs the argument type in a CHoleCan, and feeds
that singleton Ct into solveSimpleWanteds, which reduces the type in the
CHoleCan as much as possible with respect to the local given constraints. When
solveSimpleWanteds is finished, we dig out the type from the CHoleCan and
return that.



Note [Inferring the type of a let-bound variable]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
   f x = rhs

To infer f's type we do the following:
 * Gather the constraints for the RHS with ambient level *one more than*
   the current one.  This is done by the call
        pushLevelAndCaptureConstraints (tcMonoBinds...)
   in TcBinds.tcPolyInfer

 * Call simplifyInfer to simplify the constraints and decide what to
   quantify over. We pass in the level used for the RHS constraints,
   here called rhs_tclvl.

This ensures that the implication constraint we generate, if any,
has a strictly-increased level compared to the ambient level outside
the let binding.



Note [Emitting the residual implication in simplifyInfer]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
   f = e
where f's type is inferred to be something like (a, Proxy k (Int |> co))
and we have an as-yet-unsolved, or perhaps insoluble, constraint
   [W] co :: Type ~ k
We can't form types like (forall co. blah), so we can't generalise over
the coercion variable, and hence we can't generalise over things free in
its kind, in the case 'k'.  But we can still generalise over 'a'.  So
we'll generalise to
   f :: forall a. (a, Proxy k (Int |> co))
Now we do NOT want to form the residual implication constraint
   forall a. [W] co :: Type ~ k
because then co's eventual binding (which will be a value binding if we
use -fdefer-type-errors) won't scope over the entire binding for 'f' (whose
type mentions 'co').  Instead, just as we don't generalise over 'co', we
should not bury its constraint inside the implication.  Instead, we must
put it outside.

That is the reason for the partitionBag in emitResidualConstraints,
which takes the CoVars free in the inferred type, and pulls their
constraints out.  (NB: this set of CoVars should be closed-over-kinds.)

All rather subtle; see #14584.



Note [Add signature contexts as givens]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this (#11016):
  f2 :: (?x :: Int) => _
  f2 = ?x
or this
  f3 :: a ~ Bool => (a, _)
  f3 = (True, False)
or theis
  f4 :: (Ord a, _) => a -> Bool
  f4 x = x==x

We'll use plan InferGen because there are holes in the type.  But:
 * For f2 we want to have the (?x :: Int) constraint floating around
   so that the functional dependencies kick in.  Otherwise the
   occurrence of ?x on the RHS produces constraint (?x :: alpha), and
   we won't unify alpha:=Int.
 * For f3 we want the (a ~ Bool) available to solve the wanted (a ~ Bool)
   in the RHS
 * For f4 we want to use the (Ord a) in the signature to solve the Eq a
   constraint.

Solution: in simplifyInfer, just before simplifying the constraints
gathered from the RHS, add Given constraints for the context of any
type signatures.



Note [Deciding quantification]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If the monomorphism restriction does not apply, then we quantify as follows:

* Step 1. Take the global tyvars, and "grow" them using the equality
  constraints
     E.g.  if x:alpha is in the environment, and alpha ~ [beta] (which can
          happen because alpha is untouchable here) then do not quantify over
          beta, because alpha fixes beta, and beta is effectively free in
          the environment too

  We also account for the monomorphism restriction; if it applies,
  add the free vars of all the constraints.

  Result is mono_tvs; we will not quantify over these.

* Step 2. Default any non-mono tyvars (i.e ones that are definitely
  not going to become further constrained), and re-simplify the
  candidate constraints.

  Motivation for re-simplification (#7857): imagine we have a
  constraint (C (a->b)), where 'a :: TYPE l1' and 'b :: TYPE l2' are
  not free in the envt, and instance forall (a::*) (b::*). (C a) => C
  (a -> b) The instance doesn't match while l1,l2 are polymorphic, but
  it will match when we default them to LiftedRep.

  This is all very tiresome.

* Step 3: decide which variables to quantify over, as follows:

  - Take the free vars of the tau-type (zonked_tau_tvs) and "grow"
    them using all the constraints.  These are tau_tvs_plus

  - Use quantifyTyVars to quantify over (tau_tvs_plus - mono_tvs), being
    careful to close over kinds, and to skolemise the quantified tyvars.
    (This actually unifies each quantifies meta-tyvar with a fresh skolem.)

  Result is qtvs.

* Step 4: Filter the constraints using pickQuantifiablePreds and the
  qtvs. We have to zonk the constraints first, so they "see" the
  freshly created skolems.



Note [Promote momomorphic tyvars]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Promote any type variables that are free in the environment.  Eg
   f :: forall qtvs. bound_theta => zonked_tau
The free vars of f's type become free in the envt, and hence will show
up whenever 'f' is called.  They may currently at rhs_tclvl, but they
had better be unifiable at the outer_tclvl!  Example: envt mentions
alpha[1]
           tau_ty = beta[2] -> beta[2]
           constraints = alpha ~ [beta]
we don't quantify over beta (since it is fixed by envt)
so we must promote it!  The inferred type is just
  f :: beta -> beta

NB: promoteTyVar ignores coercion variables



Note [Quantification and partial signatures]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When choosing type variables to quantify, the basic plan is to
quantify over all type variables that are
 * free in the tau_tvs, and
 * not forced to be monomorphic (mono_tvs),
   for example by being free in the environment.

However, in the case of a partial type signature, be doing inference
*in the presence of a type signature*. For example:
   f :: _ -> a
   f x = ...
or
   g :: (Eq _a) => _b -> _b
In both cases we use plan InferGen, and hence call simplifyInfer.  But
those 'a' variables are skolems (actually TyVarTvs), and we should be
sure to quantify over them.  This leads to several wrinkles:

* Wrinkle 1.  In the case of a type error
     f :: _ -> Maybe a
     f x = True && x
  The inferred type of 'f' is f :: Bool -> Bool, but there's a
  left-over error of form (HoleCan (Maybe a ~ Bool)).  The error-reporting
  machine expects to find a binding site for the skolem 'a', so we
  add it to the quantified tyvars.

* Wrinkle 2.  Consider the partial type signature
     f :: (Eq _) => Int -> Int
     f x = x
  In normal cases that makes sense; e.g.
     g :: Eq _a => _a -> _a
     g x = x
  where the signature makes the type less general than it could
  be. But for 'f' we must therefore quantify over the user-annotated
  constraints, to get
     f :: forall a. Eq a => Int -> Int
  (thereby correctly triggering an ambiguity error later).  If we don't
  we'll end up with a strange open type
     f :: Eq alpha => Int -> Int
  which isn't ambiguous but is still very wrong.

  Bottom line: Try to quantify over any variable free in psig_theta,
  just like the tau-part of the type.

* Wrinkle 3 (#13482). Also consider
    f :: forall a. _ => Int -> Int
    f x = if (undefined :: a) == undefined then x else 0
  Here we get an (Eq a) constraint, but it's not mentioned in the
  psig_theta nor the type of 'f'.  But we still want to quantify
  over 'a' even if the monomorphism restriction is on.

* Wrinkle 4 (#14479)
    foo :: Num a => a -> a
    foo xxx = g xxx
      where
        g :: forall b. Num b => _ -> b
        g y = xxx + y

  In the signature for 'g', we cannot quantify over 'b' because it turns out to
  get unified with 'a', which is free in g's environment.  So we carefully
  refrain from bogusly quantifying, in TcSimplify.decideMonoTyVars.  We
  report the error later, in TcBinds.chooseInferredQuantifiers.



Note [Growing the tau-tvs using constraints]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(growThetaTyVars insts tvs) is the result of extending the set
    of tyvars, tvs, using all conceivable links from pred

E.g. tvs = {a}, preds = {H [a] b, K (b,Int) c, Eq e}
Then growThetaTyVars preds tvs = {a,b,c}

Notice that
   growThetaTyVars is conservative       if v might be fixed by vs
                                         => v `elem` grow(vs,C)



Note [Quantification with errors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If we find that the RHS of the definition has some absolutely-insoluble
constraints (including especially "variable not in scope"), we

* Abandon all attempts to find a context to quantify over,
  and instead make the function fully-polymorphic in whatever
  type we have found

* Return a flag from simplifyInfer, indicating that we found an
  insoluble constraint.  This flag is used to suppress the ambiguity
  check for the inferred type, which may well be bogus, and which
  tends to obscure the real error.  This fix feels a bit clunky,
  but I failed to come up with anything better.

Reasons:
    - Avoid downstream errors
    - Do not perform an ambiguity test on a bogus type, which might well
      fail spuriously, thereby obfuscating the original insoluble error.
      #14000 is an example

I tried an alternative approach: simply failM, after emitting the
residual implication constraint; the exception will be caught in
TcBinds.tcPolyBinds, which gives all the binders in the group the type
(forall a. a).  But that didn't work with -fdefer-type-errors, because
the recovery from failM emits no code at all, so there is no function
to run!   But -fdefer-type-errors aspires to produce a runnable program.

NB that we must include *derived* errors in the check for insolubles.
Example:
    (a::*) ~ Int#
We get an insoluble derived error *~#, and we don't want to discard
it before doing the isInsolubleWC test!  (#8262)



Note [Default while Inferring]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Our current plan is that defaulting only happens at simplifyTop and
not simplifyInfer.  This may lead to some insoluble deferred constraints.
Example:

instance D g => C g Int b

constraint inferred = (forall b. 0 => C gamma alpha b) /\ Num alpha
type inferred       = gamma -> gamma

Now, if we try to default (alpha := Int) we will be able to refine the implication to
  (forall b. 0 => C gamma Int b)
which can then be simplified further to
  (forall b. 0 => D gamma)
Finally, we /can/ approximate this implication with (D gamma) and infer the quantified
type:  forall g. D g => g -> g

Instead what will currently happen is that we will get a quantified type
(forall g. g -> g) and an implication:
       forall g. 0 => (forall b. 0 => C g alpha b) /\ Num alpha

Which, even if the simplifyTop defaults (alpha := Int) we will still be left with an
unsolvable implication:
       forall g. 0 => (forall b. 0 => D g)

The concrete example would be:
       h :: C g a s => g -> a -> ST s a
       f (x::gamma) = (\_ -> x) (runST (h x (undefined::alpha)) + 1)

But it is quite tedious to do defaulting and resolve the implication constraints, and
we have not observed code breaking because of the lack of defaulting in inference, so
we don't do it for now.





Note [Minimize by Superclasses]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we quantify over a constraint, in simplifyInfer we need to
quantify over a constraint that is minimal in some sense: For
instance, if the final wanted constraint is (Eq alpha, Ord alpha),
we'd like to quantify over Ord alpha, because we can just get Eq alpha
from superclass selection from Ord alpha. This minimization is what
mkMinimalBySCs does. Then, simplifyInfer uses the minimal constraint
to check the original wanted.




Note [Avoid unnecessary constraint simplification]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    -------- NB NB NB (Jun 12) -------------
    This note not longer applies; see the notes with #4361.
    But I'm leaving it in here so we remember the issue.)
    ----------------------------------------
When inferring the type of a let-binding, with simplifyInfer,
try to avoid unnecessarily simplifying class constraints.
Doing so aids sharing, but it also helps with delicate
situations like

   instance C t => C [t] where ..

   f :: C [t] => ....
   f x = let g y = ...(constraint C [t])...
         in ...
When inferring a type for 'g', we don't want to apply the
instance decl, because then we can't satisfy (C t).  So we
just notice that g isn't quantified over 't' and partition
the constraints before simplifying.

This only half-works, but then let-generalisation only half-works.



Note [Delete dead Given evidence bindings]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
As a result of superclass expansion, we speculatively
generate evidence bindings for Givens. E.g.
   f :: (a ~ b) => a -> b -> Bool
   f x y = ...
We'll have
   [G] d1 :: (a~b)
and we'll specuatively generate the evidence binding
   [G] d2 :: (a ~# b) = sc_sel d

Now d2 is available for solving.  But it may not be needed!  Usually
such dead superclass selections will eventually be dropped as dead
code, but:

 * It won't always be dropped (#13032).  In the case of an
   unlifted-equality superclass like d2 above, we generate
       case heq_sc d1 of d2 -> ...
   and we can't (in general) drop that case exrpession in case
   d1 is bottom.  So it's technically unsound to have added it
   in the first place.

 * Simply generating all those extra superclasses can generate lots of
   code that has to be zonked, only to be discarded later.  Better not
   to generate it in the first place.

   Moreover, if we simplify this implication more than once
   (e.g. because we can't solve it completely on the first iteration
   of simpl_looop), we'll generate all the same bindings AGAIN!

Easy solution: take advantage of the work we are doing to track dead
(unused) Givens, and use it to prune the Given bindings too.  This is
all done by neededEvVars.

This led to a remarkable 25% overall compiler allocation decrease in
test T12227.

But we don't get to discard all redundant equality superclasses, alas;
see #15205.



Note [Tracking redundant constraints]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
With Opt_WarnRedundantConstraints, GHC can report which
constraints of a type signature (or instance declaration) are
redundant, and can be omitted.  Here is an overview of how it
works:

----- What is a redundant constraint?

* The things that can be redundant are precisely the Given
  constraints of an implication.

* A constraint can be redundant in two different ways:
  a) It is implied by other givens.  E.g.
       f :: (Eq a, Ord a)     => blah   -- Eq a unnecessary
       g :: (Eq a, a~b, Eq b) => blah   -- Either Eq a or Eq b unnecessary
  b) It is not needed by the Wanted constraints covered by the
     implication E.g.
       f :: Eq a => a -> Bool
       f x = True  -- Equality not used

*  To find (a), when we have two Given constraints,
   we must be careful to drop the one that is a naked variable (if poss).
   So if we have
       f :: (Eq a, Ord a) => blah
   then we may find [G] sc_sel (d1::Ord a) :: Eq a
                    [G] d2 :: Eq a
   We want to discard d2 in favour of the superclass selection from
   the Ord dictionary.  This is done by TcInteract.solveOneFromTheOther
   See Note [Replacement vs keeping].

* To find (b) we need to know which evidence bindings are 'wanted';
  hence the eb_is_given field on an EvBind.

----- How tracking works

* The ic_need fields of an Implic records in-scope (given) evidence
  variables bound by the context, that were needed to solve this
  implication (so far).  See the declaration of Implication.

* When the constraint solver finishes solving all the wanteds in
  an implication, it sets its status to IC_Solved

  - The ics_dead field, of IC_Solved, records the subset of this
    implication's ic_given that are redundant (not needed).

* We compute which evidence variables are needed by an implication
  in setImplicationStatus.  A variable is needed if
    a) it is free in the RHS of a Wanted EvBind,
    b) it is free in the RHS of an EvBind whose LHS is needed,
    c) it is in the ics_need of a nested implication.

* We need to be careful not to discard an implication
  prematurely, even one that is fully solved, because we might
  thereby forget which variables it needs, and hence wrongly
  report a constraint as redundant.  But we can discard it once
  its free vars have been incorporated into its parent; or if it
  simply has no free vars. This careful discarding is also
  handled in setImplicationStatus.

----- Reporting redundant constraints

* TcErrors does the actual warning, in warnRedundantConstraints.

* We don't report redundant givens for *every* implication; only
  for those which reply True to TcSimplify.warnRedundantGivens:

   - For example, in a class declaration, the default method *can*
     use the class constraint, but it certainly doesn't *have* to,
     and we don't want to report an error there.

   - More subtly, in a function definition
       f :: (Ord a, Ord a, Ix a) => a -> a
       f x = rhs
     we do an ambiguity check on the type (which would find that one
     of the Ord a constraints was redundant), and then we check that
     the definition has that type (which might find that both are
     redundant).  We don't want to report the same error twice, so we
     disable it for the ambiguity check.  Hence using two different
     FunSigCtxts, one with the warn-redundant field set True, and the
     other set False in
        - TcBinds.tcSpecPrag
        - TcBinds.tcTySig

  This decision is taken in setImplicationStatus, rather than TcErrors
  so that we can discard implication constraints that we don't need.
  So ics_dead consists only of the *reportable* redundant givens.

----- Shortcomings

Consider (see #9939)
    f2 :: (Eq a, Ord a) => a -> a -> Bool
    -- Ord a redundant, but Eq a is reported
    f2 x y = (x == y)

We report (Eq a) as redundant, whereas actually (Ord a) is.  But it's
really not easy to detect that!




Note [Cutting off simpl_loop]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It is very important not to iterate in simpl_loop unless there is a chance
of progress.  #8474 is a classic example:

  * There's a deeply-nested chain of implication constraints.
       ?x:alpha => ?y1:beta1 => ... ?yn:betan => [W] ?x:Int

  * From the innermost one we get a [D] alpha ~ Int,
    but alpha is untouchable until we get out to the outermost one

  * We float [D] alpha~Int out (it is in floated_eqs), but since alpha
    is untouchable, the solveInteract in simpl_loop makes no progress

  * So there is no point in attempting to re-solve
       ?yn:betan => [W] ?x:Int
    via solveNestedImplications, because we'll just get the
    same [D] again

  * If we *do* re-solve, we'll get an ininite loop. It is cut off by
    the fixed bound of 10, but solving the next takes 10*10*...*10 (ie
    exponentially many) iterations!

Conclusion: we should call solveNestedImplications only if we did
some unification in solveSimpleWanteds; because that's the only way
we'll get more Givens (a unification is like adding a Given) to
allow the implication to make progress.


Note [ApproximateWC]
~~~~~~~~~~~~~~~~~~~~~~~
approximateWC takes a constraint, typically arising from the RHS of a
let-binding whose type we are *inferring*, and extracts from it some
*simple* constraints that we might plausibly abstract over.  Of course
the top-level simple constraints are plausible, but we also float constraints
out from inside, if they are not captured by skolems.

The same function is used when doing type-class defaulting (see the call
to applyDefaultingRules) to extract constraints that that might be defaulted.

There is one caveat:

1.  When infering most-general types (in simplifyInfer), we do *not*
    float anything out if the implication binds equality constraints,
    because that defeats the OutsideIn story.  Consider
       data T a where
         TInt :: T Int
         MkT :: T a

       f TInt = 3::Int

    We get the implication (a ~ Int => res ~ Int), where so far we've decided
      f :: T a -> res
    We don't want to float (res~Int) out because then we'll infer
      f :: T a -> Int
    which is only on of the possible types. (GHC 7.6 accidentally *did*
    float out of such implications, which meant it would happily infer
    non-principal types.)

   HOWEVER (#12797) in findDefaultableGroups we are not worried about
   the most-general type; and we /do/ want to float out of equalities.
   Hence the boolean flag to approximateWC.

------ Historical note -----------
There used to be a second caveat, driven by #8155

   2. We do not float out an inner constraint that shares a type variable
      (transitively) with one that is trapped by a skolem.  Eg
          forall a.  F a ~ beta, Integral beta
      We don't want to float out (Integral beta).  Doing so would be bad
      when defaulting, because then we'll default beta:=Integer, and that
      makes the error message much worse; we'd get
          Can't solve  F a ~ Integer
      rather than
          Can't solve  Integral (F a)

      Moreover, floating out these "contaminated" constraints doesn't help
      when generalising either. If we generalise over (Integral b), we still
      can't solve the retained implication (forall a. F a ~ b).  Indeed,
      arguably that too would be a harder error to understand.

But this transitive closure stuff gives rise to a complex rule for
when defaulting actually happens, and one that was never documented.
Moreover (#12923), the more complex rule is sometimes NOT what
you want.  So I simply removed the extra code to implement the
contamination stuff.  There was zero effect on the testsuite (not even
#8155).
------ End of historical note -----------




Note [DefaultTyVar]
~~~~~~~~~~~~~~~~~~~
defaultTyVar is used on any un-instantiated meta type variables to
default any RuntimeRep variables to LiftedRep.  This is important
to ensure that instance declarations match.  For example consider

     instance Show (a->b)
     foo x = show (\_ -> True)

Then we'll get a constraint (Show (p ->q)) where p has kind (TYPE r),
and that won't match the tcTypeKind (*) in the instance decl.  See tests
tc217 and tc175.

We look only at touchable type variables. No further constraints
are going to affect these type variables, so it's time to do it by
hand.  However we aren't ready to default them fully to () or
whatever, because the type-class defaulting rules have yet to run.

An alternate implementation would be to emit a derived constraint setting
the RuntimeRep variable to LiftedRep, but this seems unnecessarily indirect.



Note [Promote _and_ default when inferring]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we are inferring a type, we simplify the constraint, and then use
approximateWC to produce a list of candidate constraints.  Then we MUST

  a) Promote any meta-tyvars that have been floated out by
     approximateWC, to restore invariant (WantedInv) described in
     Note [TcLevel and untouchable type variables] in TcType.

  b) Default the kind of any meta-tyvars that are not mentioned in
     in the environment.

To see (b), suppose the constraint is (C ((a :: OpenKind) -> Int)), and we
have an instance (C ((x:*) -> Int)).  The instance doesn't match -- but it
should!  If we don't solve the constraint, we'll stupidly quantify over
(C (a->Int)) and, worse, in doing so skolemiseQuantifiedTyVar will quantify over
(b:*) instead of (a:OpenKind), which can lead to disaster; see #7332.
#7641 is a simpler example.



Note [Promoting unification variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we float an equality out of an implication we must "promote" free
unification variables of the equality, in order to maintain Invariant
(WantedInv) from Note [TcLevel and untouchable type variables] in
TcType.  for the leftover implication.

This is absolutely necessary. Consider the following example. We start
with two implications and a class with a functional dependency.

    class C x y | x -> y
    instance C [a] [a]

    (I1)      [untch=beta]forall b. 0 => F Int ~ [beta]
    (I2)      [untch=beta]forall c. 0 => F Int ~ [[alpha]] /\ C beta [c]

We float (F Int ~ [beta]) out of I1, and we float (F Int ~ [[alpha]]) out of I2.
They may react to yield that (beta := [alpha]) which can then be pushed inwards
the leftover of I2 to get (C [alpha] [a]) which, using the FunDep, will mean that
(alpha := a). In the end we will have the skolem 'b' escaping in the untouchable
beta! Concrete example is in indexed_types/should_fail/ExtraTcsUntch.hs:

    class C x y | x -> y where
     op :: x -> y -> ()

    instance C [a] [a]

    type family F a :: *

    h :: F Int -> ()
    h = undefined

    data TEx where
      TEx :: a -> TEx

    f (x::beta) =
        let g1 :: forall b. b -> ()
            g1 _ = h [x]
            g2 z = case z of TEx y -> (h [[undefined]], op x [y])
        in (g1 '3', g2 undefined)





Note [Float Equalities out of Implications]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For ordinary pattern matches (including existentials) we float
equalities out of implications, for instance:
     data T where
       MkT :: Eq a => a -> T
     f x y = case x of MkT _ -> (y::Int)
We get the implication constraint (x::T) (y::alpha):
     forall a. [untouchable=alpha] Eq a => alpha ~ Int
We want to float out the equality into a scope where alpha is no
longer untouchable, to solve the implication!

But we cannot float equalities out of implications whose givens may
yield or contain equalities:

      data T a where
        T1 :: T Int
        T2 :: T Bool
        T3 :: T a

      h :: T a -> a -> Int

      f x y = case x of
                T1 -> y::Int
                T2 -> y::Bool
                T3 -> h x y

We generate constraint, for (x::T alpha) and (y :: beta):
   [untouchables = beta] (alpha ~ Int => beta ~ Int)   -- From 1st branch
   [untouchables = beta] (alpha ~ Bool => beta ~ Bool) -- From 2nd branch
   (alpha ~ beta)                                      -- From 3rd branch

If we float the equality (beta ~ Int) outside of the first implication and
the equality (beta ~ Bool) out of the second we get an insoluble constraint.
But if we just leave them inside the implications, we unify alpha := beta and
solve everything.

Principle:
    We do not want to float equalities out which may
    need the given *evidence* to become soluble.

Consequence: classes with functional dependencies don't matter (since there is
no evidence for a fundep equality), but equality superclasses do matter (since
they carry evidence).


Note [Float equalities from under a skolem binding]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Which of the simple equalities can we float out?  Obviously, only
ones that don't mention the skolem-bound variables.  But that is
over-eager. Consider
   [2] forall a. F a beta[1] ~ gamma[2], G beta[1] gamma[2] ~ Int
The second constraint doesn't mention 'a'.  But if we float it,
we'll promote gamma[2] to gamma'[1].  Now suppose that we learn that
beta := Bool, and F a Bool = a, and G Bool _ = Int.  Then we'll
we left with the constraint
   [2] forall a. a ~ gamma'[1]
which is insoluble because gamma became untouchable.

Solution: float only constraints that stand a jolly good chance of
being soluble simply by being floated, namely ones of form
      a ~ ty
where 'a' is a currently-untouchable unification variable, but may
become touchable by being floated (perhaps by more than one level).

We had a very complicated rule previously, but this is nice and
simple.  (To see the notes, look at this Note in a version of
TcSimplify prior to Oct 2014).



Note [Which equalities to float]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Which equalities should we float?  We want to float ones where there
is a decent chance that floating outwards will allow unification to
happen.  In particular, float out equalities that are:

* Of form (alpha ~# ty) or (ty ~# alpha), where
   * alpha is a meta-tyvar.
   * And 'alpha' is not a TyVarTv with 'ty' being a non-tyvar.  In that
     case, floating out won't help either, and it may affect grouping
     of error messages.

* Homogeneous (both sides have the same kind). Why only homogeneous?
  Because heterogeneous equalities have derived kind equalities.
  See Note [Equalities with incompatible kinds] in TcCanonical.
  If we float out a hetero equality, then it will spit out the same
  derived kind equality again, which might create duplicate error
  messages.

  Instead, we do float out the kind equality (if it's worth floating
  out, as above). If/when we solve it, we'll be able to rewrite the
  original hetero equality to be homogeneous, and then perhaps make
  progress / float it out. The duplicate error message was spotted in
  typecheck/should_fail/T7368.

* Nominal.  No point in floating (alpha ~R# ty), because we do not
  unify representational equalities even if alpha is touchable.
  See Note [Do not unify representational equalities] in TcInteract.



Note [Skolem escape]
~~~~~~~~~~~~~~~~~~~~
You might worry about skolem escape with all this floating.
For example, consider
    [2] forall a. (a ~ F beta[2] delta,
                   Maybe beta[2] ~ gamma[1])

The (Maybe beta ~ gamma) doesn't mention 'a', so we float it, and
solve with gamma := beta. But what if later delta:=Int, and
  F b Int = b.
Then we'd get a ~ beta[2], and solve to get beta:=a, and now the
skolem has escaped!

But it's ok: when we float (Maybe beta[2] ~ gamma[1]), we promote beta[2]
to beta[1], and that means the (a ~ beta[1]) will be stuck, as it should be.



Note [What prevents a constraint from floating]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
What /prevents/ a constraint from floating?  If it mentions one of the
"bound variables of the implication".  What are they?

The "bound variables of the implication" are

  1. The skolem type variables `ic_skols`

  2. The "given" evidence variables `ic_given`.  Example:
         forall a. (co :: t1 ~# t2) =>  [W] co2 : (a ~# b |> co)
     Here 'co' is bound

  3. The binders of all evidence bindings in `ic_binds`. Example
         forall a. (d :: t1 ~ t2)
            EvBinds { (co :: t1 ~# t2) = superclass-sel d }
            => [W] co2 : (a ~# b |> co)
     Here `co` is gotten by superclass selection from `d`, and the
     wanted constraint co2 must not float.

  4. And the evidence variable of any equality constraint (incl
     Wanted ones) whose type mentions a bound variable.  Example:
        forall k. [W] co1 :: t1 ~# t2 |> co2
                  [W] co2 :: k ~# *
     Here, since `k` is bound, so is `co2` and hence so is `co1`.

Here (1,2,3) are handled by the "seed_skols" calculation, and
(4) is done by the transCloVarSet call.

The possible dependence on givens, and evidence bindings, is more
subtle than we'd realised at first.  See #14584.




Note [Avoiding spurious errors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When doing the unification for defaulting, we check for skolem
type variables, and simply don't default them.  For example:
   f = (*)      -- Monomorphic
   g :: Num a => a -> a
   g x = f x x
Here, we get a complaint when checking the type signature for g,
that g isn't polymorphic enough; but then we get another one when
dealing with the (Num a) context arising from f's definition;
we try to unify a with Int (to default it), but find that it's
already been unified with the rigid variable from g's type sig.



Note [Multi-parameter defaults]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
With -XExtendedDefaultRules, we default only based on single-variable
constraints, but do not exclude from defaulting any type variables which also
appear in multi-variable constraints. This means that the following will
default properly:

   default (Integer, Double)

   class A b (c :: Symbol) where
      a :: b -> Proxy c

   instance A Integer c where a _ = Proxy

   main = print (a 5 :: Proxy "5")

Note that if we change the above instance ("instance A Integer") to
"instance A Double", we get an error:

   No instance for (A Integer "5")

This is because the first defaulted type (Integer) has successfully satisfied
its single-parameter constraints (in this case Num).
