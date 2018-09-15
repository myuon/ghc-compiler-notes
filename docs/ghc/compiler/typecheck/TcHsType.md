[[src]](https://github.com/ghc/ghc/tree/master/compiler/typecheck/TcHsType.hs)

(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

# Typechecking user-specified @MonoTypes@


        ----------------------------
                General notes
        ----------------------------

Unlike with expressions, type-checking types both does some checking and
desugars at the same time. This is necessary because we often want to perform
equality checks on the types right away, and it would be incredibly painful
to do this on un-desugared types. Luckily, desugared types are close enough
to HsTypes to make the error messages sane.

During type-checking, we perform as little validity checking as possible.
This is because some type-checking is done in a mutually-recursive knot, and
if we look too closely at the tycons, we'll loop. This is why we always must
use mkNakedTyConApp and mkNakedAppTys, etc., which never look at a tycon.
The mkNamed... functions don't uphold Type invariants, but zonkTcTypeToType
will repair this for us. Note that zonkTcType *is* safe within a knot, and
can be done repeatedly with no ill effect: it just squeezes out metavariables.

Generally, after type-checking, you will want to do validity checking, say
with TcValidity.checkValidType.

# checking

Some of the validity check could in principle be done by the kind checker,
but not all:

- During desugaring, we normalise by expanding type synonyms.  Only
  after this step can we check things like type-synonym saturation
  e.g.  type T k = k Int
        type S a = a
  Then (T S) is ok, because T is saturated; (T S) expands to (S Int);
  and then S is saturated.  This is a GHC extension.

- Similarly, also a GHC extension, we look through synonyms before complaining
  about the form of a class or instance declaration

- Ambiguity checks involve functional dependencies, and it's easier to wait
  until knots have been resolved before poking into them

Also, in a mutually recursive group of types, we can't look at the TyCon until we've
finished building the loop.  So to keep things simple, we postpone most validity
checking until step (3).

# tying

During step (1) we might fault in a TyCon defined in another module, and it might
(via a loop) refer back to a TyCon defined in this module. So when we tie a big
knot around type declarations with ARecThing, so that the fault-in code can get
the TyCon being defined.

# 

# The main kind checker: no validity checks here


        First a couple of simple wrappers for kcHsType


### Note: Kind generalisation plan

When should we do kind-generalisation for user-written type signature?
Answer: we use the same rule as for value bindings:

 * We always kind-generalise if the type signature is closed
 * Additionally, we attempt to generalise if we have NoMonoLocalBinds

Trac #13337 shows the problem if we kind-generalise an open type (i.e.
one that mentions in-scope tpe variable
  foo :: forall k (a :: k) proxy. (Typeable k, Typeable a)
      => proxy a -> String
  foo _ = case eqT :: Maybe (k :~: Type) of
            Nothing   -> ...
            Just Refl -> case eqT :: Maybe (a :~: Int) of ...

In the expression type sig on the last line, we have (a :: k)
but (Int :: Type).  Since (:~:) is kind-homogeneous, this requires
k ~ *, which is true in the Refl branch of the outer case.

That equality will be solved if we allow it to float out to the
implication constraint for the Refl match, bnot not if we aggressively
attempt to solve all equalities the moment they occur; that is, when
checking (Maybe (a :~: Int)).   (NB: solveEqualities fails unless it
solves all the kind equalities, which is the right thing at top level.)

So here the right thing is simply not to do kind generalisation!

# Type-checking modes


The kind-checker is parameterised by a TcTyMode, which contains some
information about where we're checking a type.

The renamer issues errors about what it can. All errors issued here must
concern things that the renamer can't handle.



### Note: Bidirectional type checking

In expressions, whenever we see a polymorphic identifier, say `id`, we are
free to instantiate it with metavariables, knowing that we can always
re-generalize with type-lambdas when necessary. For example:

  rank2 :: (forall a. a -> a) -> ()
  x = rank2 id

When checking the body of `x`, we can instantiate `id` with a metavariable.
Then, when we're checking the application of `rank2`, we notice that we really
need a polymorphic `id`, and then re-generalize over the unconstrained
metavariable.

In types, however, we're not so lucky, because *we cannot re-generalize*!
There is no lambda. So, we must be careful only to instantiate at the last
possible moment, when we're sure we're never going to want the lost polymorphism
again. This is done in calls to tcInstBinders.

To implement this behavior, we use bidirectional type checking, where we
explicitly think about whether we know the kind of the type we're checking
or not. Note that there is a difference between not knowing a kind and
knowing a metavariable kind: the metavariables are TauTvs, and cannot become
forall-quantified kinds. Previously (before dependent types), there were
no higher-rank kinds, and so we could instantiate early and be sure that
no types would have polymorphic kinds, and so we could always assume that
the kind of a type was a fresh metavariable. Not so anymore, thus the
need for two algorithms.

For HsType forms that can never be kind-polymorphic, we implement only the
"down" direction, where we safely assume a metavariable kind. For HsType forms
that *can* be kind-polymorphic, we implement just the "up" (functions with
"infer" in their name) version, as we gain nothing by also implementing the
"down" version.

### Note: Future-proofing the type checker

### Note: Bidirectional type checking

### Note: Type-checking inside the knot

Suppose we are checking the argument types of a data constructor.  We
must zonk the types before making the DataCon, because once built we
can't change it.  So we must traverse the type.

BUT the parent TyCon is knot-tied, so we can't look at it yet.

So we must be careful not to use "smart constructors" for types that
look at the TyCon or Class involved.

  * Hence the use of mkNakedXXX functions. These do *not* enforce
    the invariants (for example that we use (FunTy s t) rather
    than (TyConApp (->) [s,t])).

  * The zonking functions establish invariants (even zonkTcType, a change from
    previous behaviour). So we must never inspect the result of a
    zonk that might mention a knot-tied TyCon. This is generally OK
    because we zonk *kinds* while kind-checking types. And the TyCons
    in kinds shouldn't be knot-tied, because they come from a previous
    mutually recursive group.

  * TcHsSyn.zonkTcTypeToType also can safely check/establish
    invariants.

This is horribly delicate.  I hate it.  A good example of how
delicate it is can be seen in Trac #7903.

### Note: GADT kind self-reference


A promoted type cannot be used in the body of that type's declaration.
Trac #11554 shows this example, which made GHC loop:

  import Data.Kind
  data P (x :: k) = Q
  data A :: Type where
    B :: forall (a :: A). P a -> A

In order to check the constructor B, we need to have the promoted type A, but in
order to get that promoted type, B must first be checked. To prevent looping, a
TyConPE promotion error is given when tcTyVar checks an ATcTyCon in kind mode.
Any ATcTyCon is a TyCon being defined in the current recursive group (see data
type decl for TcTyThing), and all such TyCons are illegal in kinds.

Trac #11962 proposes checking the head of a data declaration separately from
its constructors. This would allow the example above to pass.

### Note: Body kind of a HsForAllTy

The body of a forall is usually a type, but in principle
there's no reason to prohibit *unlifted* types.
In fact, GHC can itself construct a function with an
unboxed tuple inside a for-all (via CPR analysis; see
typecheck/should_compile/tc170).

Moreover in instance heads we get forall-types with
kind Constraint.

It's tempting to check that the body kind is either * or #. But this is
wrong. For example:

  class C a b
  newtype N = Mk Foo deriving (C a)

We're doing newtype-deriving for C. But notice how `a` isn't in scope in
the predicate `C a`. So we quantify, yielding `forall a. C a` even though
`C a` has kind `* -> Constraint`. The `forall a. C a` is a bit cheeky, but
convenient. Bottom line: don't check for * or # here.

### Note: Body kind of a HsQualTy

If ctxt is non-empty, the HsQualTy really is a /function/, so the
kind of the result really is '*', and in that case the kind of the
body-type can be lifted or unlifted.

However, consider
    instance Eq a => Eq [a] where ...
or
    f :: (Eq a => Eq [a]) => blah
Here both body-kind of the HsQualTy is Constraint rather than *.
Rather crudely we tell the difference by looking at exp_kind. It's
very convenient to typecheck instance types like any other HsSigType.

Admittedly the '(Eq a => Eq [a]) => blah' case is erroneous, but it's
better to reject in checkValidType.  If we say that the body kind
should be '*' we risk getting TWO error messages, one saying that Eq
[a] doens't have kind '*', and one saying that we need a Constraint to
the left of the outer (=>).

How do we figure out the right body kind?  Well, it's a bit of a
kludge: I just look at the expected kind.  If it's Constraint, we
must be in this instance situation context. It's a kludge because it
wouldn't work if any unification was involved to compute that result
kind -- but it isn't.  (The true way might be to use the 'mode'
parameter, but that seemed like a sledgehammer to crack a nut.)

### Note: Inferring tuple kinds

Give a tuple type (a,b,c), which the parser labels as HsBoxedOrConstraintTuple,
we try to figure out whether it's a tuple of kind * or Constraint.
  Step 1: look at the expected kind
  Step 2: infer argument kinds

If after Step 2 it's not clear from the arguments that it's
Constraint, then it must be *.  Once having decided that we re-check
the Check the arguments again to give good error messages
in eg. `(Maybe, Maybe)`

Note that we will still fail to infer the correct kind in this case:

  type T a = ((a,a), D a)
  type family D :: Constraint -> Constraint

While kind checking T, we do not yet know the kind of D, so we will default the
kind of T to * -> *. It works if we annotate `a` with kind `Constraint`.

### Note: Desugaring types

The type desugarer is phase 2 of dealing with HsTypes.  Specifically:

  * It transforms from HsType to Type

  * It zonks any kinds.  The returned type should have no mutable kind
    or type variables (hence returning Type not TcType):
      - any unconstrained kind variables are defaulted to (Any *) just
        as in TcHsSyn.
      - there are no mutable type variables because we are
        kind-checking a type
    Reason: the returned type may be put in a TyCon or DataCon where
    it will never subsequently be zonked.

You might worry about nested scopes:
        ..a:kappa in scope..
            let f :: forall b. T '[a,b] -> Int
In this case, f's type could have a mutable kind variable kappa in it;
and we might then default it to (Any *) when dealing with f's type
signature.  But we don't expect this to happen because we can't get a
lexically scoped type variable with a mutable kind variable in it.  A
delicate point, this.  If it becomes an issue we might need to
distinguish top-level from nested uses.

Moreover
  * it cannot fail,
  * it does no unifications
  * it does no validity checking, except for structural matters, such as
        (a) spurious ! annotations.
        (b) a class used as a type

### Note: Kind of a type splice

Consider these terms, each with TH type splice inside:
     [| e1 :: Maybe $(..blah..) |]
     [| e2 :: $(..blah..) |]
When kind-checking the type signature, we'll kind-check the splice
$(..blah..); we want to give it a kind that can fit in any context,
as if $(..blah..) :: forall k. k.

In the e1 example, the context of the splice fixes kappa to *.  But
in the e2 example, we'll desugar the type, zonking the kind unification
variables as we go.  When we encounter the unconstrained kappa, we
want to default it to '*', not to (Any *).

# Help functions for type applications



# 

### Note: Scope-check inferred kinds

Consider

  data SameKind :: k -> k -> *
  foo :: forall a (b :: Proxy a) (c :: Proxy d). SameKind b c

d has no binding site. So it gets bound implicitly, at the top. The
problem is that d's kind mentions `a`. So it's all ill-scoped.

The way we check for this is to gather all variables *bound* in a
type variable's scope. The type variable's kind should not mention
any of these variables. That is, d's kind can't mention a, b, or c.
We can't just check to make sure that d's kind is in scope, because
we might be about to kindGeneralize.

A little messy, but it works.

### Note: Dependent LHsQTyVars

We track (in the renamer) which explicitly bound variables in a
LHsQTyVars are manifestly dependent; only precisely these variables
may be used within the LHsQTyVars. We must do this so that kcLHsQTyVars
can produce the right TyConBinders, and tell Anon vs. Required.

Example   data T k1 (a:k1) (b:k2) c
               = MkT (Proxy a) (Proxy b) (Proxy c)

Here
  (a:k1),(b:k2),(c:k3)
       are Anon     (explicitly specified as a binder, not used
                     in the kind of any other binder
  k1   is Required  (explicitly specifed as a binder, but used
                     in the kind of another binder i.e. dependently)
  k2   is Specified (not explicitly bound, but used in the kind
                     of another binder)
  k3   in Inferred  (not lexically in scope at all, but inferred
                     by kind inference)
and
  T :: forall {k3} k1. forall k3 -> k1 -> k2 -> k3 -> *

### Note: TyVarBndrs, TyVarBinders, TyConBinders, and visibility

kcLHsQTyVars uses the hsq_dependent field to decide whether
k1, a, b, c should be Required or Anon.

Earlier, thought it would work simply to do a free-variable check
during kcLHsQTyVars, but this is bogus, because there may be
unsolved equalities about. And we don't want to eagerly solve the
equalities, because we may get further information after
kcLHsQTyVars is called.  (Recall that kcLHsQTyVars is usually
called from getInitialKind.  The only other case is in kcConDecl.)
This is what implements the rule that all variables intended to be
dependent must be manifestly so.

Sidenote: It's quite possible that later, we'll consider (t -> s)
as a degenerate case of some (pi (x :: t) -> s) and then this will
all get more permissive.

### Note: Kind generalisation and SigTvs

Consider
  data T (a :: k1) x = MkT (S a ())
  data S (b :: k2) y = MkS (T b ())

### Note: Quantified variables in partial type signatures

There are some wrinkles

* We always want to kind-generalise over SigTvs, and /not/ default
  them to Type.  Another way to say this is: a SigTV should /never/
  stand for a type, even via defaulting. Hence the check in
  TcSimplify.defaultTyVarTcS, and TcMType.defaultTyVar.  Here's
  another example (Trac #14555):
     data Exp :: [TYPE rep] -> TYPE rep -> Type where
        Lam :: Exp (a:xs) b -> Exp xs (a -> b)
  We want to kind-generalise over the 'rep' variable.
  Trac #14563 is another example.

* Consider Trac #11203
    data SameKind :: k -> k -> *
    data Q (a :: k1) (b :: k2) c = MkQ (SameKind a b)
  Here we will unify k1 with k2, but this time doing so is an error,
  because k1 and k2 are bound in the same delcaration.

  We sort this out using findDupSigTvs, in TcTyClTyVars; very much
  as we do with partial type signatures in mk_psig_qtvs in
  TcBinds.chooseInferredQuantifiers


### Note: Kind generalisation

We do kind generalisation only at the outer level of a type signature.
For example, consider
  T :: forall k. k -> *
  f :: (forall a. T a -> Int) -> Int
When kind-checking f's type signature we generalise the kind at
the outermost level, thus:
  f1 :: forall k. (forall (a:k). T k a -> Int) -> Int  -- YES!
and *not* at the inner forall:
  f2 :: (forall k. forall (a:k). T k a -> Int) -> Int  -- NO!
Reason: same as for HM inference on value level declarations,
we want to infer the most general type.  The f2 type signature
would be *less applicable* than f1, because it requires a more
polymorphic argument.

NB: There are no explicit kind variables written in f's signature.
When there are, the renamer adds these kind variables to the list of
variables bound by the forall, so you can indeed have a type that's
higher-rank in its kind. But only by explicit request.

### Note: Kinds of quantified type variables

tcTyVarBndrsGen quantifies over a specified list of type variables,
*and* over the kind variables mentioned in the kinds of those tyvars.

Note that we must zonk those kinds (obviously) but less obviously, we
must return type variables whose kinds are zonked too. Example
    (a :: k7)  where  k7 := k9 -> k9
We must return
    [k9, a:k9->k9]
and NOT
    [k9, a:k7]
Reason: we're going to turn this into a for-all type,
   forall k9. forall (a:k7). blah
which the type checker will then instantiate, and instantiate does not
look through unification variables!

Hence using zonked_kinds when forming tvs'.

### Note: Free-floating kind vars

Consider

  data T = MkT (forall (a :: k). Proxy a)
  -- from test ghci/scripts/T7873

This is not an existential datatype, but a higher-rank one (the forall
to the right of MkT). Also consider

  data S a = MkS (Proxy (a :: k))

According to the rules around implicitly-bound kind variables, in both
cases those k's scope over the whole declaration. The renamer grabs
it and adds it to the hsq_implicits field of the HsQTyVars of the
tycon. So it must be in scope during type-checking, but we want to
reject T while accepting S.

Why reject T? Because the kind variable isn't fixed by anything. For
a variable like k to be implicit, it needs to be mentioned in the kind
of a tycon tyvar. But it isn't.

Why accept S? Because kind inference tells us that a has kind k, so it's
all OK.

Our approach depends on whether or not the datatype has a CUSK.

Non-CUSK: In the first pass (kcTyClTyVars) we just bring
k into scope. In the second pass (tcTyClTyVars),
we check to make sure that k has been unified with some other variable
(or generalized over, making k into a skolem). If it hasn't been, then
it must be a free-floating kind var. Error.

CUSK: When we determine the tycon's final, never-to-be-changed kind
in kcLHsQTyVars, we check to make sure all implicitly-bound kind
vars are indeed mentioned in a kind somewhere. If not, error.

We also perform free-floating kind var analysis for type family instances
(see #13985). Here is an interesting example:

    type family   T :: k
    type instance T = (Nothing :: Maybe a)

Upon a cursory glance, it may appear that the kind variable `a` is
free-floating above, since there are no (visible) LHS patterns in `T`. However,
there is an *invisible* pattern due to the return kind, so inside of GHC, the
instance looks closer to this:

    type family T @k :: k
    type instance T @(Maybe a) = (Nothing :: Maybe a)

Here, we can see that `a` really is bound by a LHS type pattern, so `a` is in
fact not free-floating. Contrast that with this example:

    type instance T = Proxy (Nothing :: Maybe a)

This would looks like this inside of GHC:

    type instance T @(*) = Proxy (Nothing :: Maybe a)

So this time, `a` is neither bound by a visible nor invisible type pattern on
the LHS, so it would be reported as free-floating.

Finally, here's one more brain-teaser (from #9574). In the example below:

    class Funct f where
      type Codomain f :: *
    instance Funct ('KProxy :: KProxy o) where
      type Codomain 'KProxy = NatTr (Proxy :: o -> *)

As it turns out, `o` is not free-floating in this example. That is because `o`
bound by the kind signature of the LHS type pattern 'KProxy. To make this more
obvious, one can also write the instance like so:

    instance Funct ('KProxy :: KProxy o) where
      type Codomain ('KProxy :: KProxy o) = NatTr (Proxy :: o -> *)



### Note: TyConBinders for the result kind signature of a data type

Given
  data T (a::*) :: * -> forall k. k -> *
we want to generate the extra TyConBinders for T, so we finally get
  (a::*) (b::*) (k::*) (c::k)
The function tcDataKindSig generates these extra TyConBinders from
the result kind signature.

We need to take care to give the TyConBinders
  (a) OccNames that are fresh (because the TyConBinders of a TyCon
      must have distinct OccNames

  (b) Uniques that are fresh (obviously)

### Note: Avoid name clashes for associated data types

For (b) suppose we have
   data T :: forall k. k -> forall k. k -> *
where the two k's are identical even up to their uniques.  Surprisingly,
this can happen: see Trac #14515.

It's reasonably easy to solve all this; just run down the list with a
substitution; hence the recursive 'go' function.  But it has to be
done.

### Note: Avoid name clashes for associated data types

Consider    class C a b where
               data D b :: * -> *
When typechecking the decl for D, we'll invent an extra type variable
for D, to fill out its kind.  Ideally we don't want this type variable
to be 'a', because when pretty printing we'll get
            class C a b where
               data D b a0
(NB: the tidying happens in the conversion to IfaceSyn, which happens
as part of pretty-printing a TyThing.)

That's why we look in the LocalRdrEnv to see what's in scope. This is
important only to get nice-looking output when doing ":info C" in GHCi.
It isn't essential for correctness.

# Partial signatures




### Note: Extra-constraint holes in partial type signatures

Consider
  f :: (_) => a -> a
  f x = ...

* The renamer makes a wildcard name for the "_", and puts it in
  the hswc_wcs field.

* Then, in tcHsPartialSigType, we make a new hole TcTyVar, in
  tcWildCardBindersX.

* TcBinds.chooseInferredQuantifiers fills in that hole TcTyVar
  with the inferred constraints, e.g. (Eq a, Show a)

* TcErrors.mkHoleError finally reports the error.

### Note: How tuples work

It's incredibly tiresome, because we only need this type to fill
in the hole, to communicate to the error reporting machinery.  Nothing
more.  So I use a HACK:

* I make an /ordinary/ tuple of the constraints, in
  TcBinds.chooseInferredQuantifiers. This is ill-kinded because
  ordinary tuples can't contain constraints, but it works fine. And for
  ordinary tuples we don't have the same limit as for constraint
  tuples (which need selectors and an assocated class).

* Because it is ill-kinded, it trips an assert in writeMetaTyVar,
  so now I disable the assertion if we are writing a type of
  kind Constraint.  (That seldom/never normally happens so we aren't
  losing much.)

Result works fine, but it may eventually bite us.

# Pattern signatures (i.e signatures that occur in patterns)


### Note: Pattern signature binders

Consider
   data T = forall a. T a (a->Int)
   f (T x (f :: b->Int)) = blah

Here
 * The pattern (T p1 p2) creates a *skolem* type variable 'a_sk',
   It must be a skolem so that that it retains its identity, and
   TcErrors.getSkolemInfo can thereby find the binding site for the skolem.

 * The type signature pattern (f :: b->Int) makes a fresh meta-tyvar b_sig
   (a SigTv), and binds "b" :-> b_sig in the envt

 * Then unification makes b_sig := a_sk
   That's why we must make b_sig a MetaTv (albeit a SigTv),
   not a SkolemTv, so that it can unify to a_sk.

 * Finally, in 'blah' we must have the envt "b" :-> a_sk.  The pair
   ("b" :-> a_sk) is returned by tcHsPatSigType, constructed by
   mk_tv_pair in that function.

Another example (Trac #13881):
   fl :: forall (l :: [a]). Sing l -> Sing l
   fl (SNil :: Sing (l :: [y])) = SNil
When we reach the pattern signature, 'l' is in scope from the
outer 'forall':
   "a" :-> a_sk :: *
   "l" :-> l_sk :: [a_sk]
We make up a fresh meta-SigTv, y_sig, for 'y', and kind-check
the pattern signature
   Sing (l :: [y])
That unifies y_sig := a_sk.  We return from tcHsPatSigType with
the pair ("y" :-> a_sk).

For RULE binders, though, things are a bit different (yuk).
  RULE "foo" forall (x::a) (y::[a]).  f x y = ...
Here this really is the binding site of the type variable so we'd like
to use a skolem, so that we get a complaint if we unify two of them
together.

### Note: Unifying SigTvs

ALAS we have no decent way of avoiding two SigTvs getting unified.
Consider
  f (x::(a,b)) (y::c)) = [fst x, y]
Here we'd really like to complain that 'a' and 'c' are unified. But
for the reasons above we can't make a,b,c into skolems, so they
are just SigTvs that can unify.  And indeed, this would be ok,
  f x (y::c) = case x of
                 (x1 :: a1, True) -> [x,y]
                 (x1 :: a2, False) -> [x,y,y]
Here the type of x's first component is called 'a1' in one branch and
'a2' in the other.  We could try insisting on the same OccName, but
they definitely won't have the sane lexical Name.

I think we could solve this by recording in a SigTv a list of all the
in-scope variables that it should not unify with, but it's fiddly.

# Checking kinds




# Sort checking kinds


tcLHsKindSig converts a user-written kind to an internal, sort-checked kind.
It does sort checking and desugaring at the same time, in one single pass.


# Scoped type variables


# Error messages and such
