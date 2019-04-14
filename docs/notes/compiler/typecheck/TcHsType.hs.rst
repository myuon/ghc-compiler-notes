`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcHsType.hs>`_

compiler/typecheck/TcHsType.hs
==============================


Note [Wildcards in visible type application]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcHsType.hs#L386>`__

A HsWildCardBndrs's hswc_ext now only includes named wildcards, so any unnamed
wildcards stay unchanged in hswc_body and when called in tcHsTypeApp, tcCheckLHsType
will call emitWildCardHoleConstraints on them. However, this would trigger
error/warning when an unnamed wildcard is passed in as a visible type argument,
which we do not want because users should be able to write @_ to skip a instantiating
a type variable variable without fuss. The solution is to switch the
PartialTypeSignatures flags here to let the typechecker know that it's checking
a '@_' and do not emit hole constraints on it.
See related Note [Wildcards in visible kind application]
and Note [The wildcard story for types] in HsTypes.hs



Note [Dealing with :kind]
~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcHsType.hs#L451>`__

Consider this GHCi command
  ghci> type family F :: Either j k
  ghci> :kind F
  F :: forall {j,k}. Either j k

We will only get the 'forall' if we /refrain/ from saturating those
invisible binders. But generally we /do/ saturate those invisible
binders (see tcInferApps), and we want to do so for nested application
even in GHCi.  Consider for example (#16287)
  ghci> type family F :: k
  ghci> data T :: (forall k. k) -> Type
  ghci> :kind T F
We want to reject this. It's just at the very top level that we want
to switch off saturation.

So tcLHsTypeUnsaturated does a little special case for top level
applications.  Actually the common case is a bare variable, as above.



Note [Bidirectional type checking]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcHsType.hs#L508>`__

In expressions, whenever we see a polymorphic identifier, say `id`, we are
free to instantiate it with metavariables, knowing that we can always
re-generalize with type-lambdas when necessary. For example:

::

  rank2 :: (forall a. a -> a) -> ()
  x = rank2 id

..

When checking the body of `x`, we can instantiate `id` with a metavariable.
Then, when we're checking the application of `rank2`, we notice that we really
need a polymorphic `id`, and then re-generalize over the unconstrained
metavariable.

In types, however, we're not so lucky, because *we cannot re-generalize*!
There is no lambda. So, we must be careful only to instantiate at the last
possible moment, when we're sure we're never going to want the lost polymorphism
again. This is done in calls to tcInstInvisibleTyBinders.

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



Note [Future-proofing the type checker]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcHsType.hs#L543>`__

As discussed in Note [Bidirectional type checking], each HsType form is
handled in *either* tc_infer_hs_type *or* tc_hs_type. These functions
are mutually recursive, so that either one can work for any type former.
But, we want to make sure that our pattern-matches are complete. So,
we have a bunch of repetitive code just so that we get warnings if we're
missing any patterns.



Note [Wildcards in visible kind application]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcHsType.hs#L850>`__

There are cases where users might want to pass in a wildcard as a visible kind
argument, for instance:

data T :: forall k1 k2. k1 → k2 → Type where
  MkT :: T a b
x :: T @_ @Nat False n
x = MkT

So we should allow '@_' without emitting any hole constraints, and
regardless of whether PartialTypeSignatures is enabled or not. But how would
the typechecker know which '_' is being used in VKA and which is not when it
calls emitWildCardHoleConstraints in tcHsPartialSigType on all HsWildCardBndrs?
The solution then is to neither rename nor include unnamed wildcards in HsWildCardBndrs,
but instead give every unnamed wildcard a fresh wild tyvar in tcWildCardOcc.
And whenever we see a '@', we automatically turn on PartialTypeSignatures and
turn off hole constraint warnings, and never call emitWildCardHoleConstraints
under these conditions.
See related Note [Wildcards in visible type application] here and
Note [The wildcard story for types] in HsTypes.hs



Note [The Purely Kinded Type Invariant (PKTI)]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcHsType.hs#L1212>`__

During type inference, we maintain this invariant

::

 (PKTI) It is legal to call 'tcTypeKind' on any Type ty,
        on any sub-term of ty, /without/ zonking ty

..

        Moreover, any such returned kind
        will itself satisfy (PKTI)

By "legal to call tcTypeKind" we mean "tcTypeKind will not crash".
The way in which tcTypeKind can crash is in applications
    (a t1 t2 .. tn)
if 'a' is a type variable whose kind doesn't have enough arrows
or foralls.  (The crash is in piResultTys.)

The loop in tcInferApps has to be very careful to maintain the (PKTI).
For example, suppose
    kappa is a unification variable
    We have already unified kappa := Type
      yielding    co :: Refl (Type -> Type)
    a :: kappa
then consider the type
    (a Int)
If we call tcTypeKind on that, we'll crash, because the (un-zonked)
kind of 'a' is just kappa, not an arrow kind.  So we must zonk first.

So the type inference engine is very careful when building applications.
This happens in tcInferApps. Suppose we are kind-checking the type (a Int),
where (a :: kappa).  Then in tcInferApps we'll run out of binders on
a's kind, so we'll call matchExpectedFunKind, and unify
   kappa := kappa1 -> kappa2,  with evidence co :: kappa ~ (kappa1 ~ kappa2)
At this point we must zonk the function type to expose the arrrow, so
that (a Int) will satisfy (PKTI).

The absence of this caused #14174 and #14520.

The calls to mkAppTyM is the other place we are very careful.



Note [mkAppTyM]
~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcHsType.hs#L1251>`__

mkAppTyM is trying to guaranteed the Purely Kinded Type Invariant
(PKTI) for its result type (fun arg).  There are two ways it can go wrong:

* Nasty case 1: forall types (polykinds/T14174a)
    T :: forall (p :: *->*). p Int -> p Bool
  Now kind-check (T x), where x::kappa.
  Well, T and x both satisfy the PKTI, but
     T x :: x Int -> x Bool
  and (x Int) does /not/ satisfy the PKTI.

* Nasty case 2: type synonyms
    type S f a = f a
  Even though (S ff aa) would satisfy the (PKTI) if S was a data type
  (i.e. nasty case 1 is dealt with), it might still not satisfy (PKTI)
  if S is a type synonym, because the /expansion/ of (S ff aa) is
  (ff aa), and /that/ does not satisfy (PKTI).  E.g. perhaps
  (ff :: kappa), where 'kappa' has already been unified with (*->*).

::

  We check for nasty case 2 on the final argument of a type synonym.

..

Notice that in both cases the trickiness only happens if the
bound variable has a pi-type.  Hence isTrickyTvBinder.



Note [saturateFamApp]
~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcHsType.hs#L1295>`__

Consider
   type family F :: Either j k
   type instance F @Type = Right Maybe
   type instance F @Type = Right Either```

Then F :: forall {j,k}. Either j k

The two type instances do a visible kind application that instantiates
'j' but not 'k'.  But we want to end up with instances that look like
  type instance F @Type @(*->*) = Right @Type @(*->*) Maybe

so that F has arity 2.  We must instantiate that trailing invisible
binder. In general, Invisible binders precede Specified and Required,
so this is only going to bite for apparently-nullary families.

Note that
  type family F2 :: forall k. k -> *
is quite different and really does have arity 0.

It's not just type instances where we need to saturate those
unsaturated arguments: see #11246.  Hence doing this in tcInferApps.



Note [GADT kind self-reference]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcHsType.hs#L1459>`__

A promoted type cannot be used in the body of that type's declaration.
#11554 shows this example, which made GHC loop:

  import Data.Kind
  data P (x :: k) = Q
  data A :: Type where
    B :: forall (a :: A). P a -> A

In order to check the constructor B, we need to have the promoted type A, but in
order to get that promoted type, B must first be checked. To prevent looping, a
TyConPE promotion error is given when tcTyVar checks an ATcTyCon in kind mode.
Any ATcTyCon is a TyCon being defined in the current recursive group (see data
type decl for TcTyThing), and all such TyCons are illegal in kinds.

#11962 proposes checking the head of a data declaration separately from
its constructors. This would allow the example above to pass.



Note [Body kind of a HsForAllTy]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcHsType.hs#L1479>`__

The body of a forall is usually a type, but in principle
there's no reason to prohibit *unlifted* types.
In fact, GHC can itself construct a function with an
unboxed tuple inside a for-all (via CPR analysis; see
typecheck/should_compile/tc170).

Moreover in instance heads we get forall-types with
kind Constraint.

It's tempting to check that the body kind is either * or #. But this is
wrong. For example:

::

  class C a b
  newtype N = Mk Foo deriving (C a)

..

We're doing newtype-deriving for C. But notice how `a` isn't in scope in
the predicate `C a`. So we quantify, yielding `forall a. C a` even though
`C a` has kind `* -> Constraint`. The `forall a. C a` is a bit cheeky, but
convenient. Bottom line: don't check for * or # here.



Note [Body kind of a HsQualTy]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcHsType.hs#L1501>`__

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



Note [Inferring tuple kinds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcHsType.hs#L1528>`__

Give a tuple type (a,b,c), which the parser labels as HsBoxedOrConstraintTuple,
we try to figure out whether it's a tuple of kind * or Constraint.
  Step 1: look at the expected kind
  Step 2: infer argument kinds

If after Step 2 it's not clear from the arguments that it's
Constraint, then it must be *.  Once having decided that we re-check
the arguments to give good error messages in
  e.g.  (Maybe, Maybe)

Note that we will still fail to infer the correct kind in this case:

::

  type T a = ((a,a), D a)
  type family D :: Constraint -> Constraint

..

While kind checking T, we do not yet know the kind of D, so we will default the
kind of T to * -> *. It works if we annotate `a` with kind `Constraint`.



Note [Desugaring types]
~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcHsType.hs#L1548>`__

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



Note [Kind of a type splice]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcHsType.hs#L1580>`__

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

Help functions for type applications
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



Note [Keeping scoped variables in order: Explicit]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcHsType.hs#L1614>`__

When the user writes `forall a b c. blah`, we bring a, b, and c into
scope and then check blah. In the process of checking blah, we might
learn the kinds of a, b, and c, and these kinds might indicate that
b depends on c, and thus that we should reject the user-written type.

One approach to doing this would be to bring each of a, b, and c into
scope, one at a time, creating an implication constraint and
bumping the TcLevel for each one. This would work, because the kind
of, say, b would be untouchable when c is in scope (and the constraint
couldn't float out because c blocks it). However, it leads to terrible
error messages, complaining about skolem escape. While it is indeed
a problem of skolem escape, we can do better.

Instead, our approach is to bring the block of variables into scope
all at once, creating one implication constraint for the lot. The
user-written variables are skolems in the implication constraint. In
TcSimplify.setImplicationStatus, we check to make sure that the ordering
is correct, choosing ImplicationStatus IC_BadTelescope if they aren't.
Then, in TcErrors, we report if there is a bad telescope. This way,
we can report a suggested ordering to the user if there is a problem.



Note [Keeping scoped variables in order: Implicit]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcHsType.hs#L1637>`__

When the user implicitly quantifies over variables (say, in a type
signature), we need to come up with some ordering on these variables.
This is done by bumping the TcLevel, bringing the tyvars into scope,
and then type-checking the thing_inside. The constraints are all
wrapped in an implication, which is then solved. Finally, we can
zonk all the binders and then order them with scopedSort.

It's critical to solve before zonking and ordering in order to uncover
any unifications. You might worry that this eager solving could cause
trouble elsewhere. I don't think it will. Because it will solve only
in an increased TcLevel, it can't unify anything that was mentioned
elsewhere. Additionally, we require that the order of implicitly
quantified variables is manifest by the scope of these variables, so
we're not going to learn more information later that will help order
these variables.



Note [Recipe for checking a signature]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcHsType.hs#L1655>`__

Checking a user-written signature requires several steps:

 1. Generate constraints.
 2. Solve constraints.
 3. Zonk.
 4. Promote tyvars and/or kind-generalize.
 5. Zonk.
 6. Check validity.

There may be some surprises in here:

Step 2 is necessary for two reasons: most signatures also bring
implicitly quantified variables into scope, and solving is necessary
to get these in the right order (see Note [Keeping scoped variables in
order: Implicit]). Additionally, solving is necessary in order to
kind-generalize correctly.

In Step 4, we have to deal with the fact that metatyvars generated
in the type may have a bumped TcLevel, because explicit foralls
raise the TcLevel. To avoid these variables from ever being visible
in the surrounding context, we must obey the following dictum:

::

  Every metavariable in a type must either be
    (A) promoted
    (B) generalized, or
    (C) zapped to Any

..

If a variable is generalized, then it becomes a skolem and no longer
has a proper TcLevel. (I'm ignoring the TcLevel on a skolem here, as
it's not really in play here.) On the other hand, if it is not
generalized (because we're not generalizing the construct -- e.g., pattern
sig -- or because the metavars are constrained -- see kindGeneralizeLocal)
we need to promote to maintain (MetaTvInv) of Note [TcLevel and untouchable type variables]
in TcType.

For more about (C), see Note [Naughty quantification candidates] in TcMType.

After promoting/generalizing, we need to zonk *again* because both
promoting and generalizing fill in metavariables.

To avoid the double-zonk, we do two things:
 1. When we're not generalizing:
    zonkPromoteType and friends zonk and promote at the same time.
    Accordingly, the function does steps 3-5 all at once, preventing
    the need for multiple traversals.

 2. When we are generalizing:
    kindGeneralize does not require a zonked type -- it zonks as it
    gathers free variables. So this way effectively sidesteps step 3.



Note [The initial kind of a type constructor]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcHsType.hs#L1734>`__

kcLHsQTyVars is responsible for getting the initial kind of
a type constructor.

It has two cases:

 * The TyCon has a CUSK.  In that case, find the full, final,
   poly-kinded kind of the TyCon.  It's very like a term-level
   binding where we have a complete type signature for the
   function.

 * It does not have a CUSK.  Find a monomorphic kind, with
   unification variables in it; they will be generalised later.
   It's very like a term-level binding where we do not have
   a type signature (or, more accurately, where we have a
   partial type signature), so we infer the type and generalise.



Note [No polymorphic recursion]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcHsType.hs#L1901>`__

Should this kind-check?
  data T ka (a::ka) b  = MkT (T Type           Int   Bool)
                             (T (Type -> Type) Maybe Bool)

Notice that T is used at two different kinds in its RHS.  No!
This should not kind-check.  Polymorphic recursion is known to
be a tough nut.

Previously, we laboriously (with help from the renamer)
tried to give T the polymoprhic kind
   T :: forall ka -> ka -> kappa -> Type
where kappa is a unification variable, even in the getInitialKinds
phase (which is what kcLHsQTyVars_NonCusk is all about).  But
that is dangerously fragile (see the ticket).

Solution: make kcLHsQTyVars_NonCusk give T a straightforward
monomorphic kind, with no quantification whatsoever. That's why
we use mkAnonTyConBinder for all arguments when figuring out
tc_binders.

But notice that (#16322 comment:3)

* The algorithm successfully kind-checks this declaration:
    data T2 ka (a::ka) = MkT2 (T2 Type a)

  Starting with (getInitialKinds)
    T2 :: (kappa1 :: kappa2 :: *) -> (kappa3 :: kappa4 :: *) -> *
  we get
    kappa4 := kappa1   -- from the (a:ka) kind signature
    kappa1 := Type     -- From application T2 Type

::

  These constraints are soluble so generaliseTcTyCon gives
    T2 :: forall (k::Type) -> k -> *

..

::

  But now the /typechecking/ (aka desugaring, tcTyClDecl) phase
  fails, because the call (T2 Type a) in the RHS is ill-kinded.

..

  We'd really prefer all errors to show up in the kind checking
  phase.

* This algorithm still accepts (in all phases)
     data T3 ka (a::ka) = forall b. MkT3 (T3 Type b)
  although T3 is really polymorphic-recursive too.
  Perhaps we should somehow reject that.



Note [Kind-checking tyvar binders for associated types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcHsType.hs#L1948>`__

When kind-checking the type-variable binders for associated
   data/newtype decls
   family decls
we behave specially for type variables that are already in scope;
that is, bound by the enclosing class decl.  This is done in
kcLHsQTyVarBndrs:
  * The use of tcImplicitQTKBndrs
  * The tcLookupLocal_maybe code in kc_hs_tv

See Note [Associated type tyvar names] in Class and
    Note [TyVar binders for associated decls] in HsDecls

We must do the same for family instance decls, where the in-scope
variables may be bound by the enclosing class instance decl.
Hence the use of tcImplicitQTKBndrs in tcFamTyPatsAndGen.



Note [Kind variable ordering for associated types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcHsType.hs#L1966>`__

What should be the kind of `T` in the following example? (#15591)

::

  class C (a :: Type) where
    type T (x :: f a)

..

As per Note [Ordering of implicit variables] in RnTypes, we want to quantify
the kind variables in left-to-right order of first occurrence in order to
support visible kind application. But we cannot perform this analysis on just
T alone, since its variable `a` actually occurs /before/ `f` if you consider
the fact that `a` was previously bound by the parent class `C`. That is to say,
the kind of `T` should end up being:

::

  T :: forall a f. f a -> Type

..

(It wouldn't necessarily be /wrong/ if the kind ended up being, say,
forall f a. f a -> Type, but that would not be as predictable for users of
visible kind application.)

In contrast, if `T` were redefined to be a top-level type family, like `T2`
below:

::

  type family T2 (x :: f (a :: Type))

..

Then `a` first appears /after/ `f`, so the kind of `T2` should be:

::

  T2 :: forall f a. f a -> Type

..

In order to make this distinction, we need to know (in kcLHsQTyVars) which
type variables have been bound by the parent class (if there is one). With
the class-bound variables in hand, we can ensure that we always quantify
these first.



Note [Levels and generalisation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcHsType.hs#L2276>`__

Consider
  f x = e
with no type signature. We are currently at level i.
We must
  * Push the level to level (i+1)
  * Allocate a fresh alpha[i+1] for the result type
  * Check that e :: alpha[i+1], gathering constraint WC
  * Solve WC as far as possible
  * Zonking the result type alpha[i+1], say to beta[i-1] -> gamma[i]
  * Find the free variables with level > i, in this case gamma[i]
  * Skolemise those free variables and quantify over them, giving
       f :: forall g. beta[i-1] -> g
  * Emit the residiual constraint wrapped in an implication for g,
    thus   forall g. WC

All of this happens for types too.  Consider
  f :: Int -> (forall a. Proxy a -> Int)



Note [Kind generalisation]
~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcHsType.hs#L2296>`__

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



Note [Kinds of quantified type variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcHsType.hs#L2317>`__

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



Note [TyConBinders for the result kind signature of a data type]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcHsType.hs#L2418>`__

Given
  data T (a::*) :: * -> forall k. k -> *
we want to generate the extra TyConBinders for T, so we finally get
  (a::*) (b::*) (k::*) (c::k)
The function etaExpandAlgTyCon generates these extra TyConBinders from
the result kind signature.

We need to take care to give the TyConBinders
  (a) OccNames that are fresh (because the TyConBinders of a TyCon
      must have distinct OccNames

::

  (b) Uniques that are fresh (obviously)

..

For (a) we need to avoid clashes with the tyvars declared by
the user before the "::"; in the above example that is 'a'.
And also see Note [Avoid name clashes for associated data types].

For (b) suppose we have
   data T :: forall k. k -> forall k. k -> *
where the two k's are identical even up to their uniques.  Surprisingly,
this can happen: see #14515.

It's reasonably easy to solve all this; just run down the list with a
substitution; hence the recursive 'go' function.  But it has to be
done.



Note [Avoid name clashes for associated data types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcHsType.hs#L2446>`__

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



Note [Extra-constraint holes in partial type signatures]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcHsType.hs#L2548>`__

Consider
  f :: (_) => a -> a
  f x = ...

* The renamer leaves '_' untouched.

* Then, in tcHsPartialSigType, we make a new hole TcTyVar, in
  tcWildCardBinders.

* TcBinds.chooseInferredQuantifiers fills in that hole TcTyVar
  with the inferred constraints, e.g. (Eq a, Show a)

* TcErrors.mkHoleError finally reports the error.

An annoying difficulty happens if there are more than 62 inferred
constraints. Then we need to fill in the TcTyVar with (say) a 70-tuple.
Where do we find the TyCon?  For good reasons we only have constraint
tuples up to 62 (see Note [How tuples work] in TysWiredIn).  So how
can we make a 70-tuple?  This was the root cause of #14217.

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



Note [Pattern signature binders]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcHsType.hs#L2710>`__

See also Note [Type variables in the type environment] in TcRnTypes.
Consider

::

  data T where
    MkT :: forall a. a -> (a -> Int) -> T

..

::

  f :: T -> ...
  f (MkT x (f :: b -> c)) = <blah>

..

Here
 * The pattern (MkT p1 p2) creates a *skolem* type variable 'a_sk',
   It must be a skolem so that that it retains its identity, and
   TcErrors.getSkolemInfo can thereby find the binding site for the skolem.

 * The type signature pattern (f :: b -> c) makes freshs meta-tyvars
   beta and gamma (TauTvs), and binds "b" :-> beta, "c" :-> gamma in the
   environment

 * Then unification makes beta := a_sk, gamma := Int
   That's why we must make beta and gamma a MetaTv,
   not a SkolemTv, so that it can unify to a_sk (or Int, respectively).

 * Finally, in '<blah>' we have the envt "b" :-> beta, "c" :-> gamma,
   so we return the pairs ("b" :-> beta, "c" :-> gamma) from tcHsPatSigType,

Another example (#13881):
   fl :: forall (l :: [a]). Sing l -> Sing l
   fl (SNil :: Sing (l :: [y])) = SNil
When we reach the pattern signature, 'l' is in scope from the
outer 'forall':
   "a" :-> a_sk :: *
   "l" :-> l_sk :: [a_sk]
We make up a fresh meta-TauTv, y_sig, for 'y', and kind-check
the pattern signature
   Sing (l :: [y])
That unifies y_sig := a_sk.  We return from tcHsPatSigType with
the pair ("y" :-> y_sig).

For RULE binders, though, things are a bit different (yuk).
  RULE "foo" forall (x::a) (y::[a]).  f x y = ...
Here this really is the binding site of the type variable so we'd like
to use a skolem, so that we get a complaint if we unify two of them
together.  Hence the new_tv function in tcHsPatSigType.

