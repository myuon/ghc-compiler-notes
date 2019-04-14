`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcMType.hs>`_

compiler/typecheck/TcMType.hs
=============================


Note [ExpType]
~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcMType.hs#L367>`__

An ExpType is used as the "expected type" when type-checking an expression.
An ExpType can hold a "hole" that can be filled in by the type-checker.
This allows us to have one tcExpr that works in both checking mode and
synthesis mode (that is, bidirectional type-checking). Previously, this
was achieved by using ordinary unification variables, but we don't need
or want that generality. (For example, #11397 was caused by doing the
wrong thing with unification variables.) Instead, we observe that these
holes should

1. never be nested
2. never appear as the type of a variable
3. be used linearly (never be duplicated)

By defining ExpType, separately from Type, we can achieve goals 1 and 2
statically.

See also [wiki:typechecking]



Note [TcLevel of ExpType]
~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcMType.hs#L388>`__

Consider

::

  data G a where
    MkG :: G Bool

..

::

  foo MkG = True

..

This is a classic untouchable-variable / ambiguous GADT return type
scenario. But, with ExpTypes, we'll be inferring the type of the RHS.
And, because there is only one branch of the case, we won't trigger
Note [Case branches must never infer a non-tau type] of TcMatches.
We thus must track a TcLevel in an Inferring ExpType. If we try to
fill the ExpType and find that the TcLevels don't work out, we
fill the ExpType with a tau-tv at the low TcLevel, hopefully to
be worked out later by some means. This is triggered in
test gadt/gadt-escape1.


actual data definition is in TcType



Note [Skolemising type variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcMType.hs#L596>`__

The tcInstSkolTyVars family of functions instantiate a list of TyVars
to fresh skolem TcTyVars. Important notes:

a) Level allocation. We generally skolemise /before/ calling
   pushLevelAndCaptureConstraints.  So we want their level to the level
   of the soon-to-be-created implication, which has a level ONE HIGHER
   than the current level.  Hence the pushTcLevel.  It feels like a
   slight hack.

b) The [TyVar] should be ordered (kind vars first)
   See Note [Kind substitution when instantiating]

c) It's a complete freshening operation: the skolems have a fresh
   unique, and a location from the monad

d) The resulting skolems are
        non-overlappable for tcInstSkolTyVars,
   but overlappable for tcInstSuperSkolTyVars
   See TcDerivInfer Note [Overlap and deriving] for an example
   of where this matters.



Note [Kind substitution when instantiating]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcMType.hs#L619>`__

When we instantiate a bunch of kind and type variables, first we
expect them to be topologically sorted.
Then we have to instantiate the kind variables, build a substitution
from old variables to the new variables, then instantiate the type
variables substituting the original kind.

Exemple: If we want to instantiate
  [(k1 :: *), (k2 :: *), (a :: k1 -> k2), (b :: k1)]
we want
  [(?k1 :: *), (?k2 :: *), (?a :: ?k1 -> ?k2), (?b :: ?k1)]
instead of the buggous
  [(?k1 :: *), (?k2 :: *), (?a :: k1 -> k2), (?b :: k1)]



Note [TyVarTv]
~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcMType.hs#L643>`__

A TyVarTv can unify with type *variables* only, including other TyVarTvs and
skolems. Sometimes, they can unify with type variables that the user would
rather keep distinct; see #11203 for an example.  So, any client of this
function needs to either allow the TyVarTvs to unify with each other or check
that they don't (say, with a call to findDubTyVarTvs).

Before #15050 this (under the name SigTv) was used for ScopedTypeVariables in
patterns, to make sure these type variables only refer to other type variables,
but this restriction was dropped, and ScopedTypeVariables can now refer to full
types (GHC Proposal 29).

The remaining uses of newTyVarTyVars are
* In kind signatures, see
  TcTyClsDecls Note [Inferring kinds for type declarations]
           and Note [Kind checking for GADTs]
* In partial type signatures, see Note [Quantified variables in partial type signatures]



Note [Name of an instantiated type variable]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcMType.hs#L678>`__

At the moment we give a unification variable a System Name, which
influences the way it is tidied; see TypeRep.tidyTyVarBndr.



Note [Unification variables need fresh Names]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcMType.hs#L683>`__

Whenever we allocate a unification variable (MetaTyVar) we give
it a fresh name.   #16221 is a very tricky case that illustrates
why this is important:

::

   data SameKind :: k -> k -> *
   data T0 a = forall k2 (b :: k2). MkT0 (SameKind a b) !Int

..

When kind-checking T0, we give (a :: kappa1). Then, in kcConDecl
we allocate a unification variable kappa2 for k2, and then we
end up unifying kappa1 := kappa2 (because of the (SameKind a b).

Now we generalise over kappa2; but if kappa2's Name is k2,
we'll end up giving T0 the kind forall k2. k2 -> *.  Nothing
directly wrong with that but when we typecheck the data constrautor
we end up giving it the type
  MkT0 :: forall k1 (a :: k1) k2 (b :: k2).
          SameKind @k2 a b -> Int -> T0 @{k2} a
which is bogus.  The result type should be T0 @{k1} a.

And there no reason /not/ to clone the Name when making a
unification variable.  So that's what we do.



Note [Level check when unifying]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcMType.hs#L900>`__

When unifying
     alpha:lvl := ty
we expect that the TcLevel of 'ty' will be <= lvl.
However, during unflatting we do
     fuv:l := ty:(l+1)
which is usually wrong; hence the check isFmmvTyVar in level_check_ok.
See Note [TcLevel assignment] in TcType.

% Generating fresh variables for pattern match check



Note [Never need to instantiate coercion variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcMType.hs#L923>`__

With coercion variables sloshing around in types, it might seem that we
sometimes need to instantiate coercion variables. This would be problematic,
because coercion variables inhabit unboxed equality (~#), and the constraint
solver thinks in terms only of boxed equality (~). The solution is that
we never need to instantiate coercion variables in the first place.

The tyvars that we need to instantiate come from the types of functions,
data constructors, and patterns. These will never be quantified over
coercion variables, except for the special case of the promoted Eq#. But,
that can't ever appear in user code, so we're safe!



Note [Dependent type variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcMType.hs#L1024>`__

In Haskell type inference we quantify over type variables; but we only
quantify over /kind/ variables when -XPolyKinds is on.  Without -XPolyKinds
we default the kind variables to *.

So, to support this defaulting, and only for that reason, when
collecting the free vars of a type, prior to quantifying, we must keep
the type and kind variables separate.

But what does that mean in a system where kind variables /are/ type
variables? It's a fairly arbitrary distinction based on how the
variables appear:

  - "Kind variables" appear in the kind of some other free variable

::

     These are the ones we default to * if -XPolyKinds is off

..

  - "Type variables" are all free vars that are not kind variables

E.g.  In the type    T k (a::k)
      'k' is a kind variable, because it occurs in the kind of 'a',
          even though it also appears at "top level" of the type
      'a' is a type variable, because it doesn't

We gather these variables using a CandidatesQTvs record:
  DV { dv_kvs: Variables free in the kind of a free type variable
               or of a forall-bound type variable
     , dv_tvs: Variables sytactically free in the type }

So:  dv_kvs            are the kind variables of the type
     (dv_tvs - dv_kvs) are the type variable of the type

Note that

* A variable can occur in both.
      T k (x::k)    The first occurrence of k makes it
                    show up in dv_tvs, the second in dv_kvs

* We include any coercion variables in the "dependent",
  "kind-variable" set because we never quantify over them.

* The "kind variables" might depend on each other; e.g
     (k1 :: k2), (k2 :: *)
  The "type variables" do not depend on each other; if
  one did, it'd be classified as a kind variable!



Note [CandidatesQTvs determinism and order]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcMType.hs#L1071>`__

* Determinism: when we quantify over type variables we decide the
  order in which they appear in the final type. Because the order of
  type variables in the type can end up in the interface file and
  affects some optimizations like worker-wrapper, we want this order to
  be deterministic.

::

  To achieve that we use deterministic sets of variables that can be
  converted to lists in a deterministic order. For more information
  about deterministic sets see Note [Deterministic UniqFM] in UniqDFM.

..

* Order: as well as being deterministic, we use an
  accumulating-parameter style for candidateQTyVarsOfType so that we
  add variables one at a time, left to right.  That means we tend to
  produce the variables in left-to-right order.  This is just to make
  it bit more predictable for the programmer.



Note [Naughty quantification candidates]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcMType.hs#L1089>`__

Consider (#14880, dependent/should_compile/T14880-2), suppose
we are trying to generalise this type:

::

  forall arg. ... (alpha[tau]:arg) ...

..

We have a metavariable alpha whose kind mentions a skolem variable
boudn inside the very type we are generalising.
This can arise while type-checking a user-written type signature
(see the test case for the full code).

We cannot generalise over alpha!  That would produce a type like
  forall {a :: arg}. forall arg. ...blah...
The fact that alpha's kind mentions arg renders it completely
ineligible for generaliation.

However, we are not going to learn any new constraints on alpha,
because its kind isn't even in scope in the outer context.  So alpha
is entirely unconstrained.

What then should we do with alpha?  During generalization, every
metavariable is either (A) promoted, (B) generalized, or (C) zapped
(according again to Note [Recipe for checking a signature] in
TcHsType).

 * We can't generalise it.
 * We can't promote it, because its kind prevents that
 * We can't simply leave it be, because this type is about to
   go into the typing environment (as the type of some let-bound
   variable, say), and then chaos erupts when we try to instantiate.

So, we zap it, eagerly, to Any. We don't have to do this eager zapping
in terms (say, in `length []`) because terms are never re-examined before
the final zonk (which zaps any lingering metavariables to Any).

We do this eager zapping in candidateQTyVars, which always precedes
generalisation, because at that moment we have a clear picture of
what skolems are in scope.



Note [Order of accumulation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcMType.hs#L1326>`__

You might be tempted (like I was) to use unitDVarSet and mappend
rather than extendDVarSet.  However, the union algorithm for
deterministic sets depends on (roughly) the size of the sets. The
elements from the smaller set end up to the right of the elements from
the larger one. When sets are equal, the left-hand argument to
`mappend` goes to the right of the right-hand argument.

In our case, if we use unitDVarSet and mappend, we learn that the free
variables of (a -> b -> c -> d) are [b, a, c, d], and we then quantify
over them in that order. (The a comes after the b because we union the
singleton sets as ({a} `mappend` {b}), producing {b, a}. Thereafter,
the size criterion works to our advantage.) This is just annoying to
users, so I use `extendDVarSet`, which unambiguously puts the new
element to the right.

Note that the unitDVarSet/mappend implementation would not be wrong
against any specification -- just suboptimal and confounding to users.



Note [Defaulting with -XNoPolyKinds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcMType.hs#L1632>`__

Consider

::

  data Compose f g a = Mk (f (g a))

..

We infer

::

  Compose :: forall k1 k2. (k2 -> *) -> (k1 -> k2) -> k1 -> *
  Mk :: forall k1 k2 (f :: k2 -> *) (g :: k1 -> k2) (a :: k1).
        f (g a) -> Compose k1 k2 f g a

..

Now, in another module, we have -XNoPolyKinds -XDataKinds in effect.
What does 'Mk mean? Pre GHC-8.0 with -XNoPolyKinds,
we just defaulted all kind variables to *. But that's no good here,
because the kind variables in 'Mk aren't of kind *, so defaulting to *
is ill-kinded.

After some debate on #11334, we decided to issue an error in this case.
The code is in defaultKindVar.



Note [What is a meta variable?]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcMType.hs#L1653>`__

A "meta type-variable", also know as a "unification variable" is a placeholder
introduced by the typechecker for an as-yet-unknown monotype.

For example, when we see a call `reverse (f xs)`, we know that we calling
    reverse :: forall a. [a] -> [a]
So we know that the argument `f xs` must be a "list of something". But what is
the "something"? We don't know until we explore the `f xs` a bit more. So we set
out what we do know at the call of `reverse` by instantiate its type with a fresh
meta tyvar, `alpha` say. So now the type of the argument `f xs`, and of the
result, is `[alpha]`. The unification variable `alpha` stands for the
as-yet-unknown type of the elements of the list.

As type inference progresses we may learn more about `alpha`. For example, suppose
`f` has the type
    f :: forall b. b -> [Maybe b]
Then we instantiate `f`'s type with another fresh unification variable, say
`beta`; and equate `f`'s result type with reverse's argument type, thus
`[alpha] ~ [Maybe beta]`.

Now we can solve this equality to learn that `alpha ~ Maybe beta`, so we've
refined our knowledge about `alpha`. And so on.

If you found this Note useful, you may also want to have a look at
Section 5 of "Practical type inference for higher rank types" (Peyton Jones,
Vytiniotis, Weirich and Shields. J. Functional Programming. 2011).



Note [What is zonking?]
~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcMType.hs#L1681>`__

GHC relies heavily on mutability in the typechecker for efficient operation.
For this reason, throughout much of the type checking process meta type
variables (the MetaTv constructor of TcTyVarDetails) are represented by mutable
variables (known as TcRefs).

Zonking is the process of ripping out these mutable variables and replacing them
with a real Type. This involves traversing the entire type expression, but the
interesting part of replacing the mutable variables occurs in zonkTyVarOcc.

There are two ways to zonk a Type:

 * zonkTcTypeToType, which is intended to be used at the end of type-checking
   for the final zonk. It has to deal with unfilled metavars, either by filling
   it with a value like Any or failing (determined by the UnboundTyVarZonker
   used).

 * zonkTcType, which will happily ignore unfilled metavars. This is the
   appropriate function to use while in the middle of type-checking.



Note [Zonking to Skolem]
~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcMType.hs#L1702>`__

We used to zonk quantified type variables to regular TyVars.  However, this
leads to problems.  Consider this program from the regression test suite:

::

  eval :: Int -> String -> String -> String
  eval 0 root actual = evalRHS 0 root actual

..

::

  evalRHS :: Int -> a
  evalRHS 0 root actual = eval 0 root actual

..

It leads to the deferral of an equality (wrapped in an implication constraint)

::

  forall a. () => ((String -> String -> String) ~ a)

..

which is propagated up to the toplevel (see TcSimplify.tcSimplifyInferCheck).
In the meantime `a' is zonked and quantified to form `evalRHS's signature.
This has the *side effect* of also zonking the `a' in the deferred equality
(which at this point is being handed around wrapped in an implication
constraint).

Finally, the equality (with the zonked `a') will be handed back to the
simplifier by TcRnDriver.tcRnSrcDecls calling TcSimplify.tcSimplifyTop.
If we zonk `a' with a regular type variable, we will have this regular type
variable now floating around in the simplifier, which in many places assumes to
only see proper TcTyVars.

We can avoid this problem by zonking with a skolem.  The skolem is rigid
(which we require for a quantified variable), but is still a TcTyVar that the
simplifier knows how to deal with.



Note [Silly Type Synonyms]
~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcMType.hs#L1733>`__

Consider this:
        type C u a = u  -- Note 'a' unused

::

        foo :: (forall a. C u a -> C u a) -> u
        foo x = ...

..

::

        bar :: Num u => u
        bar = foo (\t -> t + t)

..

* From the (\t -> t+t) we get type  {Num d} =>  d -> d
  where d is fresh.

* Now unify with type of foo's arg, and we get:
        {Num (C d a)} =>  C d a -> C d a
  where a is fresh.

* Now abstract over the 'a', but float out the Num (C d a) constraint
  because it does not 'really' mention a.  (see exactTyVarsOfType)
  The arg to foo becomes
        \/\a -> \t -> t+t

* So we get a dict binding for Num (C d a), which is zonked to give
        a = ()
  [Note Sept 04: now that we are zonking quantified type variables
  on construction, the 'a' will be frozen as a regular tyvar on
  quantification, so the floated dict will still have type (C d a).
  Which renders this whole note moot; happily!]

* Then the \/\a abstraction has a zonked 'a' in it.

All very silly.   I think its harmless to ignore the problem.  We'll end up with
a \/\a in the final result but all the occurrences of a will be zonked to ()



Note [zonkCt behaviour]
~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcMType.hs#L1882>`__

zonkCt tries to maintain the canonical form of a Ct.  For example,
  - a CDictCan should stay a CDictCan;
  - a CTyEqCan should stay a CTyEqCan (if the LHS stays as a variable.).
  - a CHoleCan should stay a CHoleCan
  - a CIrredCan should stay a CIrredCan with its cc_insol flag intact

Why?, for example:
- For CDictCan, the @TcSimplify.expandSuperClasses@ step, which runs after the
  simple wanted and plugin loop, looks for @CDictCan@s. If a plugin is in use,
  constraints are zonked before being passed to the plugin. This means if we
  don't preserve a canonical form, @expandSuperClasses@ fails to expand
  superclasses. This is what happened in #11525.

- For CHoleCan, once we forget that it's a hole, we can never recover that info.

- For CIrredCan we want to see if a constraint is insoluble with insolubleWC

NB: we do not expect to see any CFunEqCans, because zonkCt is only
called on unflattened constraints.

NB: Constraints are always re-flattened etc by the canonicaliser in
@TcCanonical@ even if they come in as CDictCan. Only canonical constraints that
are actually in the inert set carry all the guarantees. So it is okay if zonkCt
creates e.g. a CDictCan where the cc_tyars are /not/ function free.



Note [Sharing in zonking]
~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcMType.hs#L2063>`__

Suppose we have
   alpha :-> beta :-> gamma :-> ty
where the ":->" means that the unification variable has been
filled in with Indirect. Then when zonking alpha, it'd be nice
to short-circuit beta too, so we end up with
   alpha :-> zty
   beta  :-> zty
   gamma :-> zty
where zty is the zonked version of ty.  That way, if we come across
beta later, we'll have less work to do.  (And indeed the same for
alpha.)

This is easily achieved: just overwrite (Indirect ty) with (Indirect
zty).  Non-systematic perf comparisons suggest that this is a modest
win.

But c.f Note [Sharing when zonking to Type] in TcHsSyn.

%************************************************************************
%*                                                                      *
                 Tidying
*                                                                      *
************************************************************************

