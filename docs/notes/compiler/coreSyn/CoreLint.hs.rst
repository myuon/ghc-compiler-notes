`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/coreSyn/CoreLint.hs>`_

Note [GHC Formalism]
~~~~~~~~~~~~~~~~~~~~
This file implements the type-checking algorithm for System FC, the "official"
name of the Core language. Type safety of FC is heart of the claim that
executables produced by GHC do not have segmentation faults. Thus, it is
useful to be able to reason about System FC independently of reading the code.
To this purpose, there is a document core-spec.pdf built in docs/core-spec that
contains a formalism of the types and functions dealt with here. If you change
just about anything in this file or you change other types/functions throughout
the Core language (all signposted to this note), you should update that
formalism. See docs/core-spec/README for more info about how to do so.



Note [check vs lint]
~~~~~~~~~~~~~~~~~~~~
This file implements both a type checking algorithm and also general sanity
checking. For example, the "sanity checking" checks for TyConApp on the left
of an AppTy, which should never happen. These sanity checks don't really
affect any notion of type soundness. Yet, it is convenient to do the sanity
checks at the same time as the type checks. So, we use the following naming
convention:

- Functions that begin with 'lint'... are involved in type checking. These
  functions might also do some sanity checking.

- Functions that begin with 'check'... are *not* involved in type checking.
  They exist only for sanity checking.

Issues surrounding variable naming, shadowing, and such are considered *not*
to be part of type checking, as the formalism omits these details.

Summary of checks
~~~~~~~~~~~~~~~~~
Checks that a set of core bindings is well-formed.  The PprStyle and String
just control what we print in the event of an error.  The Bool value
indicates whether we have done any specialisation yet (in which case we do
some extra checks).

We check for
        (a) type errors
        (b) Out-of-scope type variables
        (c) Out-of-scope local variables
        (d) Ill-kinded types
        (e) Incorrect unsafe coercions

If we have done specialisation the we check that there are
        (a) No top-level bindings of primitive (unboxed type)

Outstanding issues:

    -- Things are *not* OK if:
    --
    --  * Unsaturated type app before specialisation has been done;
    --
    --  * Oversaturated type app after specialisation (eta reduction
    --   may well be happening...);




Note [Linting function types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
As described in Note [Representation of function types], all saturated
applications of funTyCon are represented with the FunTy constructor. We check
this invariant in lintType.



Note [Linting type lets]
~~~~~~~~~~~~~~~~~~~~~~~~
In the desugarer, it's very very convenient to be able to say (in effect)
        let a = Type Int in <body>
That is, use a type let.   See Note [Type let] in CoreSyn.

However, when linting <body> we need to remember that a=Int, else we might
reject a correct program.  So we carry a type substitution (in this example
[a -> Int]) and apply this substitution before comparing types.  The functin
        lintInTy :: Type -> LintM (Type, Kind)
returns a substituted type.

When we encounter a binder (like x::a) we must apply the substitution
to the type of the binding variable.  lintBinders does this.

For Ids, the type-substituted Id is added to the in_scope set (which
itself is part of the TCvSubst we are carrying down), and when we
find an occurrence of an Id, we fetch it from the in-scope set.



Note [Bad unsafe coercion]
~~~~~~~~~~~~~~~~~~~~~~~~~~
For discussion see https://ghc.haskell.org/trac/ghc/wiki/BadUnsafeCoercions
Linter introduces additional rules that checks improper coercion between
different types, called bad coercions. Following coercions are forbidden:

  (a) coercions between boxed and unboxed values;
  (b) coercions between unlifted values of the different sizes, here
      active size is checked, i.e. size of the actual value but not
      the space allocated for value;
  (c) coercions between floating and integral boxed values, this check
      is not yet supported for unboxed tuples, as no semantics were
      specified for that;
  (d) coercions from / to vector type
  (e) If types are unboxed tuples then tuple (# A_1,..,A_n #) can be
      coerced to (# B_1,..,B_m #) if n=m and for each pair A_i, B_i rules
      (a-e) holds.



Note [Join points]
~~~~~~~~~~~~~~~~~~
We check the rules listed in Note [Invariants on join points] in CoreSyn. The
only one that causes any difficulty is the first: All occurrences must be tail
calls. To this end, along with the in-scope set, we remember in le_joins the
subset of in-scope Ids that are valid join ids. For example:

  join j x = ... in
  case e of
    A -> jump j y -- good
    B -> case (jump j z) of -- BAD
           C -> join h = jump j w in ... -- good
           D -> let x = jump j v in ... -- BAD

A join point remains valid in case branches, so when checking the A
branch, j is still valid. When we check the scrutinee of the inner
case, however, we set le_joins to empty, and catch the
error. Similarly, join points can occur free in RHSes of other join
points but not the RHSes of value bindings (thunks and functions).



Note [Linting Unfoldings from Interfaces]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We use this to check all top-level unfoldings that come in from interfaces
(it is very painful to catch errors otherwise).

We do not need to call lintUnfolding on unfoldings that are nested within
top-level unfoldings; they are linted when we lint the top-level unfolding;
hence the `TopLevelFlag` on `tcPragExpr` in TcIface.



Note [Checking for INLINE loop breakers]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It's very suspicious if a strong loop breaker is marked INLINE.

However, the desugarer generates instance methods with INLINE pragmas
that form a mutually recursive group.  Only after a round of
simplification are they unravelled.  So we suppress the test for
the desugarer.



Note [No alternatives lint check]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Case expressions with no alternatives are odd beasts, and it would seem
like they would worth be looking at in the linter (cf #10180). We
used to check two things:

* exprIsHNF is false: it would *seem* to be terribly wrong if
  the scrutinee was already in head normal form.

* exprIsBottom is true: we should be able to see why GHC believes the
  scrutinee is diverging for sure.

It was already known that the second test was not entirely reliable.
Unfortunately (#13990), the first test turned out not to be reliable
either. Getting the checks right turns out to be somewhat complicated.

For example, suppose we have (comment 8)

  data T a where
    TInt :: T Int

  absurdTBool :: T Bool -> a
  absurdTBool v = case v of

  data Foo = Foo !(T Bool)

  absurdFoo :: Foo -> a
  absurdFoo (Foo x) = absurdTBool x

GHC initially accepts the empty case because of the GADT conditions. But then
we inline absurdTBool, getting

  absurdFoo (Foo x) = case x of

x is in normal form (because the Foo constructor is strict) but the
case is empty. To avoid this problem, GHC would have to recognize
that matching on Foo x is already absurd, which is not so easy.

More generally, we don't really know all the ways that GHC can
lose track of why an expression is bottom, so we shouldn't make too
much fuss when that happens.




Note [Beta redexes]
~~~~~~~~~~~~~~~~~~~
Consider:

  join j @x y z = ... in
  (\@x y z -> jump j @x y z) @t e1 e2

This is clearly ill-typed, since the jump is inside both an application and a
lambda, either of which is enough to disqualify it as a tail call (see Note
[Invariants on join points] in CoreSyn). However, strictly from a
lambda-calculus perspective, the term doesn't go wrong---after the two beta
reductions, the jump *is* a tail call and everything is fine.

Why would we want to allow this when we have let? One reason is that a compound
beta redex (that is, one with more than one argument) has different scoping
rules: naively reducing the above example using lets will capture any free
occurrence of y in e2. More fundamentally, type lets are tricky; many passes,
such as Float Out, tacitly assume that the incoming program's type lets have
all been dealt with by the simplifier. Thus we don't want to let-bind any types
in, say, CoreSubst.simpleOptPgm, which in some circumstances can run immediately
before Float Out.

All that said, currently CoreSubst.simpleOptPgm is the only thing using this
loophole, doing so to avoid re-traversing large functions (beta-reducing a type
lambda without introducing a type let requires a substitution). TODO: Improve
simpleOptPgm so that we can forget all this ever happened.



Note [Stupid type synonyms]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider (#14939)
   type Alg cls ob = ob
   f :: forall (cls :: * -> Constraint) (b :: Alg cls *). b

Here 'cls' appears free in b's kind, which would usually be illegal
(because in (forall a. ty), ty's kind should not mention 'a'). But
#in this case (Alg cls *) = *, so all is well.  Currently we allow
this, and make Lint expand synonyms where necessary to make it so.

c.f. TcUnify.occCheckExpand and CoreUtils.coreAltsType which deal
with the same problem. A single systematic solution eludes me.
---------------


Note [Linting rules]
~~~~~~~~~~~~~~~~~~~~~~~
It's very bad if simplifying a rule means that one of the template
variables (ru_bndrs) that /is/ mentioned on the RHS becomes
not-mentioned in the LHS (ru_args).  How can that happen?  Well, in
#10602, SpecConstr stupidly constructed a rule like

  forall x,c1,c2.
     f (x |> c1 |> c2) = ....

But simplExpr collapses those coercions into one.  (Indeed in
#10602, it collapsed to the identity and was removed altogether.)

We don't have a great story for what to do here, but at least
this check will nail it.

NB (#11643): it's possible that a variable listed in the
binders becomes not-mentioned on both LHS and RHS.  Here's a silly
example:
   RULE forall x y. f (g x y) = g (x+1) (y-1)
And suppose worker/wrapper decides that 'x' is Absent.  Then
we'll end up with
   RULE forall x y. f ($gw y) = $gw (x+1)
This seems sufficiently obscure that there isn't enough payoff to
try to trim the forall'd binder list.



Note [Rules for join points]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A join point cannot be partially applied. However, the left-hand side of a rule
for a join point is effectively a *pattern*, not a piece of code, so there's an
argument to be made for allowing a situation like this:

  join $sj :: Int -> Int -> String
       $sj n m = ...
       j :: forall a. Eq a => a -> a -> String
       {-# RULES "SPEC j" jump j @ Int $dEq = jump $sj #-}
       j @a $dEq x y = ...

Applying this rule can't turn a well-typed program into an ill-typed one, so
conceivably we could allow it. But we can always eta-expand such an
"undersaturated" rule (see 'CoreArity.etaExpandToJoinPointRule'), and in fact
the simplifier would have to in order to deal with the RHS. So we take a
conservative view and don't allow undersaturated rules for join points. See
Note [Rules and join points] in OccurAnal for further discussion.


Note [Checking for global Ids]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Before CoreTidy, all locally-bound Ids must be LocalIds, even
top-level ones. See Note [Exported LocalIds] and #9857.



Note [Checking StaticPtrs]
~~~~~~~~~~~~~~~~~~~~~~~~~~
See Note [Grand plan for static forms] in StaticPtrTable for an overview.

Every occurrence of the function 'makeStatic' should be moved to the
top level by the FloatOut pass.  It's vital that we don't have nested
'makeStatic' occurrences after CorePrep, because we populate the Static
Pointer Table from the top-level bindings. See SimplCore Note [Grand
plan for static forms].

The linter checks that no occurrence is left behind, nested within an
expression. The check is enabled only after the FloatOut, CorePrep,
and CoreTidy passes and only if the module uses the StaticPointers
language extension. Checking more often doesn't help since the condition
doesn't hold until after the first FloatOut pass.



Note [Type substitution]
~~~~~~~~~~~~~~~~~~~~~~~~
Why do we need a type substitution?  Consider
        /\(a:*). \(x:a). /\(a:*). id a x
This is ill typed, because (renaming variables) it is really
        /\(a:*). \(x:a). /\(b:*). id b x
Hence, when checking an application, we can't naively compare x's type
(at its binding site) with its expected type (at a use site).  So we
rename type binders as we go, maintaining a substitution.

The same substitution also supports let-type, current expressed as
        (/\(a:*). body) ty
Here we substitute 'ty' for 'a' in 'body', on the fly.



Note [Linting type synonym applications]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When linting a type-synonym, or type-family, application
  S ty1 .. tyn
we behave as follows (#15057, #T15664):

* If lf_report_unsat_syns = True, and S has arity < n,
  complain about an unsaturated type synonym or type family

* Switch off lf_report_unsat_syns, and lint ty1 .. tyn.

  Reason: catch out of scope variables or other ill-kinded gubbins,
  even if S discards that argument entirely. E.g. (#15012):
     type FakeOut a = Int
     type family TF a
     type instance TF Int = FakeOut a
  Here 'a' is out of scope; but if we expand FakeOut, we conceal
  that out-of-scope error.

  Reason for switching off lf_report_unsat_syns: with
  LiberalTypeSynonyms, GHC allows unsaturated synonyms provided they
  are saturated when the type is expanded. Example
     type T f = f Int
     type S a = a -> a
     type Z = T S
  In Z's RHS, S appears unsaturated, but it is saturated when T is expanded.

* If lf_report_unsat_syns is on, expand the synonym application and
  lint the result.  Reason: want to check that synonyms are saturated
  when the type is expanded.

