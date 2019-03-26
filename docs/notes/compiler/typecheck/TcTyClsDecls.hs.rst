`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcTyClsDecls.hs>`_

compiler/typecheck/TcTyClsDecls.hs
==================================


Note [Grouping of type and class declarations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcTyClsDecls.hs#L88>`__

tcTyAndClassDecls is called on a list of `TyClGroup`s. Each group is a strongly
connected component of mutually dependent types and classes. We kind check and
type check each group separately to enhance kind polymorphism. Take the
following example:

::

  type Id a = a
  data X = X (Id Int)

If we were to kind check the two declarations together, we would give Id the
kind * -> *, since we apply it to an Int in the definition of X. But we can do
better than that, since Id really is kind polymorphic, and should get kind
forall (k::*). k -> k. Since it does not depend on anything else, it can be
kind-checked by itself, hence getting the most general kind. We then kind check
X, which works fine because we then know the polymorphic kind of Id, and simply
instantiate k to *.



Note [Check role annotations in a second pass]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcTyClsDecls.hs#L106>`__

Role inference potentially depends on the types of all of the datacons declared
in a mutually recursive group. The validity of a role annotation, in turn,
depends on the result of role inference. Because the types of datacons might
be ill-formed (see #7175 and Note [Checking GADT return types]) we must check
*all* the tycons in a group for validity before checking *any* of the roles.
Thus, we take two passes over the resulting tycons, first checking for general
validity and then checking for valid role annotations.



Note [Kind checking for type and class decls]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcTyClsDecls.hs#L262>`__

Kind checking is done thus:

   1. Make up a kind variable for each parameter of the declarations,
      and extend the kind environment (which is in the TcLclEnv)

   2. Kind check the declarations

We need to kind check all types in the mutually recursive group
before we know the kind of the type variables.  For example:

::

  class C a where
     op :: D b => a -> b -> b

::

  class D c where
     bop :: (Monad c) => ...

Here, the kind of the locally-polymorphic type variable "b"
depends on *all the uses of class D*.  For example, the use of
Monad c in bop's type signature means that D must have kind Type->Type.

Note: we don't treat type synonyms specially (we used to, in the past);
in particular, even if we have a type synonym cycle, we still kind check
it normally, and test for cycles later (checkSynCycles).  The reason
we can get away with this is because we have more systematic TYPE r
inference, which means that we can do unification between kinds that
aren't lifted (this historically was not true.)

The downside of not directly reading off the kinds off the RHS of
type synonyms in topological order is that we don't transparently
support making synonyms of types with higher-rank kinds.  But
you can always specify a CUSK directly to make this work out.
See tc269 for an example.



Note [Skip decls with CUSKs in kcLTyClDecl]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcTyClsDecls.hs#L297>`__

Consider

::

    data T (a :: *) = MkT (S a)   -- Has CUSK
    data S a = MkS (T Int) (S a)  -- No CUSK

Via getInitialKinds we get
  T :: * -> *
  S :: kappa -> *

Then we call kcTyClDecl on each decl in the group, to constrain the
kind unification variables.  BUT we /skip/ the RHS of any decl with
a CUSK.  Here we skip the RHS of T, so we eventually get
  S :: forall k. k -> *

This gets us more polymorphism than we would otherwise get, similar
(but implemented strangely differently from) the treatment of type
signatures in value declarations.

Open type families
~~~~~~~~~~~~~~~~~~
This treatment of type synonyms only applies to Haskell 98-style synonyms.
General type functions can be recursive, and hence, appear in `alg_decls'.

The kind of an open type family is solely determinded by its kind signature;
hence, only kind signatures participate in the construction of the initial
kind environment (as constructed by `getInitialKind'). In fact, we ignore
instances of families altogether in the following. However, we need to include
the kinds of *associated* families into the construction of the initial kind
environment. (This is handled by `allDecls').

See also Note [Kind checking recursive type and class declarations]



Note [How TcTyCons work]
~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcTyClsDecls.hs#L331>`__

TcTyCons are used for two distinct purposes

1.  When recovering from a type error in a type declaration,
    we want to put the erroneous TyCon in the environment in a
    way that won't lead to more errors.  We use a TcTyCon for this;
    see makeRecoveryTyCon.

2.  When checking a type/class declaration (in module TcTyClsDecls), we come
    upon knowledge of the eventual tycon in bits and pieces.

::

      S1) First, we use getInitialKinds to look over the user-provided
          kind signature of a tycon (including, for example, the number
          of parameters written to the tycon) to get an initial shape of
          the tycon's kind.  We record that shape in a TcTyCon.

::

          For CUSK tycons, the TcTyCon has the final, generalised kind.
          For non-CUSK tycons, the TcTyCon has as its tyConBinders only
          the explicit arguments given -- no kind variables, etc.

::

      S2) Then, using these initial kinds, we kind-check the body of the
          tycon (class methods, data constructors, etc.), filling in the
          metavariables in the tycon's initial kind.

::

      S3) We then generalize to get the (non-CUSK) tycon's final, fixed
          kind. Finally, once this has happened for all tycons in a
          mutually recursive group, we can desugar the lot.

::

    For convenience, we store partially-known tycons in TcTyCons, which
    might store meta-variables. These TcTyCons are stored in the local
    environment in TcTyClsDecls, until the real full TyCons can be created
    during desugaring. A desugared program should never have a TcTyCon.

3.  In a TcTyCon, everything is zonked after the kind-checking pass (S2).

4.  tyConScopedTyVars.  A challenging piece in all of this is that we
    end up taking three separate passes over every declaration:
      - one in getInitialKind (this pass look only at the head, not the body)
      - one in kcTyClDecls (to kind-check the body)
      - a final one in tcTyClDecls (to desugar)

::

    In the latter two passes, we need to connect the user-written type
    variables in an LHsQTyVars with the variables in the tycon's
    inferred kind. Because the tycon might not have a CUSK, this
    matching up is, in general, quite hard to do.  (Look through the
    git history between Dec 2015 and Apr 2016 for
    TcHsType.splitTelescopeTvs!)

::

    Instead of trying, we just store the list of type variables to
    bring into scope, in the tyConScopedTyVars field of the TcTyCon.
    These tyvars are brought into scope in TcHsType.bindTyClTyVars.

::

    In a TcTyCon, why is tyConScopedTyVars :: [(Name,TcTyVar)] rather
    than just [TcTyVar]?  Consider these mutually-recursive decls
       data T (a :: k1) b = MkT (S a b)
       data S (c :: k2) d = MkS (T c d)
    We start with k1 bound to kappa1, and k2 to kappa2; so initially
    in the (Name,TcTyVar) pairs the Name is that of the TcTyVar. But
    then kappa1 and kappa2 get unified; so after the zonking in
    'generalise' in 'kcTyClGroup' the Name and TcTyVar may differ.

See also Note [Type checking recursive type and class declarations].



Note [Type environment evolution]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcTyClsDecls.hs#L395>`__

As we typecheck a group of declarations the type environment evolves.
Consider for example:
  data B (a :: Type) = MkB (Proxy 'MkB)

We do the following steps:

  1. Start of tcTyClDecls: use mkPromotionErrorEnv to initialise the
     type env with promotion errors
            B   :-> TyConPE
            MkB :-> DataConPE

  2. kcTyCLGruup
      - Do getInitialKinds, which will signal a promotion
        error if B is used in any of the kinds needed to initialse
        B's kind (e.g. (a :: Type)) here

      - Extend the type env with these initial kinds (monomorphic for
        decls that lack a CUSK)
            B :-> TcTyCon <initial kind>
        (thereby overriding the B :-> TyConPE binding)
        and do kcLTyClDecl on each decl to get equality constraints on
        all those inital kinds

      - Generalise the inital kind, making a poly-kinded TcTyCon

  3. Back in tcTyDecls, extend the envt with bindings of the poly-kinded
     TcTyCons, again overriding the promotion-error bindings.

::

     But note that the data constructor promotion errors are still in place
     so that (in our example) a use of MkB will sitll be signalled as
     an error.

  4. Typecheck the decls.

  5. In tcTyClGroup, extend the envt with bindings for TyCon and DataCons



Note [Missed opportunity to retain higher-rank kinds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcTyClsDecls.hs#L434>`__

In 'kcTyClGroup', there is a missed opportunity to make kind
inference work in a few more cases.  The idea is analogous
to Note [Single function non-recursive binding special-case]:

     * If we have an SCC with a single decl, which is non-recursive,
       instead of creating a unification variable representing the
       kind of the decl and unifying it with the rhs, we can just
       read the type directly of the rhs.

     * Furthermore, we can update our SCC analysis to ignore
       dependencies on declarations which have CUSKs: we don't
       have to kind-check these all at once, since we can use
       the CUSK to initialize the kind environment.

Unfortunately this requires reworking a bit of the code in
'kcLTyClDecl' so I've decided to punt unless someone shouts about it.



Note [Don't process associated types in kcLHsQTyVars]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcTyClsDecls.hs#L453>`__

Previously, we processed associated types in the thing_inside in kcLHsQTyVars,
but this was wrong -- we want to do ATs sepearately.
The consequence for not doing it this way is #15142:

::

  class ListTuple (tuple :: Type) (as :: [(k, Type)]) where
    type ListToTuple as :: Type

We assign k a kind kappa[1]. When checking the tuple (k, Type), we try to unify
kappa ~ Type, but this gets deferred because we bumped the TcLevel as we bring
`tuple` into scope. Thus, when we check ListToTuple, kappa[1] still hasn't
unified with Type. And then, when we generalize the kind of ListToTuple (which
indeed has a CUSK, according to the rules), we skolemize the free metavariable
kappa. Note that we wouldn't skolemize kappa when generalizing the kind of ListTuple,
because the solveEqualities in kcLHsQTyVars is at TcLevel 1 and so kappa[1]
will unify with Type.

Bottom line: as associated types should have no effect on a CUSK enclosing class,
we move processing them to a separate action, run after the outer kind has
been generalized.



Note [Required, Specified, and Inferred for types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcTyClsDecls.hs#L658>`__

Each forall'd type variable in a type or kind is one of

  * Required: an argument must be provided at every call site

  * Specified: the argument can be inferred at call sites, but
    may be instantiated with visible type/kind application

  * Inferred: the must be inferred at call sites; it
    is unavailable for use with visible type/kind application.

Why have Inferred at all? Because we just can't make user-facing
promises about the ordering of some variables. These might swizzle
around even between minor released. By forbidding visible type
application, we ensure users aren't caught unawares.

Go read Note [VarBndrs, TyCoVarBinders, TyConBinders, and visibility] in TyCoRep.

The question for this Note is this:
   given a TyClDecl, how are its quantified type variables classified?
Much of the debate is memorialized in #15743.

Here is our design choice. When inferring the ordering of variables
for a TyCl declaration (that is, for those variables that he user
has not specified the order with an explicit `forall`), we use the
following order:

 1. Inferred variables
 2. Specified variables; in the left-to-right order in which
    the user wrote them, modified by scopedSort (see below)
    to put them in depdendency order.
 3. Required variables before a top-level ::
 4. All variables after a top-level ::

If this ordering does not make a valid telescope, we reject the definition.

Example:
  data SameKind :: k -> k -> *
  data Bad a (c :: Proxy b) (d :: Proxy a) (x :: SameKind b d)

For Bad:
  - a, c, d, x are Required; they are explicitly listed by the user
    as the positional arguments of Bad
  - b is Specified; it appears explicitly in a kind signature
  - k, the kind of a, is Inferred; it is not mentioned explicitly at all

Putting variables in the order Inferred, Specified, Required
gives us this telescope:
  Inferred:  k
  Specified: b : Proxy a
  Required : (a : k) (c : Proxy b) (d : Proxy a) (x : SameKind b d)

But this order is ill-scoped, because b's kind mentions a, which occurs
after b in the telescope. So we reject Bad.

Associated types
~~~~~~~~~~~~~~~~
For associated types everything above is determined by the
associated-type declaration alone, ignoring the class header.
Here is an example (#15592)
  class C (a :: k) b where
    type F (x :: b a)

In the kind of C, 'k' is Specified.  But what about F?
In the kind of F,

 * Should k be Inferred or Specified?  It's Specified for C,
   but not mentioned in F's declaration.

 * In which order should the Specified variables a and b occur?
   It's clearly 'a' then 'b' in C's declaration, but the L-R ordering
   in F's declaration is 'b' then 'a'.

In both cases we make the choice by looking at F's declaration alone,
so it gets the kind
   F :: forall {k}. forall b a. b a -> Type

How it works
~~~~~~~~~~~~
These design choices are implemented by two completely different code
paths for

  * Declarations with a complete user-specified kind signature (CUSK)
    Handed by the CUSK case of kcLHsQTyVars.

  * Declarations without a CUSK are handled by kcTyClDecl; see
    Note [Inferring kinds for type declarations].

Note that neither code path worries about point (4) above, as this
is nicely handled by not mangling the res_kind. (Mangling res_kinds is done
*after* all this stuff, in tcDataDefn's call to etaExpandAlgTyCon.)

We can tell Inferred apart from Specified by looking at the scoped
tyvars; Specified are always included there.

Design alternatives
~~~~~~~~~~~~~~~~~~~
* For associated types we considered putting the class variables
  before the local variables, in a nod to the treatment for class
  methods. But it got too compilicated; see #15592, comment:21ff.

* We rigidly require the ordering above, even though we could be much more
  permissive. Relevant musings are at
  https://gitlab.haskell.org/ghc/ghc/issues/15743#note_161623
  The bottom line conclusion is that, if the user wants a different ordering,
  then can specify it themselves, and it is better to be predictable and dumb
  than clever and capricious.

::

  I (Richard) conjecture we could be fully permissive, allowing all classes
  of variables to intermix. We would have to augment ScopedSort to refuse to
  reorder Required variables (or check that it wouldn't have). But this would
  allow more programs. See #15743 for examples. Interestingly, Idris seems
  to allow this intermixing. The intermixing would be fully specified, in that
  we can be sure that inference wouldn't change between versions. However,
  would users be able to predict it? That I cannot answer.

Test cases (and tickets) relevant to these design decisions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  T15591*
  T15592*
  T15743*



Note [Inferring kinds for type declarations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcTyClsDecls.hs#L781>`__

This note deals with /inference/ for type declarations
that do not have a CUSK.  Consider
  data T (a :: k1) k2 (x :: k2) = MkT (S a k2 x)
  data S (b :: k3) k4 (y :: k4) = MkS (T b k4 y)

We do kind inference as follows:

* Step 1: getInitialKinds, and in particular kcLHsQTyVars_NonCusk.
  Make a unification variable for each of the Required and Specified
  type varialbes in the header.

::

  Record the connection between the Names the user wrote and the
  fresh unification variables in the tcTyConScopedTyVars field
  of the TcTyCon we are making
      [ (a,  aa)
      , (k1, kk1)
      , (k2, kk2)
      , (x,  xx) ]
  (I'm using the convention that double letter like 'aa' or 'kk'
  mean a unification variable.)

  These unification variables
    - Are TyVarTvs: that is, unification variables that can
      unify only with other type variables.
      See Note [Signature skolems] in TcType

    - Have complete fresh Names; see TcMType
      Note [Unification variables need fresh Names]

::

  Assign initial monomorophic kinds to S, T
          S :: kk1 -> * -> kk2 -> *
          T :: kk3 -> * -> kk4 -> *

* Step 2: kcTyClDecl. Extend the environment with a TcTyCon for S and
  T, with these monomophic kinds.  Now kind-check the declarations,
  and solve the resulting equalities.  The goal here is to discover
  constraints on all these unification variables.

::

  Here we find that kk1 := kk3, and kk2 := kk4.

::

  This is why we can't use skolems for kk1 etc; they have to
  unify with each other.

* Step 3: generaliseTcTyCon. Generalise each TyCon in turn.
  We find the free variables of the kind, skolemise them,
  sort them out into Inferred/Required/Specified (see the above
  Note [Required, Specified, and Inferred for types]),
  and perform some validity checks.

::

  This makes the utterly-final TyConBinders for the TyCon.

::

  All this is very similar at the level of terms: see TcBinds
  Note [Quantified variables in partial type signatures]

::

  But there some tricky corners: Note [Tricky scoping in generaliseTcTyCon]

* Step 4.  Extend the type environment with a TcTyCon for S and T, now
  with their utterly-final polymorphic kinds (needed for recursive
  occurrences of S, T).  Now typecheck the declarations, and build the
  final AlgTyCOn for S and T resp.

The first three steps are in kcTyClGroup; the fourth is in
tcTyClDecls.

There are some wrinkles

* Do not default TyVarTvs.  We always want to kind-generalise over
  TyVarTvs, and /not/ default them to Type. By definition a TyVarTv is
  not allowed to unify with a type; it must stand for a type
  variable. Hence the check in TcSimplify.defaultTyVarTcS, and
  TcMType.defaultTyVar.  Here's another example (#14555):
     data Exp :: [TYPE rep] -> TYPE rep -> Type where
        Lam :: Exp (a:xs) b -> Exp xs (a -> b)
  We want to kind-generalise over the 'rep' variable.
  #14563 is another example.

* Duplicate type variables. Consider #11203
    data SameKind :: k -> k -> *
    data Q (a :: k1) (b :: k2) c = MkQ (SameKind a b)
  Here we will unify k1 with k2, but this time doing so is an error,
  because k1 and k2 are bound in the same declaration.

::

  We spot this during validity checking (findDupTyVarTvs),
  in generaliseTcTyCon.

* Required arguments.  Even the Required arguments should be made
  into TyVarTvs, not skolems.  Consider
    data T k (a :: k)
  Here, k is a Required, dependent variable. For uniformity, it is helpful
  to have k be a TyVarTv, in parallel with other dependent variables.

* Duplicate skolemisation is expected.  When generalising in Step 3,
  we may find that one of the variables we want to quantify has
  already been skolemised.  For example, suppose we have already
  generalise S. When we come to T we'll find that kk1 (now the same as
  kk3) has already been skolemised.

::

  That's fine -- but it means that
    a) when collecting quantification candidates, in
       candidateQTyVarsOfKind, we must collect skolems
    b) quantifyTyVars should be a no-op on such a skolem



Note [Tricky scoping in generaliseTcTyCon]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcTyClsDecls.hs#L885>`__

Consider #16342
  class C (a::ka) x where
    cop :: D a x => x -> Proxy a -> Proxy a
    cop _ x = x :: Proxy (a::ka)

::

  class D (b::kb) y where
    dop :: C b y => y -> Proxy b -> Proxy b
    dop _ x = x :: Proxy (b::kb)

C and D are mutually recursive, by the time we get to
generaliseTcTyCon we'll have unified kka := kkb.

But when typechecking the default declarations for 'cop' and 'dop' in
tcDlassDecl2 we need {a, ka} and {b, kb} respectively to be in scope.
But at that point all we have is the utterly-final Class itself.

Conclusion: the classTyVars of a class must have the same Mame as
that originally assigned by the user.  In our example, C must have
classTyVars {a, ka, x} while D has classTyVars {a, kb, y}.  Despite
the fact that kka and kkb got unified!

We achieve this sleight of hand in generaliseTcTyCon, using
the specialised function zonkRecTyVarBndrs.  We make the call
   zonkRecTyVarBndrs [ka,a,x] [kkb,aa,xxx]
where the [ka,a,x] are the Names originally assigned by the user, and
[kkb,aa,xx] are the corresponding (post-zonking, skolemised) TcTyVars.
zonkRecTyVarBndrs builds a recursive ZonkEnv that binds
   kkb :-> (ka :: <zonked kind of kkb>)
   aa  :-> (a  :: <konked kind of aa>)
   etc
That is, it maps each skolemised TcTyVars to the utterly-final
TyVar to put in the class, with its correct user-specified name.
When generalising D we'll do the same thing, but the ZonkEnv will map
   kkb :-> (kb :: <zonked kind of kkb>)
   bb  :-> (b  :: <konked kind of bb>)
   etc
Note that 'kkb' again appears in the domain of the mapping, but this
time mapped to 'kb'.  That's how C and D end up with differently-named
final TyVars despite the fact that we unified kka:=kkb

zonkRecTyVarBndrs we need to do knot-tying because of the need to
apply this same substitution to the kind of each.  ------------



Note [Recursion and promoting data constructors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcTyClsDecls.hs#L1179>`__

We don't want to allow promotion in a strongly connected component
when kind checking.

Consider:
  data T f = K (f (K Any))

When kind checking the `data T' declaration the local env contains the
mappings:
  T -> ATcTyCon <some initial kind>
  K -> APromotionErr

APromotionErr is only used for DataCons, and only used during type checking
in tcTyClGroup.



Note [Kind-checking for GADTs]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcTyClsDecls.hs#L1195>`__

Consider

::

  data Proxy a where
    MkProxy1 :: forall k (b :: k). Proxy b
    MkProxy2 :: forall j (c :: j). Proxy c

It seems reasonable that this should be accepted. But something very strange
is going on here: when we're kind-checking this declaration, we need to unify
the kind of `a` with k and j -- even though k and j's scopes are local to the type of
MkProxy{1,2}. The best approach we've come up with is to use TyVarTvs during
the kind-checking pass. First off, note that it's OK if the kind-checking pass
is too permissive: we'll snag the problems in the type-checking pass later.
(This extra permissiveness might happen with something like

::

  data SameKind :: k -> k -> Type
  data Bad a where
    MkBad :: forall k1 k2 (a :: k1) (b :: k2). Bad (SameKind a b)

which would be accepted if k1 and k2 were TyVarTvs. This is correctly rejected
in the second pass, though. Test case: polykinds/TyVarTvKinds3)
Recall that the kind-checking pass exists solely to collect constraints
on the kinds and to power unification.

To achieve the use of TyVarTvs, we must be careful to use specialized functions
that produce TyVarTvs, not ordinary skolems. This is why we need
kcExplicitTKBndrs and kcImplicitTKBndrs in TcHsType, separate from their
tc... variants.

The drawback of this approach is sometimes it will accept a definition that
a (hypothetical) declarative specification would likely reject. As a general
rule, we don't want to allow polymorphic recursion without a CUSK. Indeed,
the whole point of CUSKs is to allow polymorphic recursion. Yet, the TyVarTvs
approach allows a limited form of polymorphic recursion *without* a CUSK.

To wit:
  data T a = forall k (b :: k). MkT (T b) Int
  (test case: dependent/should_compile/T14066a)

Note that this is polymorphically recursive, with the recursive occurrence
of T used at a kind other than a's kind. The approach outlined here accepts
this definition, because this kind is still a kind variable (and so the
TyVarTvs unify). Stepping back, I (Richard) have a hard time envisioning a
way to describe exactly what declarations will be accepted and which will
be rejected (without a CUSK). However, the accepted definitions are indeed
well-kinded and any rejected definitions would be accepted with a CUSK,
and so this wrinkle need not cause anyone to lose sleep.



Note [Type checking recursive type and class declarations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcTyClsDecls.hs#L1250>`__

At this point we have completed *kind-checking* of a mutually
recursive group of type/class decls (done in kcTyClGroup). However,
we discarded the kind-checked types (eg RHSs of data type decls);
note that kcTyClDecl returns ().  There are two reasons:

  * It's convenient, because we don't have to rebuild a
    kinded HsDecl (a fairly elaborate type)

  * It's necessary, because after kind-generalisation, the
    TyCons/Classes may now be kind-polymorphic, and hence need
    to be given kind arguments.

Example:
       data T f a = MkT (f a) (T f a)
During kind-checking, we give T the kind T :: k1 -> k2 -> *
and figure out constraints on k1, k2 etc. Then we generalise
to get   T :: forall k. (k->*) -> k -> *
So now the (T f a) in the RHS must be elaborated to (T k f a).

However, during tcTyClDecl of T (above) we will be in a recursive
"knot". So we aren't allowed to look at the TyCon T itself; we are only
allowed to put it (lazily) in the returned structures.  But when
kind-checking the RHS of T's decl, we *do* need to know T's kind (so
that we can correctly elaboarate (T k f a).  How can we get T's kind
without looking at T?  Delicate answer: during tcTyClDecl, we extend

::

  *Global* env with T -> ATyCon (the (not yet built) final TyCon for T)
  *Local*  env with T -> ATcTyCon (TcTyCon with the polymorphic kind of T)

Then:

  * During TcHsType.tcTyVar we look in the *local* env, to get the
    fully-known, not knot-tied TcTyCon for T.

  * Then, in TcHsSyn.zonkTcTypeToType (and zonkTcTyCon in particular)
    we look in the *global* env to get the TyCon.

This fancy footwork (with two bindings for T) is only necessary for the
TyCons or Classes of this recursive group.  Earlier, finished groups,
live in the global env only.

See also Note [Kind checking recursive type and class declarations]



Note [Kind checking recursive type and class declarations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcTyClsDecls.hs#L1295>`__

Before we can type-check the decls, we must kind check them. This
is done by establishing an "initial kind", which is a rather uninformed
guess at a tycon's kind (by counting arguments, mainly) and then
using this initial kind for recursive occurrences.

The initial kind is stored in exactly the same way during
kind-checking as it is during type-checking (Note [Type checking
recursive type and class declarations]): in the *local* environment,
with ATcTyCon. But we still must store *something* in the *global*
environment. Even though we discard the result of kind-checking, we
sometimes need to produce error messages. These error messages will
want to refer to the tycons being checked, except that they don't
exist yet, and it would be Terribly Annoying to get the error messages
to refer back to HsSyn. So we create a TcTyCon and put it in the
global env. This tycon can print out its name and knows its kind, but
any other action taken on it will panic. Note that TcTyCons are *not*
knot-tied, unlike the rather valid but knot-tied ones that occur
during type-checking.



Note [Declarations for wired-in things]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcTyClsDecls.hs#L1316>`__

For wired-in things we simply ignore the declaration
and take the wired-in information.  That avoids complications.
e.g. the need to make the data constructor worker name for
     a constraint tuple match the wired-in one



Note [Associated type defaults]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcTyClsDecls.hs#L1435>`__

The following is an example of associated type defaults:
             class C a where
               data D a

::

               type F a b :: *
               type F a b = [a]        -- Default

Note that we can get default definitions only for type families, not data
families.



Note [Type-checking default assoc decls]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcTyClsDecls.hs#L1542>`__

Consider this default declaration for an associated type

::

   class C a where
      type F (a :: k) b :: *
      type F x y = Proxy x -> y

Note that the class variable 'a' doesn't scope over the default assoc
decl (rather oddly I think), and (less oddly) neither does the second
argument 'b' of the associated type 'F', or the kind variable 'k'.
Instead, the default decl is treated more like a top-level type
instance.

However we store the default rhs (Proxy x -> y) in F's TyCon, using
F's own type variables, so we need to convert it to (Proxy a -> b).
We do this by calling tcMatchTys to match them up.  This also ensures
that x's kind matches a's and similarly for y and b.  The error
message isn't great, mind you.  (#11361 was caused by not doing a
proper tcMatchTys here.)

Recall also that the left-hand side of an associated type family
default is always just variables -- no tycons here. Accordingly,
the patterns used in the tcMatchTys won't actually be knot-tied,
even though we're in the knot. This is too delicate for my taste,
but it works.



Note [Instantiating a family tycon]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcTyClsDecls.hs#L1883>`__

It's possible that kind-checking the result of a family tycon applied to
its patterns will instantiate the tycon further. For example, we might
have

::

  type family F :: k where
    F = Int
    F = Maybe

After checking (F :: forall k. k) (with no visible patterns), we still need
to instantiate the k. With data family instances, this problem can be even
more intricate, due to Note [Arity of data families] in FamInstEnv. See
indexed-types/should_compile/T12369 for an example.

So, the kind-checker must return the new skolems and args (that is, Type
or (Type -> Type) for the equations above) and the instantiated kind.



Note [Generalising in tcFamTyPatsGuts]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcTyClsDecls.hs#L1901>`__

Suppose we have something like
  type instance forall (a::k) b. F t1 t2 = rhs

Then  imp_vars = [k], exp_bndrs = [a::k, b]

We want to quantify over
  * k, a, and b  (all user-specified)
  * and any inferred free kind vars from
      - the kinds of k, a, b
      - the types t1, t2

However, unlike a type signature like
  f :: forall (a::k). blah

we do /not/ care about the Inferred/Specified designation
or order for the final quantified tyvars.  Type-family
instances are not invoked directly in Haskell source code,
so visible type application etc plays no role.

So, the simple thing is
   - gather candiates from [k, a, b] and pats
   - quantify over them

Hence the sligtly mysterious call:
    candidateQTyVarsOfTypes (pats ++ mkTyVarTys scoped_tvs)

Simple, neat, but a little non-obvious!
------------------------



Note [Constraints in patterns]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcTyClsDecls.hs#L2051>`__

NB: This isn't the whole story. See comment in tcFamTyPats.

At first glance, it seems there is a complicated story to tell in tcFamTyPats
around constraint solving. After all, type family patterns can now do
GADT pattern-matching, which is jolly complicated. But, there's a key fact
which makes this all simple: everything is at top level! There cannot
be untouchable type variables. There can't be weird interaction between
case branches. There can't be global skolems.

This means that the semantics of type-level GADT matching is a little
different than term level. If we have

::

  data G a where
    MkGBool :: G Bool

And then

::

  type family F (a :: G k) :: k
  type instance F MkGBool = True

we get

::

  axF : F Bool (MkGBool <Bool>) ~ True

Simple! No casting on the RHS, because we can affect the kind parameter
to F.

If we ever introduce local type families, this all gets a lot more
complicated, and will end up looking awfully like term-level GADT
pattern-matching.


** The new story **

Here is really what we want:

The matcher really can't deal with covars in arbitrary spots in coercions.
But it can deal with covars that are arguments to GADT data constructors.
So we somehow want to allow covars only in precisely those spots, then use
them as givens when checking the RHS. TODO (RAE): Implement plan.



Note [Quantifying over family patterns]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcTyClsDecls.hs#L2095>`__

We need to quantify over two different lots of kind variables:

First, the ones that come from the kinds of the tyvar args of
tcTyVarBndrsKindGen, as usual
  data family Dist a

::

  -- Proxy :: forall k. k -> *
  data instance Dist (Proxy a) = DP
  -- Generates  data DistProxy = DP
  --            ax8 k (a::k) :: Dist * (Proxy k a) ~ DistProxy k a
  -- The 'k' comes from the tcTyVarBndrsKindGen (a::k)

Second, the ones that come from the kind argument of the type family
which we pick up using the (tyCoVarsOfTypes typats) in the result of
the thing_inside of tcHsTyvarBndrsGen.
  -- Any :: forall k. k
  data instance Dist Any = DA
  -- Generates  data DistAny k = DA
  --            ax7 k :: Dist k (Any k) ~ DistAny k
  -- The 'k' comes from kindGeneralizeKinds (Any k)



Note [Quantified kind variables of a family pattern]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcTyClsDecls.hs#L2118>`__

Consider   type family KindFam (p :: k1) (q :: k1)
           data T :: Maybe k1 -> k2 -> *
           type instance KindFam (a :: Maybe k) b = T a b -> Int
The HsBSig for the family patterns will be ([k], [a])

Then in the family instance we want to
  * Bring into scope [ "k" -> k:*, "a" -> a:k ]
  * Kind-check the RHS
  * Quantify the type instance over k and k', as well as a,b, thus
       type instance [k, k', a:Maybe k, b:k']
                     KindFam (Maybe k) k' a b = T k k' a b -> Int

Notice that in the third step we quantify over all the visibly-mentioned
type variables (a,b), but also over the implicitly mentioned kind variables
(k, k').  In this case one is bound explicitly but often there will be
none. The role of the kind signature (a :: Maybe k) is to add a constraint
that 'a' must have that kind, and to bring 'k' into scope.



Note [Infix GADT constructors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcTyClsDecls.hs#L2413>`__

We do not currently have syntax to declare an infix constructor in GADT syntax,
but it makes a (small) difference to the Show instance.  So as a slightly
ad-hoc solution, we regard a GADT data constructor as infix if
  a) it is an operator symbol
  b) it has two arguments
  c) there is a fixity declaration for it
For example:
   infix 6 (:--:)
   data T a where
     (:--:) :: t1 -> t2 -> T Int



Note [Checking GADT return types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcTyClsDecls.hs#L2427>`__

There is a delicacy around checking the return types of a datacon. The
central problem is dealing with a declaration like

::

  data T a where
    MkT :: T a -> Q a

Note that the return type of MkT is totally bogus. When creating the T
tycon, we also need to create the MkT datacon, which must have a "rejigged"
return type. That is, the MkT datacon's type must be transformed to have
a uniform return type with explicit coercions for GADT-like type parameters.
This rejigging is what rejigConRes does. The problem is, though, that checking
that the return type is appropriate is much easier when done over *Type*,
not *HsType*, and doing a call to tcMatchTy will loop because T isn't fully
defined yet.

So, we want to make rejigConRes lazy and then check the validity of
the return type in checkValidDataCon.  To do this we /always/ return a
6-tuple from rejigConRes (so that we can compute the return type from it, which
checkValidDataCon needs), but the first three fields may be bogus if
the return type isn't valid (the last equation for rejigConRes).

This is better than an earlier solution which reduced the number of
errors reported in one pass.  See #7175, and #10836.
Example
  data instance T (b,c) where
     TI :: forall e. e -> T (e,e)

The representation tycon looks like this:
  data :R7T b c where
     TI :: forall b1 c1. (b1 ~ c1) => b1 -> :R7T b1 c1
In this case orig_res_ty = T (e,e)



Note [mkGADTVars]
~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcTyClsDecls.hs#L2537>`__

Running example:

data T (k1 :: *) (k2 :: *) (a :: k2) (b :: k2) where
  MkT :: forall (x1 : *) (y :: x1) (z :: *).
         T x1 * (Proxy (y :: x1), z) z

We need the rejigged type to be

::

  MkT :: forall (x1 :: *) (k2 :: *) (a :: k2) (b :: k2).
         forall (y :: x1) (z :: *).
         (k2 ~ *, a ~ (Proxy x1 y, z), b ~ z)
      => T x1 k2 a b

You might naively expect that z should become a universal tyvar,
not an existential. (After all, x1 becomes a universal tyvar.)
But z has kind * while b has kind k2, so the return type
   T x1 k2 a z
is ill-kinded.  Another way to say it is this: the universal
tyvars must have exactly the same kinds as the tyConTyVars.

So we need an existential tyvar and a heterogeneous equality
constraint. (The b ~ z is a bit redundant with the k2 ~ * that
comes before in that b ~ z implies k2 ~ *. I'm sure we could do
some analysis that could eliminate k2 ~ *. But we don't do this
yet.)

The data con signature has already been fully kind-checked.
The return type

  T x1 * (Proxy (y :: x1), z) z
becomes
  qtkvs    = [x1 :: *, y :: x1, z :: *]
  res_tmpl = T x1 * (Proxy x1 y, z) z

We start off by matching (T k1 k2 a b) with (T x1 * (Proxy x1 y, z) z). We
know this match will succeed because of the validity check (actually done
later, but laziness saves us -- see Note [Checking GADT return types]).
Thus, we get

::

  subst := { k1 |-> x1, k2 |-> *, a |-> (Proxy x1 y, z), b |-> z }

Now, we need to figure out what the GADT equalities should be. In this case,
we *don't* want (k1 ~ x1) to be a GADT equality: it should just be a
renaming. The others should be GADT equalities. We also need to make
sure that the universally-quantified variables of the datacon match up
with the tyvars of the tycon, as required for Core context well-formedness.
(This last bit is why we have to rejig at all!)

`choose` walks down the tycon tyvars, figuring out what to do with each one.
It carries two substitutions:
  - t_sub's domain is *template* or *tycon* tyvars, mapping them to variables
    mentioned in the datacon signature.
  - r_sub's domain is *result* tyvars, names written by the programmer in
    the datacon signature. The final rejigged type will use these names, but
    the subst is still needed because sometimes the printed name of these variables
    is different. (See choose_tv_name, below.)

Before explaining the details of `choose`, let's just look at its operation
on our example:

::

  choose [] [] {} {} [k1, k2, a, b]
  -->          -- first branch of `case` statement
  choose
    univs:    [x1 :: *]
    eq_spec:  []
    t_sub:    {k1 |-> x1}
    r_sub:    {x1 |-> x1}
    t_tvs:    [k2, a, b]
  -->          -- second branch of `case` statement
  choose
    univs:    [k2 :: *, x1 :: *]
    eq_spec:  [k2 ~ *]
    t_sub:    {k1 |-> x1, k2 |-> k2}
    r_sub:    {x1 |-> x1}
    t_tvs:    [a, b]
  -->          -- second branch of `case` statement
  choose
    univs:    [a :: k2, k2 :: *, x1 :: *]
    eq_spec:  [ a ~ (Proxy x1 y, z)
              , k2 ~ * ]
    t_sub:    {k1 |-> x1, k2 |-> k2, a |-> a}
    r_sub:    {x1 |-> x1}
    t_tvs:    [b]
  -->          -- second branch of `case` statement
  choose
    univs:    [b :: k2, a :: k2, k2 :: *, x1 :: *]
    eq_spec:  [ b ~ z
              , a ~ (Proxy x1 y, z)
              , k2 ~ * ]
    t_sub:    {k1 |-> x1, k2 |-> k2, a |-> a, b |-> z}
    r_sub:    {x1 |-> x1}
    t_tvs:    []
  -->          -- end of recursion
  ( [x1 :: *, k2 :: *, a :: k2, b :: k2]
  , [k2 ~ *, a ~ (Proxy x1 y, z), b ~ z]
  , {x1 |-> x1} )

`choose` looks up each tycon tyvar in the matching (it *must* be matched!).

* If it finds a bare result tyvar (the first branch of the `case`
  statement), it checks to make sure that the result tyvar isn't yet
  in the list of univ_tvs.  If it is in that list, then we have a
  repeated variable in the return type, and we in fact need a GADT
  equality.

* It then checks to make sure that the kind of the result tyvar
  matches the kind of the template tyvar. This check is what forces
  `z` to be existential, as it should be, explained above.

* Assuming no repeated variables or kind-changing, we wish to use the
  variable name given in the datacon signature (that is, `x1` not
  `k1`), not the tycon signature (which may have been made up by
  GHC). So, we add a mapping from the tycon tyvar to the result tyvar
  to t_sub.

* If we discover that a mapping in `subst` gives us a non-tyvar (the
  second branch of the `case` statement), then we have a GADT equality
  to create.  We create a fresh equality, but we don't extend any
  substitutions. The template variable substitution is meant for use
  in universal tyvar kinds, and these shouldn't be affected by any
  GADT equalities.

This whole algorithm is quite delicate, indeed. I (Richard E.) see two ways
of simplifying it:

1) The first branch of the `case` statement is really an optimization, used
in order to get fewer GADT equalities. It might be possible to make a GADT
equality for *every* univ. tyvar, even if the equality is trivial, and then
either deal with the bigger type or somehow reduce it later.

2) This algorithm strives to use the names for type variables as specified
by the user in the datacon signature. If we always used the tycon tyvar
names, for example, this would be simplified. This change would almost
certainly degrade error messages a bit, though.



Note [Substitution in template variables kinds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcTyClsDecls.hs#L2750>`__

data G (a :: Maybe k) where
  MkG :: G Nothing

With explicit kind variables

data G k (a :: Maybe k) where
  MkG :: G k1 (Nothing k1)

Note how k1 is distinct from k. So, when we match the template
`G k a` against `G k1 (Nothing k1)`, we get a subst
[ k |-> k1, a |-> Nothing k1 ]. Even though this subst has two
mappings, we surely don't want to add (k, k1) to the list of
GADT equalities -- that would be overly complex and would create
more untouchable variables than we need. So, when figuring out
which tyvars are GADT-like and which aren't (the fundamental
job of `choose`), we want to treat `k` as *not* GADT-like.
Instead, we wish to substitute in `a`'s kind, to get (a :: Maybe k1)
instead of (a :: Maybe k). This is the reason for dealing
with a substitution in here.

However, we do not *always* want to substitute. Consider

data H (a :: k) where
  MkH :: H Int

With explicit kind variables:

data H k (a :: k) where
  MkH :: H * Int

Here, we have a kind-indexed GADT. The subst in question is
[ k |-> *, a |-> Int ]. Now, we *don't* want to substitute in `a`'s
kind, because that would give a constructor with the type

MkH :: forall (k :: *) (a :: *). (k ~ *) -> (a ~ Int) -> H k a

The problem here is that a's kind is wrong -- it needs to be k, not *!
So, if the matching for a variable is anything but another bare variable,
we drop the mapping from the substitution before proceeding. This
was not an issue before kind-indexed GADTs because this case could
never happen.



Note [Recover from validity error]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcTyClsDecls.hs#L2831>`__

We recover from a validity error in a type or class, which allows us
to report multiple validity errors. In the failure case we return a
TyCon of the right kind, but with no interesting behaviour
(makeRecoveryTyCon). Why?  Suppose we have
   type T a = Fun
where Fun is a type family of arity 1.  The RHS is invalid, but we
want to go on checking validity of subsequent type declarations.
So we replace T with an abstract TyCon which will do no harm.
See indexed-types/should_fail/BadSock and #10896

Some notes:

* We must make fakes for promoted DataCons too. Consider (#15215)
      data T a = MkT ...
      data S a = ...T...MkT....
  If there is an error in the definition of 'T' we add a "fake type
  constructor" to the type environment, so that we can continue to
  typecheck 'S'.  But we /were not/ adding a fake anything for 'MkT'
  and so there was an internal error when we met 'MkT' in the body of
  'S'.

* Painfully, we *don't* want to do this for classes.
  Consider tcfail041:
     class (?x::Int) => C a where ...
     instance C Int
  The class is invalid because of the superclass constraint.  But
  we still want it to look like a /class/, else the instance bleats
  that the instance is mal-formed because it hasn't got a class in
  the head.

  This is really bogus; now we have in scope a Class that is invalid
  in some way, with unknown downstream consequences.  A better
  alterantive might be to make a fake class TyCon.  A job for another day.
-----------------------
 For data types declared with record syntax, we require
 that each constructor that has a field 'f'
      (a) has the same result type
      (b) has the same type for 'f'
 module alpha conversion of the quantified type variables
 of the constructor.

::

 Note that we allow existentials to match because the
 fields can never meet. E.g
      data T where
        T1 { f1 :: b, f2 :: a, f3 ::Int } :: T
        T2 { f1 :: c, f2 :: c, f3 ::Int } :: T
 Here we do not complain about f1,f2 because they are existential



Note [Class method constraints]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcTyClsDecls.hs#L3338>`__

Haskell 2010 is supposed to reject
  class C a where
    op :: Eq a => a -> a
where the method type constrains only the class variable(s).  (The extension
-XConstrainedClassMethods switches off this check.)  But regardless
we should not reject
  class C a where
    op :: (?x::Int) => a -> a
as pointed out in #11793. So the test here rejects the program if
  * -XConstrainedClassMethods is off
  * the tyvars of the constraint are non-empty
  * all the tyvars are class tyvars, none are locally quantified



Note [Abort when superclass cycle is detected]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcTyClsDecls.hs#L3353>`__

We must avoid doing the ambiguity check for the methods (in
checkValidClass.check_op) when there are already errors accumulated.
This is because one of the errors may be a superclass cycle, and
superclass cycles cause canonicalization to loop. Here is a
representative example:

::

  class D a => C a where
    meth :: D a => ()
  class C a => D a

This fixes #9415, #9739



Note [Default method type signatures must align]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcTyClsDecls.hs#L3367>`__

GHC enforces the invariant that a class method's default type signature
must "align" with that of the method's non-default type signature, as per
GHC #12918. For instance, if you have:

::

  class Foo a where
    bar :: forall b. Context => a -> b

Then a default type signature for bar must be alpha equivalent to
(forall b. a -> b). That is, the types must be the same modulo differences in
contexts. So the following would be acceptable default type signatures:

::

    default bar :: forall b. Context1 => a -> b
    default bar :: forall x. Context2 => a -> x

But the following are NOT acceptable default type signatures:

::

    default bar :: forall b. b -> a
    default bar :: forall x. x
    default bar :: a -> Int

Note that a is bound by the class declaration for Foo itself, so it is
not allowed to differ in the default type signature.

The default type signature (default bar :: a -> Int) deserves special mention,
since (a -> Int) is a straightforward instantiation of (forall b. a -> b). To
write this, you need to declare the default type signature like so:

::

    default bar :: forall b. (b ~ Int). a -> b

As noted in #12918, there are several reasons to do this:

1. It would make no sense to have a type that was flat-out incompatible with
   the non-default type signature. For instance, if you had:

::

     class Foo a where
       bar :: a -> Int
       default bar :: a -> Bool

::

   Then that would always fail in an instance declaration. So this check
   nips such cases in the bud before they have the chance to produce
   confusing error messages.

2. Internally, GHC uses TypeApplications to instantiate the default method in
   an instance. See Note [Default methods in instances] in TcInstDcls.
   Thus, GHC needs to know exactly what the universally quantified type
   variables are, and when instantiated that way, the default method's type
   must match the expected type.

3. Aesthetically, by only allowing the default type signature to differ in its
   context, we are making it more explicit the ways in which the default type
   signature is less polymorphic than the non-default type signature.

You might be wondering: why are the contexts allowed to be different, but not
the rest of the type signature? That's because default implementations often
rely on assumptions that the more general, non-default type signatures do not.
For instance, in the Enum class declaration:

::

    class Enum a where
      enum :: [a]
      default enum :: (Generic a, GEnum (Rep a)) => [a]
      enum = map to genum

::

    class GEnum f where
      genum :: [f a]

The default implementation for enum only works for types that are instances of
Generic, and for which their generic Rep type is an instance of GEnum. But
clearly enum doesn't _have_ to use this implementation, so naturally, the
context for enum is allowed to be different to accomodate this. As a result,
when we validity-check default type signatures, we ignore contexts completely.

Note that when checking whether two type signatures match, we must take care to
split as many foralls as it takes to retrieve the tau types we which to check.
See Note [Splitting nested sigma types in class type signatures].



Note [Splitting nested sigma types in class type signatures]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcTyClsDecls.hs#L3444>`__

Consider this type synonym and class definition:

::

  type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t

::

  class Each s t a b where
    each         ::                                      Traversal s t a b
    default each :: (Traversable g, s ~ g a, t ~ g b) => Traversal s t a b

It might seem obvious that the tau types in both type signatures for `each`
are the same, but actually getting GHC to conclude this is surprisingly tricky.
That is because in general, the form of a class method's non-default type
signature is:

::

  forall a. C a => forall d. D d => E a b

And the general form of a default type signature is:

::

  forall f. F f => E a f -- The variable `a` comes from the class

So it you want to get the tau types in each type signature, you might find it
reasonable to call tcSplitSigmaTy twice on the non-default type signature, and
call it once on the default type signature. For most classes and methods, this
will work, but Each is a bit of an exceptional case. The way `each` is written,
it doesn't quantify any additional type variables besides those of the Each
class itself, so the non-default type signature for `each` is actually this:

::

  forall s t a b. Each s t a b => Traversal s t a b

Notice that there _appears_ to only be one forall. But there's actually another
forall lurking in the Traversal type synonym, so if you call tcSplitSigmaTy
twice, you'll also go under the forall in Traversal! That is, you'll end up
with:

::

  (a -> f b) -> s -> f t

A problem arises because you only call tcSplitSigmaTy once on the default type
signature for `each`, which gives you

::

  Traversal s t a b

Or, equivalently:

::

  forall f. Applicative f => (a -> f b) -> s -> f t

This is _not_ the same thing as (a -> f b) -> s -> f t! So now tcMatchTy will
say that the tau types for `each` are not equal.

A solution to this problem is to use tcSplitNestedSigmaTys instead of
tcSplitSigmaTy. tcSplitNestedSigmaTys will always split any foralls that it
sees until it can't go any further, so if you called it on the default type
signature for `each`, it would return (a -> f b) -> s -> f t like we desired.



Note [Checking partial record field]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcTyClsDecls.hs#L3498>`__

This check checks the partial record field selector, and warns (#7169).

For example:

::

  data T a = A { m1 :: a, m2 :: a } | B { m1 :: a }

The function 'm2' is partial record field, and will fail when it is applied to
'B'. The warning identifies such partial fields. The check is performed at the
declaration of T, not at the call-sites of m2.

The warning can be suppressed by prefixing the field-name with an underscore.
For example:

::

  data T a = A { m1 :: a, _m2 :: a } | B { m1 :: a }

