`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/types/TyCoRep.hs>`_

Note [The Type-related module hierarchy]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  Class
  CoAxiom
  TyCon    imports Class, CoAxiom
  TyCoRep  imports Class, CoAxiom, TyCon
  TysPrim  imports TyCoRep ( including mkTyConTy )
  Kind     imports TysPrim ( mainly for primitive kinds )
  Type     imports Kind
  Coercion imports Type
We expose the relevant stuff from this module via the Type module


Note [ATyCon for classes]
~~~~~~~~~~~~~~~~~~~~~~~~~
Both classes and type constructors are represented in the type environment
as ATyCon.  You can tell the difference, and get to the class, with
   isClassTyCon :: TyCon -> Bool
   tyConClass_maybe :: TyCon -> Maybe Class
The Class and its associated TyCon have the same Name.


Note [Function types]
~~~~~~~~~~~~~~~~~~~~~~~~
FFunTy is the constructor for a function type.  Lots of things to say
about it!

* FFunTy is the data constructor, meaning "full function type".

* The function type constructor (->) has kind
     (->) :: forall r1 r2. TYPE r1 -> TYPE r2 -> Type LiftedRep
  mkTyConApp ensure that we convert a saturated application
    TyConApp (->) [r1,r2,t1,t2] into FunTy t1 t2
  dropping the 'r1' and 'r2' arguments; they are easily recovered
  from 't1' and 't2'.

* The ft_af field says whether or not this is an invisible argument
     VisArg:   t1 -> t2    Ordinary function type
     InvisArg: t1 => t2    t1 is guaranteed to be a predicate type,
                           i.e. t1 :: Constraint
  See Note [Types for coercions, predicates, and evidence]

  This visibility info makes no difference in Core; it matters
  only when we regard the type as a Haskell source type.

* FunTy is a (unidirectional) pattern synonym that allows
  positional pattern matching (FunTy arg res), ignoring the
  ArgFlag.
-----------------------
      Commented out until the pattern match
      checker can handle it; see #16185

      For now we use the CPP macro #define FunTy FFunTy _
      (see HsVersions.h) to allow pattern matching on a
      (positional) FunTy constructor.

{-# COMPLETE FunTy, TyVarTy, AppTy, TyConApp
           , ForAllTy, LitTy, CastTy, CoercionTy :: Type #-}

-- | 'FunTy' is a (uni-directional) pattern synonym for the common
-- case where we want to match on the argument/result type, but
-- ignoring the AnonArgFlag
pattern FunTy :: Type -> Type -> Type
pattern FunTy arg res <- FFunTy { ft_arg = arg, ft_res = res }

       End of commented out block
---------------------------------- 

Note [Types for coercions, predicates, and evidence]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We treat differently:

  (a) Predicate types
        Test: isPredTy
        Binders: DictIds
        Kind: Constraint
        Examples: (Eq a), and (a ~ b)

  (b) Coercion types are primitive, unboxed equalities
        Test: isCoVarTy
        Binders: CoVars (can appear in coercions)
        Kind: TYPE (TupleRep [])
        Examples: (t1 ~# t2) or (t1 ~R# t2)

  (c) Evidence types is the type of evidence manipulated by
      the type constraint solver.
        Test: isEvVarType
        Binders: EvVars
        Kind: Constraint or TYPE (TupleRep [])
        Examples: all coercion types and predicate types

Coercion types and predicate types are mutually exclusive,
but evidence types are a superset of both.

When treated as a user type,

  - Predicates (of kind Constraint) are invisible and are
    implicitly instantiated

  - Coercion types, and non-pred evidence types (i.e. not
    of kind Constrain), are just regular old types, are
    visible, and are not implicitly instantiated.

In a FunTy { ft_af = InvisArg }, the argument type is always
a Predicate type.



Note [Constraints in kinds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
Do we allow a type constructor to have a kind like
   S :: Eq a => a -> Type

No, we do not.  Doing so would mean would need a TyConApp like
   S @k @(d :: Eq k) (ty :: k)
 and we have no way to build, or decompose, evidence like
 (d :: Eq k) at the type level.

But we admit one exception: equality.  We /do/ allow, say,
   MkT :: (a ~ b) => a -> b -> Type a b

Why?  Because we can, without much difficulty.  Moreover
we can promote a GADT data constructor (see TyCon
Note [Promoted data constructors]), like
  data GT a b where
    MkGT : a -> a -> GT a a
so programmers might reasonably expect to be able to
promote MkT as well.

How does this work?

* In TcValidity.checkConstraintsOK we reject kinds that
  have constraints other than (a~b) and (a~~b).

* In Inst.tcInstInvisibleTyBinder we instantiate a call
  of MkT by emitting
     [W] co :: alpha ~# beta
  and producing the elaborated term
     MkT @alpha @beta (Eq# alpha beta co)
  We don't generate a boxed "Wanted"; we generate only a
  regular old /unboxed/ primitive-equality Wanted, and build
  the box on the spot.

* How can we get such a MkT?  By promoting a GADT-style data
  constructor
     data T a b where
       MkT :: (a~b) => a -> b -> T a b
  See DataCon.mkPromotedDataCon
  and Note [Promoted data constructors] in TyCon

* We support both homogeneous (~) and heterogeneous (~~)
  equality.  (See Note [The equality types story]
  in TysPrim for a primer on these equality types.)

* How do we prevent a MkT having an illegal constraint like
  Eq a?  We check for this at use-sites; see TcHsType.tcTyVar,
  specifically dc_theta_illegal_constraint.

* Notice that nothing special happens if
    K :: (a ~# b) => blah
  because (a ~# b) is not a predicate type, and is never
  implicitly instantiated. (Mind you, it's not clear how you
  could creates a type constructor with such a kind.) See
  Note [Types for coercions, predicates, and evidence]

* The existence of promoted MkT with an equality-constraint
  argument is the (only) reason that the AnonTCB constructor
  of TyConBndrVis carries an AnonArgFlag (VisArg/InvisArg).
  For example, when we promote the data constructor
     MkT :: forall a b. (a~b) => a -> b -> T a b
  we get a PromotedDataCon with tyConBinders
      Bndr (a :: Type)  (NamedTCB Inferred)
      Bndr (b :: Type)  (NamedTCB Inferred)
      Bndr (_ :: a ~ b) (AnonTCB InvisArg)
      Bndr (_ :: a)     (AnonTCB VisArg))
      Bndr (_ :: b)     (AnonTCB VisArg))

* One might reasonably wonder who *unpacks* these boxes once they are
  made. After all, there is no type-level `case` construct. The
  surprising answer is that no one ever does. Instead, if a GADT
  constructor is used on the left-hand side of a type family equation,
  that occurrence forces GHC to unify the types in question. For
  example:

  data G a where
    MkG :: G Bool

  type family F (x :: G a) :: a where
    F MkG = False

  When checking the LHS `F MkG`, GHC sees the MkG constructor and then must
  unify F's implicit parameter `a` with Bool. This succeeds, making the equation

    F Bool (MkG @Bool <Bool>) = False

  Note that we never need unpack the coercion. This is because type
  family equations are *not* parametric in their kind variables. That
  is, we could have just said

  type family H (x :: G a) :: a where
    H _ = False

  The presence of False on the RHS also forces `a` to become Bool,
  giving us

    H Bool _ = False

  The fact that any of this works stems from the lack of phase
  separation between types and kinds (unlike the very present phase
  separation between terms and types).

  Once we have the ability to pattern-match on types below top-level,
  this will no longer cut it, but it seems fine for now.




Note [Arguments to type constructors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Because of kind polymorphism, in addition to type application we now
have kind instantiation. We reuse the same notations to do so.

For example:

  Just (* -> *) Maybe
  Right * Nat Zero

are represented by:

  TyConApp (PromotedDataCon Just) [* -> *, Maybe]
  TyConApp (PromotedDataCon Right) [*, Nat, (PromotedDataCon Zero)]

Important note: Nat is used as a *kind* and not as a type. This can be
confusing, since type-level Nat and kind-level Nat are identical. We
use the kind of (PromotedDataCon Right) to know if its arguments are
kinds or types.

This kind instantiation only happens in TyConApp currently.



Note [Non-trivial definitional equality]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Is Int |> <*> the same as Int? YES! In order to reduce headaches,
we decide that any reflexive casts in types are just ignored.
(Indeed they must be. See Note [Respecting definitional equality].)
More generally, the `eqType` function, which defines Core's type equality
relation, ignores casts and coercion arguments, as long as the
two types have the same kind. This allows us to be a little sloppier
in keeping track of coercions, which is a good thing. It also means
that eqType does not depend on eqCoercion, which is also a good thing.

Why is this sensible? That is, why is something different than α-equivalence
appropriate for the implementation of eqType?

Anything smaller than ~ and homogeneous is an appropriate definition for
equality. The type safety of FC depends only on ~. Let's say η : τ ~ σ. Any
expression of type τ can be transmuted to one of type σ at any point by
casting. The same is true of expressions of type σ. So in some sense, τ and σ
are interchangeable.

But let's be more precise. If we examine the typing rules of FC (say, those in
https://cs.brynmawr.edu/~rae/papers/2015/equalities/equalities.pdf)
there are several places where the same metavariable is used in two different
premises to a rule. (For example, see Ty_App.) There is an implicit equality
check here. What definition of equality should we use? By convention, we use
α-equivalence. Take any rule with one (or more) of these implicit equality
checks. Then there is an admissible rule that uses ~ instead of the implicit
check, adding in casts as appropriate.

The only problem here is that ~ is heterogeneous. To make the kinds work out
in the admissible rule that uses ~, it is necessary to homogenize the
coercions. That is, if we have η : (τ : κ1) ~ (σ : κ2), then we don't use η;
we use η |> kind η, which is homogeneous.

The effect of this all is that eqType, the implementation of the implicit
equality check, can use any homogeneous relation that is smaller than ~, as
those rules must also be admissible.

A more drawn out argument around all of this is presented in Section 7.2 of
Richard E's thesis (http://cs.brynmawr.edu/~rae/papers/2016/thesis/eisenberg-thesis.pdf).

What would go wrong if we insisted on the casts matching? See the beginning of
Section 8 in the unpublished paper above. Theoretically, nothing at all goes
wrong. But in practical terms, getting the coercions right proved to be
nightmarish. And types would explode: during kind-checking, we often produce
reflexive kind coercions. When we try to cast by these, mkCastTy just discards
them. But if we used an eqType that distinguished between Int and Int |> <*>,
then we couldn't discard -- the output of kind-checking would be enormous,
and we would need enormous casts with lots of CoherenceCo's to straighten
them out.

Would anything go wrong if eqType respected type families? No, not at all. But
that makes eqType rather hard to implement.

Thus, the guideline for eqType is that it should be the largest
easy-to-implement relation that is still smaller than ~ and homogeneous. The
precise choice of relation is somewhat incidental, as long as the smart
constructors and destructors in Type respect whatever relation is chosen.

Another helpful principle with eqType is this:

 (EQ) If (t1 `eqType` t2) then I can replace t1 by t2 anywhere.

This principle also tells us that eqType must relate only types with the
same kinds.



Note [Respecting definitional equality]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Note [Non-trivial definitional equality] introduces the property (EQ).
How is this upheld?

Any function that pattern matches on all the constructors will have to
consider the possibility of CastTy. Presumably, those functions will handle
CastTy appropriately and we'll be OK.

More dangerous are the splitXXX functions. Let's focus on splitTyConApp.
We don't want it to fail on (T a b c |> co). Happily, if we have
  (T a b c |> co) `eqType` (T d e f)
then co must be reflexive. Why? eqType checks that the kinds are equal, as
well as checking that (a `eqType` d), (b `eqType` e), and (c `eqType` f).
By the kind check, we know that (T a b c |> co) and (T d e f) have the same
kind. So the only way that co could be non-reflexive is for (T a b c) to have
a different kind than (T d e f). But because T's kind is closed (all tycon kinds
are closed), the only way for this to happen is that one of the arguments has
to differ, leading to a contradiction. Thus, co is reflexive.

Accordingly, by eliminating reflexive casts, splitTyConApp need not worry
about outermost casts to uphold (EQ). Eliminating reflexive casts is done
in mkCastTy.

Unforunately, that's not the end of the story. Consider comparing
  (T a b c)      =?       (T a b |> (co -> <Type>)) (c |> co)
These two types have the same kind (Type), but the left type is a TyConApp
while the right type is not. To handle this case, we say that the right-hand
type is ill-formed, requiring an AppTy never to have a casted TyConApp
on its left. It is easy enough to pull around the coercions to maintain
this invariant, as done in Type.mkAppTy. In the example above, trying to
form the right-hand type will instead yield (T a b (c |> co |> sym co) |> <Type>).
Both the casts there are reflexive and will be dropped. Huzzah.

This idea of pulling coercions to the right works for splitAppTy as well.

However, there is one hiccup: it's possible that a coercion doesn't relate two
Pi-types. For example, if we have @type family Fun a b where Fun a b = a -> b@,
then we might have (T :: Fun Type Type) and (T |> axFun) Int. That axFun can't
be pulled to the right. But we don't need to pull it: (T |> axFun) Int is not
`eqType` to any proper TyConApp -- thus, leaving it where it is doesn't violate
our (EQ) property.

Lastly, in order to detect reflexive casts reliably, we must make sure not
to have nested casts: we update (t |> co1 |> co2) to (t |> (co1 `TransCo` co2)).

In sum, in order to uphold (EQ), we need the following three invariants:

  (EQ1) No decomposable CastTy to the left of an AppTy, where a decomposable
        cast is one that relates either a FunTy to a FunTy or a
        ForAllTy to a ForAllTy.
  (EQ2) No reflexive casts in CastTy.
  (EQ3) No nested CastTys.
  (EQ4) No CastTy over (ForAllTy (Bndr tyvar vis) body).
        See Note [Weird typing rule for ForAllTy] in Type.

These invariants are all documented above, in the declaration for Type.



Note [Unused coercion variable in ForAllTy]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have
  \(co:t1 ~ t2). e

What type should we give to this expression?
  (1) forall (co:t1 ~ t2) -> t
  (2) (t1 ~ t2) -> t

If co is used in t, (1) should be the right choice.
if co is not used in t, we would like to have (1) and (2) equivalent.

However, we want to keep eqType simple and don't want eqType (1) (2) to return
True in any case.

We decide to always construct (2) if co is not used in t.

Thus in mkLamType, we check whether the variable is a coercion
variable (of type (t1 ~# t2), and whether it is un-used in the
body. If so, it returns a FunTy instead of a ForAllTy.

There are cases we want to skip the check. For example, the check is
unnecessary when it is known from the context that the input variable
is a type variable.  In those cases, we use mkForAllTy.



Note [TyCoBinders]
~~~~~~~~~~~~~~~~~~~
A ForAllTy contains a TyCoVarBinder.  But a type can be decomposed
to a telescope consisting of a [TyCoBinder]

A TyCoBinder represents the type of binders -- that is, the type of an
argument to a Pi-type. GHC Core currently supports two different
Pi-types:

 * A non-dependent function type,
   written with ->, e.g. ty1 -> ty2
   represented as FunTy ty1 ty2. These are
   lifted to Coercions with the corresponding FunCo.

 * A dependent compile-time-only polytype,
   written with forall, e.g.  forall (a:*). ty
   represented as ForAllTy (Bndr a v) ty

Both Pi-types classify terms/types that take an argument. In other
words, if `x` is either a function or a polytype, `x arg` makes sense
(for an appropriate `arg`).




Note [VarBndrs, TyCoVarBinders, TyConBinders, and visibility]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
* A ForAllTy (used for both types and kinds) contains a TyCoVarBinder.
  Each TyCoVarBinder
      Bndr a tvis
  is equipped with tvis::ArgFlag, which says whether or not arguments
  for this binder should be visible (explicit) in source Haskell.

* A TyCon contains a list of TyConBinders.  Each TyConBinder
      Bndr a cvis
  is equipped with cvis::TyConBndrVis, which says whether or not type
  and kind arguments for this TyCon should be visible (explicit) in
  source Haskell.

This table summarises the visibility rules:
---------------------------------------------------------------------------------------
|                                                      Occurrences look like this
|                             GHC displays type as     in Haskell source code
|--------------------------------------------------------------------------------------
| Bndr a tvis :: TyCoVarBinder, in the binder of ForAllTy for a term
|  tvis :: ArgFlag
|  tvis = Inferred:            f :: forall {a}. type    Arg not allowed:  f
                               f :: forall {co}. type   Arg not allowed:  f
|  tvis = Specified:           f :: forall a. type      Arg optional:     f  or  f @Int
|  tvis = Required:            T :: forall k -> type    Arg required:     T *
|    This last form is illegal in terms: See Note [No Required TyCoBinder in terms]
|
| Bndr k cvis :: TyConBinder, in the TyConBinders of a TyCon
|  cvis :: TyConBndrVis
|  cvis = AnonTCB:             T :: kind -> kind        Required:            T *
|  cvis = NamedTCB Inferred:   T :: forall {k}. kind    Arg not allowed:     T
|                              T :: forall {co}. kind   Arg not allowed:     T
|  cvis = NamedTCB Specified:  T :: forall k. kind      Arg not allowed[1]:  T
|  cvis = NamedTCB Required:   T :: forall k -> kind    Required:            T *
---------------------------------------------------------------------------------------

[1] In types, in the Specified case, it would make sense to allow
    optional kind applications, thus (T @*), but we have not
    yet implemented that

---- In term declarations ----

* Inferred.  Function defn, with no signature:  f1 x = x
  We infer f1 :: forall {a}. a -> a, with 'a' Inferred
  It's Inferred because it doesn't appear in any
  user-written signature for f1

* Specified.  Function defn, with signature (implicit forall):
     f2 :: a -> a; f2 x = x
  So f2 gets the type f2 :: forall a. a -> a, with 'a' Specified
  even though 'a' is not bound in the source code by an explicit forall

* Specified.  Function defn, with signature (explicit forall):
     f3 :: forall a. a -> a; f3 x = x
  So f3 gets the type f3 :: forall a. a -> a, with 'a' Specified

* Inferred/Specified.  Function signature with inferred kind polymorphism.
     f4 :: a b -> Int
  So 'f4' gets the type f4 :: forall {k} (a:k->*) (b:k). a b -> Int
  Here 'k' is Inferred (it's not mentioned in the type),
  but 'a' and 'b' are Specified.

* Specified.  Function signature with explicit kind polymorphism
     f5 :: a (b :: k) -> Int
  This time 'k' is Specified, because it is mentioned explicitly,
  so we get f5 :: forall (k:*) (a:k->*) (b:k). a b -> Int

* Similarly pattern synonyms:
  Inferred - from inferred types (e.g. no pattern type signature)
           - or from inferred kind polymorphism

---- In type declarations ----

* Inferred (k)
     data T1 a b = MkT1 (a b)
  Here T1's kind is  T1 :: forall {k:*}. (k->*) -> k -> *
  The kind variable 'k' is Inferred, since it is not mentioned

  Note that 'a' and 'b' correspond to /Anon/ TyCoBinders in T1's kind,
  and Anon binders don't have a visibility flag. (Or you could think
  of Anon having an implicit Required flag.)

* Specified (k)
     data T2 (a::k->*) b = MkT (a b)
  Here T's kind is  T :: forall (k:*). (k->*) -> k -> *
  The kind variable 'k' is Specified, since it is mentioned in
  the signature.

* Required (k)
     data T k (a::k->*) b = MkT (a b)
  Here T's kind is  T :: forall k:* -> (k->*) -> k -> *
  The kind is Required, since it bound in a positional way in T's declaration
  Every use of T must be explicitly applied to a kind

* Inferred (k1), Specified (k)
     data T a b (c :: k) = MkT (a b) (Proxy c)
  Here T's kind is  T :: forall {k1:*} (k:*). (k1->*) -> k1 -> k -> *
  So 'k' is Specified, because it appears explicitly,
  but 'k1' is Inferred, because it does not

Generally, in the list of TyConBinders for a TyCon,

* Inferred arguments always come first
* Specified, Anon and Required can be mixed

e.g.
  data Foo (a :: Type) :: forall b. (a -> b -> Type) -> Type where ...

Here Foo's TyConBinders are
   [Required 'a', Specified 'b', Anon]
and its kind prints as
   Foo :: forall a -> forall b. (a -> b -> Type) -> Type

See also Note [Required, Specified, and Inferred for types] in TcTyClsDecls

---- Printing -----

 We print forall types with enough syntax to tell you their visibility
 flag.  But this is not source Haskell, and these types may not all
 be parsable.

 Specified: a list of Specified binders is written between `forall` and `.`:
               const :: forall a b. a -> b -> a

 Inferred:  with -fprint-explicit-foralls, Inferred binders are written
            in braces:
               f :: forall {k} (a:k). S k a -> Int
            Otherwise, they are printed like Specified binders.

 Required: binders are put between `forall` and `->`:
              T :: forall k -> *

---- Other points -----

* In classic Haskell, all named binders (that is, the type variables in
  a polymorphic function type f :: forall a. a -> a) have been Inferred.

* Inferred variables correspond to "generalized" variables from the
  Visible Type Applications paper (ESOP'16).



Note [No Required TyCoBinder in terms]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We don't allow Required foralls for term variables, including pattern
synonyms and data constructors.  Why?  Because then an application
would need a /compulsory/ type argument (possibly without an "@"?),
thus (f Int); and we don't have concrete syntax for that.

We could change this decision, but Required, Named TyCoBinders are rare
anyway.  (Most are Anons.)

However the type of a term can (just about) have a required quantifier;
see Note [Required quantifiers in the type of a term] in TcExpr.


Note [Refl invariant]
~~~~~~~~~~~~~~~~~~~~~
Invariant 1:

Coercions have the following invariant
     Refl (similar for GRefl r ty MRefl) is always lifted as far as possible.

You might think that a consequencs is:
     Every identity coercions has Refl at the root

But that's not quite true because of coercion variables.  Consider
     g         where g :: Int~Int
     Left h    where h :: Maybe Int ~ Maybe Int
etc.  So the consequence is only true of coercions that
have no coercion variables.



Note [Generalized reflexive coercion]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

GRefl is a generalized reflexive coercion (see #15192). It wraps a kind
coercion, which might be reflexive (MRefl) or any coercion (MCo co). The typing
rules for GRefl:

  ty : k1
  ------------------------------------
  GRefl r ty MRefl: ty ~r ty

  ty : k1       co :: k1 ~ k2
  ------------------------------------
  GRefl r ty (MCo co) : ty ~r ty |> co

Consider we have

   g1 :: s ~r t
   s  :: k1
   g2 :: k1 ~ k2

and we want to construct a coercions co which has type

   (s |> g2) ~r t

We can define

   co = Sym (GRefl r s g2) ; g1

It is easy to see that

   Refl == GRefl Nominal ty MRefl :: ty ~n ty

A nominal reflexive coercion is quite common, so we keep the special form Refl to
save allocation.



Note [Coercion axioms applied to coercions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The reason coercion axioms can be applied to coercions and not just
types is to allow for better optimization.  There are some cases where
we need to be able to "push transitivity inside" an axiom in order to
expose further opportunities for optimization.

For example, suppose we have

  C a : t[a] ~ F a
  g   : b ~ c

and we want to optimize

  sym (C b) ; t[g] ; C c

which has the kind

  F b ~ F c

(stopping through t[b] and t[c] along the way).

We'd like to optimize this to just F g -- but how?  The key is
that we need to allow axioms to be instantiated by *coercions*,
not just by types.  Then we can (in certain cases) push
transitivity inside the axiom instantiations, and then react
opposite-polarity instantiations of the same axiom.  In this
case, e.g., we match t[g] against the LHS of (C c)'s kind, to
obtain the substitution  a |-> g  (note this operation is sort
of the dual of lifting!) and hence end up with

  C g : t[b] ~ F c

which indeed has the same kind as  t[g] ; C c.

Now we have

  sym (C b) ; C g

which can be optimized to F g.



Note [CoAxiom index]
~~~~~~~~~~~~~~~~~~~~
A CoAxiom has 1 or more branches. Each branch has contains a list
of the free type variables in that branch, the LHS type patterns,
and the RHS type for that branch. When we apply an axiom to a list
of coercions, we must choose which branch of the axiom we wish to
use, as the different branches may have different numbers of free
type variables. (The number of type patterns is always the same
among branches, but that doesn't quite concern us here.)

The Int in the AxiomInstCo constructor is the 0-indexed number
of the chosen branch.



Note [Forall coercions]
~~~~~~~~~~~~~~~~~~~~~~~
Constructing coercions between forall-types can be a bit tricky,
because the kinds of the bound tyvars can be different.

The typing rule is:


  kind_co : k1 ~ k2
  tv1:k1 |- co : t1 ~ t2
  -------------------------------------------------------------------
  ForAllCo tv1 kind_co co : all tv1:k1. t1  ~
                            all tv1:k2. (t2[tv1 |-> tv1 |> sym kind_co])

First, the TyCoVar stored in a ForAllCo is really an optimisation: this field
should be a Name, as its kind is redundant. Thinking of the field as a Name
is helpful in understanding what a ForAllCo means.
The kind of TyCoVar always matches the left-hand kind of the coercion.

The idea is that kind_co gives the two kinds of the tyvar. See how, in the
conclusion, tv1 is assigned kind k1 on the left but kind k2 on the right.

Of course, a type variable can't have different kinds at the same time. So,
we arbitrarily prefer the first kind when using tv1 in the inner coercion
co, which shows that t1 equals t2.

The last wrinkle is that we need to fix the kinds in the conclusion. In
t2, tv1 is assumed to have kind k1, but it has kind k2 in the conclusion of
the rule. So we do a kind-fixing substitution, replacing (tv1:k1) with
(tv1:k2) |> sym kind_co. This substitution is slightly bizarre, because it
mentions the same name with different kinds, but it *is* well-kinded, noting
that `(tv1:k2) |> sym kind_co` has kind k1.

This all really would work storing just a Name in the ForAllCo. But we can't
add Names to, e.g., VarSets, and there generally is just an impedance mismatch
in a bunch of places. So we use tv1. When we need tv2, we can use
setTyVarKind.



Note [Predicate coercions]
~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have
   g :: a~b
How can we coerce between types
   ([c]~a) => [a] -> c
and
   ([c]~b) => [b] -> c
where the equality predicate *itself* differs?

Answer: we simply treat (~) as an ordinary type constructor, so these
types really look like

   ((~) [c] a) -> [a] -> c
   ((~) [c] b) -> [b] -> c

So the coercion between the two is obviously

   ((~) [c] g) -> [g] -> c

Another way to see this to say that we simply collapse predicates to
their representation type (see Type.coreView and Type.predTypeRep).

This collapse is done by mkPredCo; there is no PredCo constructor
in Coercion.  This is important because we need Nth to work on
predicates too:
    Nth 1 ((~) [c] g) = g
See Simplify.simplCoercionF, which generates such selections.



Note [Roles]
~~~~~~~~~~~~
Roles are a solution to the GeneralizedNewtypeDeriving problem, articulated
in #1496. The full story is in docs/core-spec/core-spec.pdf. Also, see
http://ghc.haskell.org/trac/ghc/wiki/RolesImplementation

Here is one way to phrase the problem:

Given:
newtype Age = MkAge Int
type family F x
type instance F Age = Bool
type instance F Int = Char

This compiles down to:
axAge :: Age ~ Int
axF1 :: F Age ~ Bool
axF2 :: F Int ~ Char

Then, we can make:
(sym (axF1) ; F axAge ; axF2) :: Bool ~ Char

Yikes!

The solution is _roles_, as articulated in "Generative Type Abstraction and
Type-level Computation" (POPL 2010), available at
http://www.seas.upenn.edu/~sweirich/papers/popl163af-weirich.pdf

The specification for roles has evolved somewhat since that paper. For the
current full details, see the documentation in docs/core-spec. Here are some
highlights.

We label every equality with a notion of type equivalence, of which there are
three options: Nominal, Representational, and Phantom. A ground type is
nominally equivalent only with itself. A newtype (which is considered a ground
type in Haskell) is representationally equivalent to its representation.
Anything is "phantomly" equivalent to anything else. We use "N", "R", and "P"
to denote the equivalences.

The axioms above would be:
axAge :: Age ~R Int
axF1 :: F Age ~N Bool
axF2 :: F Age ~N Char

Then, because transitivity applies only to coercions proving the same notion
of equivalence, the above construction is impossible.

However, there is still an escape hatch: we know that any two types that are
nominally equivalent are representationally equivalent as well. This is what
the form SubCo proves -- it "demotes" a nominal equivalence into a
representational equivalence. So, it would seem the following is possible:

sub (sym axF1) ; F axAge ; sub axF2 :: Bool ~R Char   -- WRONG

What saves us here is that the arguments to a type function F, lifted into a
coercion, *must* prove nominal equivalence. So, (F axAge) is ill-formed, and
we are safe.

Roles are attached to parameters to TyCons. When lifting a TyCon into a
coercion (through TyConAppCo), we need to ensure that the arguments to the
TyCon respect their roles. For example:

data T a b = MkT a (F b)

If we know that a1 ~R a2, then we know (T a1 b) ~R (T a2 b). But, if we know
that b1 ~R b2, we know nothing about (T a b1) and (T a b2)! This is because
the type function F branches on b's *name*, not representation. So, we say
that 'a' has role Representational and 'b' has role Nominal. The third role,
Phantom, is for parameters not used in the type's definition. Given the
following definition

data Q a = MkQ Int

the Phantom role allows us to say that (Q Bool) ~R (Q Char), because we
can construct the coercion Bool ~P Char (using UnivCo).

See the paper cited above for more examples and information.



Note [TyConAppCo roles]
~~~~~~~~~~~~~~~~~~~~~~~
The TyConAppCo constructor has a role parameter, indicating the role at
which the coercion proves equality. The choice of this parameter affects
the required roles of the arguments of the TyConAppCo. To help explain
it, assume the following definition:

  type instance F Int = Bool   -- Axiom axF : F Int ~N Bool
  newtype Age = MkAge Int      -- Axiom axAge : Age ~R Int
  data Foo a = MkFoo a         -- Role on Foo's parameter is Representational

TyConAppCo Nominal Foo axF : Foo (F Int) ~N Foo Bool
  For (TyConAppCo Nominal) all arguments must have role Nominal. Why?
  So that Foo Age ~N Foo Int does *not* hold.

TyConAppCo Representational Foo (SubCo axF) : Foo (F Int) ~R Foo Bool
TyConAppCo Representational Foo axAge       : Foo Age     ~R Foo Int
  For (TyConAppCo Representational), all arguments must have the roles
  corresponding to the result of tyConRoles on the TyCon. This is the
  whole point of having roles on the TyCon to begin with. So, we can
  have Foo Age ~R Foo Int, if Foo's parameter has role R.

  If a Representational TyConAppCo is over-saturated (which is otherwise fine),
  the spill-over arguments must all be at Nominal. This corresponds to the
  behavior for AppCo.

TyConAppCo Phantom Foo (UnivCo Phantom Int Bool) : Foo Int ~P Foo Bool
  All arguments must have role Phantom. This one isn't strictly
  necessary for soundness, but this choice removes ambiguity.

The rules here dictate the roles of the parameters to mkTyConAppCo
(should be checked by Lint).



Note [NthCo and newtypes]
~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have

  newtype N a = MkN Int
  type role N representational

This yields axiom

  NTCo:N :: forall a. N a ~R Int

We can then build

  co :: forall a b. N a ~R N b
  co = NTCo:N a ; sym (NTCo:N b)

for any `a` and `b`. Because of the role annotation on N, if we use
NthCo, we'll get out a representational coercion. That is:

  NthCo r 0 co :: forall a b. a ~R b

Yikes! Clearly, this is terrible. The solution is simple: forbid
NthCo to be used on newtypes if the internal coercion is representational.

This is not just some corner case discovered by a segfault somewhere;
it was discovered in the proof of soundness of roles and described
in the "Safe Coercions" paper (ICFP '14).



Note [NthCo Cached Roles]
~~~~~~~~~~~~~~~~~~~~~~~~~
Why do we cache the role of NthCo in the NthCo constructor?
Because computing role(Nth i co) involves figuring out that

  co :: T tys1 ~ T tys2

using coercionKind, and finding (coercionRole co), and then looking
at the tyConRoles of T. Avoiding bad asymptotic behaviour here means
we have to compute the kind and role of a coercion simultaneously,
which makes the code complicated and inefficient.

This only happens for NthCo. Caching the role solves the problem, and
allows coercionKind and coercionRole to be simple.

See #11735



Note [InstCo roles]
~~~~~~~~~~~~~~~~~~~
Here is (essentially) the typing rule for InstCo:

g :: (forall a. t1) ~r (forall a. t2)
w :: s1 ~N s2
------------------------------- InstCo
InstCo g w :: (t1 [a |-> s1]) ~r (t2 [a |-> s2])

Note that the Coercion w *must* be nominal. This is necessary
because the variable a might be used in a "nominal position"
(that is, a place where role inference would require a nominal
role) in t1 or t2. If we allowed w to be representational, we
could get bogus equalities.

A more nuanced treatment might be able to relax this condition
somewhat, by checking if t1 and/or t2 use their bound variables
in nominal ways. If not, having w be representational is OK.


%************************************************************************
%*                                                                      *
                UnivCoProvenance
%*                                                                      *
%************************************************************************

A UnivCo is a coercion whose proof does not directly express its role
and kind (indeed for some UnivCos, like UnsafeCoerceProv, there /is/
no proof).

The different kinds of UnivCo are described by UnivCoProvenance.  Really
each is entirely separate, but they all share the need to represent their
role and kind, which is done in the UnivCo constructor.



Note [Phantom coercions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
     data T a = T1 | T2
Then we have
     T s ~R T t
for any old s,t. The witness for this is (TyConAppCo T Rep co),
where (co :: s ~P t) is a phantom coercion built with PhantomProv.
The role of the UnivCo is always Phantom.  The Coercion stored is the
(nominal) kind coercion between the types
   kind(s) ~N kind (t)



Note [Coercion holes]
~~~~~~~~~~~~~~~~~~~~~~~~
During typechecking, constraint solving for type classes works by
  - Generate an evidence Id,  d7 :: Num a
  - Wrap it in a Wanted constraint, [W] d7 :: Num a
  - Use the evidence Id where the evidence is needed
  - Solve the constraint later
  - When solved, add an enclosing let-binding  let d7 = .... in ....
    which actually binds d7 to the (Num a) evidence

For equality constraints we use a different strategy.  See Note [The
equality types story] in TysPrim for background on equality constraints.
  - For /boxed/ equality constraints, (t1 ~N t2) and (t1 ~R t2), it's just
    like type classes above. (Indeed, boxed equality constraints *are* classes.)
  - But for /unboxed/ equality constraints (t1 ~R# t2) and (t1 ~N# t2)
    we use a different plan

For unboxed equalities:
  - Generate a CoercionHole, a mutable variable just like a unification
    variable
  - Wrap the CoercionHole in a Wanted constraint; see TcRnTypes.TcEvDest
  - Use the CoercionHole in a Coercion, via HoleCo
  - Solve the constraint later
  - When solved, fill in the CoercionHole by side effect, instead of
    doing the let-binding thing

The main reason for all this is that there may be no good place to let-bind
the evidence for unboxed equalities:

  - We emit constraints for kind coercions, to be used to cast a
    type's kind. These coercions then must be used in types. Because
    they might appear in a top-level type, there is no place to bind
    these (unlifted) coercions in the usual way.

  - A coercion for (forall a. t1) ~ (forall a. t2) will look like
       forall a. (coercion for t1~t2)
    But the coercion for (t1~t2) may mention 'a', and we don't have
    let-bindings within coercions.  We could add them, but coercion
    holes are easier.

  - Moreover, nothing is lost from the lack of let-bindings. For
    dicionaries want to achieve sharing to avoid recomoputing the
    dictionary.  But coercions are entirely erased, so there's little
    benefit to sharing. Indeed, even if we had a let-binding, we
    always inline types and coercions at every use site and drop the
    binding.

Other notes about HoleCo:

 * INVARIANT: CoercionHole and HoleCo are used only during type checking,
   and should never appear in Core. Just like unification variables; a Type
   can contain a TcTyVar, but only during type checking. If, one day, we
   use type-level information to separate out forms that can appear during
   type-checking vs forms that can appear in core proper, holes in Core will
   be ruled out.

 * See Note [CoercionHoles and coercion free variables]

 * Coercion holes can be compared for equality like other coercions:
   by looking at the types coerced.




Note [CoercionHoles and coercion free variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Why does a CoercionHole contain a CoVar, as well as reference to
fill in?  Because we want to treat that CoVar as a free variable of
the coercion.  See #14584, and Note [What prevents a
constraint from floating] in TcSimplify, item (4):

        forall k. [W] co1 :: t1 ~# t2 |> co2
                  [W] co2 :: k ~# *

Here co2 is a CoercionHole. But we /must/ know that it is free in
co1, because that's all that stops it floating outside the
implication.




Note [ProofIrrelProv]
~~~~~~~~~~~~~~~~~~~~~
A ProofIrrelProv is a coercion between coercions. For example:

  data G a where
    MkG :: G Bool

In core, we get

  G :: * -> *
  MkG :: forall (a :: *). (a ~ Bool) -> G a

Now, consider 'MkG -- that is, MkG used in a type -- and suppose we want
a proof that ('MkG a1 co1) ~ ('MkG a2 co2). This will have to be

  TyConAppCo Nominal MkG [co3, co4]
  where
    co3 :: co1 ~ co2
    co4 :: a1 ~ a2

Note that
  co1 :: a1 ~ Bool
  co2 :: a2 ~ Bool

Here,
  co3 = UnivCo (ProofIrrelProv co5) Nominal (CoercionTy co1) (CoercionTy co2)
  where
    co5 :: (a1 ~ Bool) ~ (a2 ~ Bool)
    co5 = TyConAppCo Nominal (~#) [<*>, <*>, co4, <Bool>]


%************************************************************************
%*                                                                      *
                 Free variables of types and coercions
%*                                                                      *
%************************************************************************


Note [Free variables of types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The family of functions tyCoVarsOfType, tyCoVarsOfTypes etc, returns
a VarSet that is closed over the types of its variables.  More precisely,
  if    S = tyCoVarsOfType( t )
  and   (a:k) is in S
  then  tyCoVarsOftype( k ) is a subset of S

Example: The tyCoVars of this ((a:* -> k) Int) is {a, k}.

We could /not/ close over the kinds of the variable occurrences, and
instead do so at call sites, but it seems that we always want to do
so, so it's easiest to do it here.

It turns out that getting the free variables of types is performance critical,
so we profiled several versions, exploring different implementation strategies.

1. Baseline version: uses FV naively. Essentially:

   tyCoVarsOfType ty = fvVarSet $ tyCoFVsOfType ty

   This is not nice, because FV introduces some overhead to implement
   determinism, and throught its "interesting var" function, neither of which
   we need here, so they are a complete waste.

2. UnionVarSet version: instead of reusing the FV-based code, we simply used
   VarSets directly, trying to avoid the overhead of FV. E.g.:

   -- FV version:
   tyCoFVsOfType (AppTy fun arg)    a b c = (tyCoFVsOfType fun `unionFV` tyCoFVsOfType arg) a b c

   -- UnionVarSet version:
   tyCoVarsOfType (AppTy fun arg)    = (tyCoVarsOfType fun `unionVarSet` tyCoVarsOfType arg)

   This looks deceptively similar, but while FV internally builds a list- and
   set-generating function, the VarSet functions manipulate sets directly, and
   the latter peforms a lot worse than the naive FV version.

3. Accumulator-style VarSet version: this is what we use now. We do use VarSet
   as our data structure, but delegate the actual work to a new
   ty_co_vars_of_...  family of functions, which use accumulator style and the
   "in-scope set" filter found in the internals of FV, but without the
   determinism overhead.

See #14880.



Note [Closing over free variable kinds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tyCoVarsOfType and tyCoFVsOfType, while traversing a type, will also close over
free variable kinds. In previous GHC versions, this happened naively: whenever
we would encounter an occurrence of a free type variable, we would close over
its kind. This, however is wrong for two reasons (see #14880):

1. Efficiency. If we have Proxy (a::k) -> Proxy (a::k) -> Proxy (a::k), then
   we don't want to have to traverse k more than once.

2. Correctness. Imagine we have forall k. b -> k, where b has
   kind k, for some k bound in an outer scope. If we look at b's kind inside
   the forall, we'll collect that k is free and then remove k from the set of
   free variables. This is plain wrong. We must instead compute that b is free
   and then conclude that b's kind is free.

An obvious first approach is to move the closing-over-kinds from the
occurrences of a type variable to after finding the free vars - however, this
turns out to introduce performance regressions, and isn't even entirely
correct.

In fact, it isn't even important *when* we close over kinds; what matters is
that we handle each type var exactly once, and that we do it in the right
context.

So the next approach we tried was to use the "in-scope set" part of FV or the
equivalent argument in the accumulator-style `ty_co_vars_of_type` function, to
say "don't bother with variables we have already closed over". This should work
fine in theory, but the code is complicated and doesn't perform well.

But there is a simpler way, which is implemented here. Consider the two points
above:

1. Efficiency: we now have an accumulator, so the second time we encounter 'a',
   we'll ignore it, certainly not looking at its kind - this is why
   pre-checking set membership before inserting ends up not only being faster,
   but also being correct.

2. Correctness: we have an "in-scope set" (I think we should call it it a
  "bound-var set"), specifying variables that are bound by a forall in the type
  we are traversing; we simply ignore these variables, certainly not looking at
  their kind.

So now consider:

    forall k. b -> k

where b :: k->Type is free; but of course, it's a different k! When looking at
b -> k we'll have k in the bound-var set. So we'll ignore the k. But suppose
this is our first encounter with b; we want the free vars of its kind. But we
want to behave as if we took the free vars of its kind at the end; that is,
with no bound vars in scope.

So the solution is easy. The old code was this:

  ty_co_vars_of_type (TyVarTy v) is acc
    | v `elemVarSet` is  = acc
    | v `elemVarSet` acc = acc
    | otherwise          = ty_co_vars_of_type (tyVarKind v) is (extendVarSet acc v)

Now all we need to do is take the free vars of tyVarKind v *with an empty
bound-var set*, thus:

ty_co_vars_of_type (TyVarTy v) is acc
  | v `elemVarSet` is  = acc
  | v `elemVarSet` acc = acc
  | otherwise          = ty_co_vars_of_type (tyVarKind v) emptyVarSet (extendVarSet acc v)
                                                          ^^^^^^^^^^^

And that's it.



Note [CoVarsOfX and the InterestingVarFun]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The coVarsOfType, coVarsOfTypes, coVarsOfCo, and coVarsOfCos functions are
implemented in terms of the respective FV equivalents (tyCoFVsOf...), rather
than the VarSet-based flavors (tyCoVarsOf...), despite the performance
considerations outlined in Note [Free variables of types].

This is because FV includes the InterestingVarFun, which is useful here,
because we can cleverly use it to restrict our calculations to CoVars - this
is what getCoVarSet achieves.

See #14880.



Note [When does a tycon application need an explicit kind signature?]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
There are a couple of places in GHC where we convert Core Types into forms that
more closely resemble user-written syntax. These include:

1. Template Haskell Type reification (see, for instance, TcSplice.reify_tc_app)
2. Converting Types to LHsTypes (in HsUtils.typeToLHsType, or in Haddock)

This conversion presents a challenge: how do we ensure that the resulting type
has enough kind information so as not to be ambiguous? To better motivate this
question, consider the following Core type:

  -- Foo :: Type -> Type
  type Foo = Proxy Type

There is nothing ambiguous about the RHS of Foo in Core. But if we were to,
say, reify it into a TH Type, then it's tempting to just drop the invisible
Type argument and simply return `Proxy`. But now we've lost crucial kind
information: we don't know if we're dealing with `Proxy Type` or `Proxy Bool`
or `Proxy Int` or something else! We've inadvertently introduced ambiguity.

Unlike in other situations in GHC, we can't just turn on
-fprint-explicit-kinds, as we need to produce something which has the same
structure as a source-syntax type. Moreover, we can't rely on visible kind
application, since the first kind argument to Proxy is inferred, not specified.
Our solution is to annotate certain tycons with their kinds whenever they
appear in applied form in order to resolve the ambiguity. For instance, we
would reify the RHS of Foo like so:

  type Foo = (Proxy :: Type -> Type)

We need to devise an algorithm that determines precisely which tycons need
these explicit kind signatures. We certainly don't want to annotate _every_
tycon with a kind signature, or else we might end up with horribly bloated
types like the following:

  (Either :: Type -> Type -> Type) (Int :: Type) (Char :: Type)

We only want to annotate tycons that absolutely require kind signatures in
order to resolve some sort of ambiguity, and nothing more.

Suppose we have a tycon application (T ty_1 ... ty_n). Why might this type
require a kind signature? It might require it when we need to fill in any of
T's omitted arguments. By "omitted argument", we mean one that is dropped when
reifying ty_1 ... ty_n. Sometimes, the omitted arguments are inferred and
specified arguments (e.g., TH reification in TcSplice), and sometimes the
omitted arguments are only the inferred ones (e.g., in HsUtils.typeToLHsType,
which reifies specified arguments through visible kind application).
Regardless, the key idea is that _some_ arguments are going to be omitted after
reification, and the only mechanism we have at our disposal for filling them in
is through explicit kind signatures.

What do we mean by "fill in"? Let's consider this small example:

  T :: forall {k}. Type -> (k -> Type) -> k

Moreover, we have this application of T:

  T @{j} Int aty

When we reify this type, we omit the inferred argument @{j}. Is it fixed by the
other (non-inferred) arguments? Yes! If we know the kind of (aty :: blah), then
we'll generate an equality constraint (kappa -> Type) and, assuming we can
solve it, that will fix `kappa`. (Here, `kappa` is the unification variable
that we instantiate `k` with.)

Therefore, for any application of a tycon T to some arguments, the Question We
Must Answer is:

* Given the first n arguments of T, do the kinds of the non-omitted arguments
  fill in the omitted arguments?

(This is still a bit hand-wavey, but we'll refine this question incrementally
as we explain more of the machinery underlying this process.)

Answering this question is precisely the role that the `injectiveVarsOfType`
and `injective_vars_of_binder` functions exist to serve. If an omitted argument
`a` appears in the set returned by `injectiveVarsOfType ty`, then knowing
`ty` determines (i.e., fills in) `a`. (More on `injective_vars_of_binder` in a
bit.)

More formally, if
`a` is in `injectiveVarsOfType ty`
and  S1(ty) ~ S2(ty),
then S1(a)  ~ S2(a),
where S1 and S2 are arbitrary substitutions.

For example, is `F` is a non-injective type family, then

  injectiveVarsOfType(Either c (Maybe (a, F b c))) = {a, c}

Now that we know what this function does, here is a second attempt at the
Question We Must Answer:

* Given the first n arguments of T (ty_1 ... ty_n), consider the binders
  of T that are instantiated by non-omitted arguments. Do the injective
  variables of these binders fill in the remainder of T's kind?

Alright, we're getting closer. Next, we need to clarify what the injective
variables of a tycon binder are. This the role that the
`injective_vars_of_binder` function serves. Here is what this function does for
each form of tycon binder:

* Anonymous binders are injective positions. For example, in the promoted data
  constructor '(:):

    '(:) :: forall a. a -> [a] -> [a]

  The second and third tyvar binders (of kinds `a` and `[a]`) are both
  anonymous, so if we had '(:) 'True '[], then the kinds of 'True and
  '[] would contribute to the kind of '(:) 'True '[]. Therefore,
  injective_vars_of_binder(_ :: a) = injectiveVarsOfType(a) = {a}.
  (Similarly, injective_vars_of_binder(_ :: [a]) = {a}.)
* Named binders:
  - Inferred binders are never injective positions. For example, in this data
    type:

      data Proxy a
      Proxy :: forall {k}. k -> Type

    If we had Proxy 'True, then the kind of 'True would not contribute to the
    kind of Proxy 'True. Therefore,
    injective_vars_of_binder(forall {k}. ...) = {}.
  - Required binders are injective positions. For example, in this data type:

      data Wurble k (a :: k) :: k
      Wurble :: forall k -> k -> k

  The first tyvar binder (of kind `forall k`) has required visibility, so if
  we had Wurble (Maybe a) Nothing, then the kind of Maybe a would
  contribute to the kind of Wurble (Maybe a) Nothing. Hence,
  injective_vars_of_binder(forall a -> ...) = {a}.
  - Specified binders /might/ be injective positions, depending on how you
    approach things. Continuing the '(:) example:

      '(:) :: forall a. a -> [a] -> [a]

    Normally, the (forall a. ...) tyvar binder wouldn't contribute to the kind
    of '(:) 'True '[], since it's not explicitly instantiated by the user. But
    if visible kind application is enabled, then this is possible, since the
    user can write '(:) @Bool 'True '[]. (In that case,
    injective_vars_of_binder(forall a. ...) = {a}.)

    There are some situations where using visible kind application is appropriate
    (e.g., HsUtils.typeToLHsType) and others where it is not (e.g., TH
    reification), so the `injective_vars_of_binder` function is parametrized by
    a Bool which decides if specified binders should be counted towards
    injective positions or not.

Now that we've defined injective_vars_of_binder, we can refine the Question We
Must Answer once more:

* Given the first n arguments of T (ty_1 ... ty_n), consider the binders
  of T that are instantiated by non-omitted arguments. For each such binder
  b_i, take the union of all injective_vars_of_binder(b_i). Is this set a
  superset of the free variables of the remainder of T's kind?

If the answer to this question is "no", then (T ty_1 ... ty_n) needs an
explicit kind signature, since T's kind has kind variables leftover that
aren't fixed by the non-omitted arguments.

One last sticking point: what does "the remainder of T's kind" mean? You might
be tempted to think that it corresponds to all of the arguments in the kind of
T that would normally be instantiated by omitted arguments. But this isn't
quite right, strictly speaking. Consider the following (silly) example:

  S :: forall {k}. Type -> Type

And suppose we have this application of S:

  S Int Bool

The Int argument would be omitted, and
injective_vars_of_binder(_ :: Type) = {}. This is not a superset of {k}, which
might suggest that (S Bool) needs an explicit kind signature. But
(S Bool :: Type) doesn't actually fix `k`! This is because the kind signature
only affects the /result/ of the application, not all of the individual
arguments. So adding a kind signature here won't make a difference. Therefore,
the fourth (and final) iteration of the Question We Must Answer is:

* Given the first n arguments of T (ty_1 ... ty_n), consider the binders
  of T that are instantiated by non-omitted arguments. For each such binder
  b_i, take the union of all injective_vars_of_binder(b_i). Is this set a
  superset of the free variables of the kind of (T ty_1 ... ty_n)?

Phew, that was a lot of work!

How can be sure that this is correct? That is, how can we be sure that in the
event that we leave off a kind annotation, that one could infer the kind of the
tycon application from its arguments? It's essentially a proof by induction: if
we can infer the kinds of every subtree of a type, then the whole tycon
application will have an inferrable kind--unless, of course, the remainder of
the tycon application's kind has uninstantiated kind variables.

What happens if T is oversaturated? That is, if T's kind has fewer than n
arguments, in the case that the concrete application instantiates a result
kind variable with an arrow kind? If we run out of arguments, we do not attach
a kind annotation. This should be a rare case, indeed. Here is an example:

   data T1 :: k1 -> k2 -> *
   data T2 :: k1 -> k2 -> *

   type family G (a :: k) :: k
   type instance G T1 = T2

   type instance F Char = (G T1 Bool :: (* -> *) -> *)   -- F from above

Here G's kind is (forall k. k -> k), and the desugared RHS of that last
instance of F is (G (* -> (* -> *) -> *) (T1 * (* -> *)) Bool). According to
the algorithm above, there are 3 arguments to G so we should peel off 3
arguments in G's kind. But G's kind has only two arguments. This is the
rare special case, and we choose not to annotate the application of G with
a kind signature. After all, we needn't do this, since that instance would
be reified as:

   type instance F Char = G (T1 :: * -> (* -> *) -> *) Bool

So the kind of G isn't ambiguous anymore due to the explicit kind annotation
on its argument. See #8953 and test th/T8953.
----------- No free vars -----------------


Note [The substitution invariant]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When calling (substTy subst ty) it should be the case that
the in-scope set in the substitution is a superset of both:

  (SIa) The free vars of the range of the substitution
  (SIb) The free vars of ty minus the domain of the substitution

The same rules apply to other substitutions (notably CoreSubst.Subst)

* Reason for (SIa). Consider
      substTy [a :-> Maybe b] (forall b. b->a)
  we must rename the forall b, to get
      forall b2. b2 -> Maybe b
  Making 'b' part of the in-scope set forces this renaming to
  take place.

* Reason for (SIb). Consider
     substTy [a :-> Maybe b] (forall b. (a,b,x))
  Then if we use the in-scope set {b}, satisfying (SIa), there is
  a danger we will rename the forall'd variable to 'x' by mistake,
  getting this:
      forall x. (Maybe b, x, x)
  Breaking (SIb) caused the bug from #11371.

Note: if the free vars of the range of the substitution are freshly created,
then the problems of (SIa) can't happen, and so it would be sound to
ignore (SIa).



Note [Substitutions apply only once]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We use TCvSubsts to instantiate things, and we might instantiate
        forall a b. ty
with the types
        [a, b], or [b, a].
So the substitution might go [a->b, b->a].  A similar situation arises in Core
when we find a beta redex like
        (/\ a /\ b -> e) b a
Then we also end up with a substitution that permutes type variables. Other
variations happen to; for example [a -> (a, b)].

        ********************************************************
        *** So a substitution must be applied precisely once ***
        ********************************************************

A TCvSubst is not idempotent, but, unlike the non-idempotent substitution
we use during unifications, it must not be repeatedly applied.



Note [Extending the TvSubstEnv]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
See #tcvsubst_invariant# for the invariants that must hold.

This invariant allows a short-cut when the subst envs are empty:
if the TvSubstEnv and CvSubstEnv are empty --- i.e. (isEmptyTCvSubst subst)
holds --- then (substTy subst ty) does nothing.

For example, consider:
        (/\a. /\b:(a~Int). ...b..) Int
We substitute Int for 'a'.  The Unique of 'b' does not change, but
nevertheless we add 'b' to the TvSubstEnv, because b's kind does change

This invariant has several crucial consequences:

* In substVarBndr, we need extend the TvSubstEnv
        - if the unique has changed
        - or if the kind has changed

* In substTyVar, we do not need to consult the in-scope set;
  the TvSubstEnv is enough

* In substTy, substTheta, we can short-circuit when the TvSubstEnv is empty



Note [Substituting types and coercions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Types and coercions are mutually recursive, and either may have variables
"belonging" to the other. Thus, every time we wish to substitute in a
type, we may also need to substitute in a coercion, and vice versa.
However, the constructor used to create type variables is distinct from
that of coercion variables, so we carry two VarEnvs in a TCvSubst. Note
that it would be possible to use the CoercionTy constructor to combine
these environments, but that seems like a false economy.

Note that the TvSubstEnv should *never* map a CoVar (built with the Id
constructor) and the CvSubstEnv should *never* map a TyVar. Furthermore,
the range of the TvSubstEnv should *never* include a type headed with
CoercionTy.


Note [Sym and ForAllCo]
~~~~~~~~~~~~~~~~~~~~~~~
In OptCoercion, we try to push "sym" out to the leaves of a coercion. But,
how do we push sym into a ForAllCo? It's a little ugly.

Here is the typing rule:

h : k1 ~# k2
(tv : k1) |- g : ty1 ~# ty2
----------------------------
ForAllCo tv h g : (ForAllTy (tv : k1) ty1) ~#
                  (ForAllTy (tv : k2) (ty2[tv |-> tv |> sym h]))

Here is what we want:

ForAllCo tv h' g' : (ForAllTy (tv : k2) (ty2[tv |-> tv |> sym h])) ~#
                    (ForAllTy (tv : k1) ty1)


Because the kinds of the type variables to the right of the colon are the kinds
coerced by h', we know (h' : k2 ~# k1). Thus, (h' = sym h).

Now, we can rewrite ty1 to be (ty1[tv |-> tv |> sym h' |> h']). We thus want

ForAllCo tv h' g' :
  (ForAllTy (tv : k2) (ty2[tv |-> tv |> h'])) ~#
  (ForAllTy (tv : k1) (ty1[tv |-> tv |> h'][tv |-> tv |> sym h']))

We thus see that we want

g' : ty2[tv |-> tv |> h'] ~# ty1[tv |-> tv |> h']

and thus g' = sym (g[tv |-> tv |> h']).

Putting it all together, we get this:

sym (ForAllCo tv h g)
==>
ForAllCo tv (sym h) (sym g[tv |-> tv |> sym h])



Note [Substituting in a coercion hole]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It seems highly suspicious to be substituting in a coercion that still
has coercion holes. Yet, this can happen in a situation like this:

  f :: forall k. k :~: Type -> ()
  f Refl = let x :: forall (a :: k). [a] -> ...
               x = ...

When we check x's type signature, we require that k ~ Type. We indeed
know this due to the Refl pattern match, but the eager unifier can't
make use of givens. So, when we're done looking at x's type, a coercion
hole will remain. Then, when we're checking x's definition, we skolemise
x's type (in order to, e.g., bring the scoped type variable `a` into scope).
This requires performing a substitution for the fresh skolem variables.

This subsitution needs to affect the kind of the coercion hole, too --
otherwise, the kind will have an out-of-scope variable in it. More problematically
in practice (we won't actually notice the out-of-scope variable ever), skolems
in the kind might have too high a level, triggering a failure to uphold the
invariant that no free variables in a type have a higher level than the
ambient level in the type checker. In the event of having free variables in the
hole's kind, I'm pretty sure we'll always have an erroneous program, so we
don't need to worry what will happen when the hole gets filled in. After all,
a hole relating a locally-bound type variable will be unable to be solved. This
is why it's OK not to look through the IORef of a coercion hole during
substitution.



Note [When to print foralls]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Mostly we want to print top-level foralls when (and only when) the user specifies
-fprint-explicit-foralls.  But when kind polymorphism is at work, that suppresses
too much information; see #9018.

So I'm trying out this rule: print explicit foralls if
  a) User specifies -fprint-explicit-foralls, or
  b) Any of the quantified type variables has a kind
     that mentions a kind variable

This catches common situations, such as a type siguature
     f :: m a
which means
      f :: forall k. forall (m :: k->*) (a :: k). m a
We really want to see both the "forall k" and the kind signatures
on m and a.  The latter comes from pprTCvBndr.



Note [Infix type variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
With TypeOperators you can say

   f :: (a ~> b) -> b

and the (~>) is considered a type variable.  However, the type
pretty-printer in this module will just see (a ~> b) as

   App (App (TyVarTy "~>") (TyVarTy "a")) (TyVarTy "b")

So it'll print the type in prefix form.  To avoid confusion we must
remember to parenthesise the operator, thus

   (~>) a b -> b

See #2766.

