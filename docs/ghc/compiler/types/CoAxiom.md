[[src]](https://github.com/ghc/ghc/tree/master/compiler/types/CoAxiom.hs)
# LANGUAGE CPP, DataKinds, DeriveDataTypeable, GADTs, KindSignatures,
             ScopedTypeVariables, StandaloneDeriving, RoleAnnotations #

### Note: Coercion axiom branches

In order to allow closed type families, an axiom needs to contain an
ordered list of alternatives, called branches. The kind of the coercion built
from an axiom is determined by which index is used when building the coercion
from the axiom.

For example, consider the axiom derived from the following declaration:

type family F a where
  F [Int] = Bool
  F [a]   = Double
  F (a b) = Char

This will give rise to this axiom:

axF :: {                                         F [Int] ~ Bool
       ; forall (a :: *).                        F [a]   ~ Double
       ; forall (k :: *) (a :: k -> *) (b :: k). F (a b) ~ Char
       }

The axiom is used with the AxiomInstCo constructor of Coercion. If we wish
to have a coercion showing that F (Maybe Int) ~ Char, it will look like

axF[2] <*> <Maybe> <Int> :: F (Maybe Int) ~ Char
-- or, written using concrete-ish syntax --
AxiomInstCo axF 2 [Refl *, Refl Maybe, Refl Int]

Note that the index is 0-based.

For type-checking, it is also necessary to check that no previous pattern
can unify with the supplied arguments. After all, it is possible that some
of the type arguments are lambda-bound type variables whose instantiation may
cause an earlier match among the branches. We wish to prohibit this behavior,
so the type checker rules out the choice of a branch where a previous branch
can unify. See also [Apartness] in FamInstEnv.hs.

For example, the following is malformed, where 'a' is a lambda-bound type
variable:

axF[2] <*> <a> <Bool> :: F (a Bool) ~ Char

Why? Because a might be instantiated with [], meaning that branch 1 should
apply, not branch 2. This is a vital consistency check; without it, we could
derive Int ~ Bool, and that is a Bad Thing.

### Note: Branched axioms

Although a CoAxiom has the capacity to store many branches, in certain cases,
we want only one. These cases are in data/newtype family instances, newtype
coercions, and type family instances.
Furthermore, these unbranched axioms are used in a
variety of places throughout GHC, and it would difficult to generalize all of
that code to deal with branched axioms, especially when the code can be sure
of the fact that an axiom is indeed a singleton. At the same time, it seems
dangerous to assume singlehood in various places through GHC.

The solution to this is to label a CoAxiom with a phantom type variable
declaring whether it is known to be a singleton or not. The branches
are stored using a special datatype, declared below, that ensures that the
type variable is accurate.

# Branches


# Coercion axioms


### Note: Storing compatibility

### Note: Compatibility

Specifically, each branch refers to all other branches with which it is
incompatible. This list might well be empty, and it will always be for the
first branch of any axiom.

CoAxBranches that do not (yet) belong to a CoAxiom should have a panic thunk
stored in cab_incomps. The incompatibilities are properly a property of the
axiom as a whole, and they are computed only when the final axiom is built.

During serialization, the list is converted into a list of the indices
of the branches.


### Note: CoAxiom saturation

* When co

### Note: CoAxBranch type variables

In the case of a CoAxBranch of an associated type-family instance,
we use the *same* type variables (where possible) as the
enclosing class or instance.  Consider
   class C a b where
     type F x b
     type F [y] b = ...     -- Second param must be b

   instance C Int [z] where
     type F Int [z] = ...   -- Second param must be [z]

In the CoAxBranch in the instance decl (F Int [z]) we use the
same 'z', so that it's easy to check that that type is the same
as that in the instance header.

Similarly in the CoAxBranch for the default decl for F in the
class decl, we use the same 'b' to make the same check easy.

So, unlike FamInsts, there is no expectation that the cab_tvs
are fresh wrt each other, or any other CoAxBranch.

### Note: CoAxBranch roles

Consider this code:

  newtype Age = MkAge Int
  newtype Wrap a = MkWrap a

  convert :: Wrap Age -> Int
  convert (MkWrap (MkAge i)) = i

We want this to compile to:

  NTCo:Wrap :: forall a. Wrap a ~R a
  NTCo:Age  :: Age ~R Int
  convert = \x -> x |> (NTCo:Wrap[0] NTCo:Age[0])

But, note that NTCo:Age is at role R. Thus, we need to be able to pass
coercions at role R into axioms. However, we don't *always* want to be able to
do this, as it would be disastrous with type families. The solution is to
annotate the arguments to the axiom with roles, much like we annotate tycon
tyvars. Where do these roles get set? Newtype axioms inherit their roles from
the newtype tycon; family axioms are all at role N.

### Note: CoAxiom locations

The source location of a CoAxiom is stored in two places in the
datatype tree.
  * The first is in the location info buried in the Name of the
    CoAxiom. This span includes all of the branches of a branched
    CoAxiom.
  * The second is in the cab_loc fields of the CoAxBranches.

In the case of a single branch, we can extract the source location of
the branch from the name of the CoAxiom. In other cases, we need an
explicit SrcSpan to correctly store the location of the equation
giving rise to the FamInstBranch.

### Note: Implicit axioms

### Note: Implicit TyThings

* The CoAxiom arising from a newtype declaration *is* "implicit".
  That is, it does not have its own IfaceAxiom declaration in an
  interface file; instead the CoAxiom is generated by type-checking
  the newtype declaration


# Roles


Roles are defined here to avoid circular dependencies.


# CoAxiomRule
              Rules for building Evidence


Conditional axioms.  The general idea is that a `CoAxiomRule` looks like this:

    forall as. (r1 ~ r2, s1 ~ s2) => t1 ~ t2

My intention is to reuse these for both (~) and (~#).
The short-term plan is to use this datatype to represent the type-nat axioms.
In the longer run, it may be good to unify this and `CoAxiom`,
as `CoAxiom` is the special case when there are no assumptions.
