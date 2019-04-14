`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/types/CoAxiom.hs>`_

compiler/types/CoAxiom.hs
=========================


Note [Coercion axiom branches]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/types/CoAxiom.hs#L54>`__

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



Note [Branched axioms]
~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/types/CoAxiom.hs#L100>`__

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



Note [Storing compatibility]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/types/CoAxiom.hs#L182>`__

During axiom application, we need to be aware of which branches are compatible
with which others. The full explanation is in Note [Compatibility] in
FamInstEnv. (The code is placed there to avoid a dependency from CoAxiom on
the unification algorithm.) Although we could theoretically compute
compatibility on the fly, this is silly, so we store it in a CoAxiom.

Specifically, each branch refers to all other branches with which it is
incompatible. This list might well be empty, and it will always be for the
first branch of any axiom.

CoAxBranches that do not (yet) belong to a CoAxiom should have a panic thunk
stored in cab_incomps. The incompatibilities are properly a property of the
axiom as a whole, and they are computed only when the final axiom is built.

During serialization, the list is converted into a list of the indices
of the branches.



Note [CoAxiom saturation]
~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/types/CoAxiom.hs#L313>`__

* When co



Note [CoAxBranch type variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/types/CoAxiom.hs#L317>`__

In the case of a CoAxBranch of an associated type-family instance,
we use the *same* type variables (where possible) as the
enclosing class or instance.  Consider

::

  instance C Int [z] where
     type F Int [z] = ...   -- Second param must be [z]

..

In the CoAxBranch in the instance decl (F Int [z]) we use the
same 'z', so that it's easy to check that that type is the same
as that in the instance header.

So, unlike FamInsts, there is no expectation that the cab_tvs
are fresh wrt each other, or any other CoAxBranch.



Note [CoAxBranch roles]
~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/types/CoAxiom.hs#L333>`__

Consider this code:

::

  newtype Age = MkAge Int
  newtype Wrap a = MkWrap a

..

::

  convert :: Wrap Age -> Int
  convert (MkWrap (MkAge i)) = i

..

We want this to compile to:

::

  NTCo:Wrap :: forall a. Wrap a ~R a
  NTCo:Age  :: Age ~R Int
  convert = \x -> x |> (NTCo:Wrap[0] NTCo:Age[0])

..

But, note that NTCo:Age is at role R. Thus, we need to be able to pass
coercions at role R into axioms. However, we don't *always* want to be able to
do this, as it would be disastrous with type families. The solution is to
annotate the arguments to the axiom with roles, much like we annotate tycon
tyvars. Where do these roles get set? Newtype axioms inherit their roles from
the newtype tycon; family axioms are all at role N.



Note [CoAxiom locations]
~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/types/CoAxiom.hs#L356>`__

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



Note [Implicit axioms]
~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/types/CoAxiom.hs#L370>`__

See also Note [Implicit TyThings] in HscTypes
* A CoAxiom arising from data/type family instances is not "implicit".
  That is, it has its own IfaceAxiom declaration in an interface file

* The CoAxiom arising from a newtype declaration *is* "implicit".
  That is, it does not have its own IfaceAxiom declaration in an
  interface file; instead the CoAxiom is generated by type-checking
  the newtype declaration



Note [Eta reduction for data families]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/types/CoAxiom.hs#L381>`__

Consider this
   data family T a b :: *
   newtype instance T Int a = MkT (IO a) deriving( Monad )
We'd like this to work.

From the 'newtype instance' you might think we'd get:
   newtype TInt a = MkT (IO a)
   axiom ax1 a :: T Int a ~ TInt a   -- The newtype-instance part
   axiom ax2 a :: TInt a ~ IO a      -- The newtype part

But now what can we do?  We have this problem
   Given:   d  :: Monad IO
   Wanted:  d' :: Monad (T Int) = d |> ????
What coercion can we use for the ???

Solution: eta-reduce both axioms, thus:
   axiom ax1 :: T Int ~ TInt
   axiom ax2 :: TInt ~ IO
Now
   d' = d |> Monad (sym (ax2 ; ax1))

----- Bottom line ------

For a CoAxBranch for a data family instance with representation
TyCon rep_tc:

  - cab_tvs (of its CoAxiom) may be shorter
    than tyConTyVars of rep_tc.

  - cab_lhs may be shorter than tyConArity of the family tycon
       i.e. LHS is unsaturated

  - cab_rhs will be (rep_tc cab_tvs)
       i.e. RHS is un-saturated

  - This eta reduction happens for data instances as well
    as newtype instances. Here we want to eta-reduce the data family axiom.

  - This eta-reduction is done in TcInstDcls.tcDataFamInstDecl.

But for a /type/ family
  - cab_lhs has the exact arity of the family tycon

There are certain situations (e.g., pretty-printing) where it is necessary to
deal with eta-expanded data family instances. For these situations, the
cab_eta_tvs field records the stuff that has been eta-reduced away.
So if we have
    axiom forall a b. F [a->b] = D b a
and cab_eta_tvs is [p,q], then the original user-written definition
looked like
    axiom forall a b p q. F [a->b] p q = D b a p q
(See #9692, #14179, and #15845 for examples of what can go wrong if
we don't eta-expand when showing things to the user.)

(See also Note [Newtype eta] in TyCon.  This is notionally separate
and deals with the axiom connecting a newtype with its representation
type; but it too is eta-reduced.)

