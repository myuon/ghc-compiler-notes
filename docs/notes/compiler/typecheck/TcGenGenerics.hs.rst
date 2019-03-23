`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcGenGenerics.hs>`_

Note [Requirements for deriving Generic and Rep]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In the following, T, Tfun, and Targ are "meta-variables" ranging over type
expressions.

(Generic T) and (Rep T) are derivable for some type expression T if the
following constraints are satisfied.

  (a) D is a type constructor *value*. In other words, D is either a type
      constructor or it is equivalent to the head of a data family instance (up to
      alpha-renaming).

  (b) D cannot have a "stupid context".

  (c) The right-hand side of D cannot include existential types, universally
      quantified types, or "exotic" unlifted types. An exotic unlifted type
      is one which is not listed in the definition of allowedUnliftedTy
      (i.e., one for which we have no representation type).
      See Note [Generics and unlifted types]

  (d) T :: *.

(Generic1 T) and (Rep1 T) are derivable for some type expression T if the
following constraints are satisfied.

  (a),(b),(c) As above.

  (d) T must expect arguments, and its last parameter must have kind *.

      We use `a' to denote the parameter of D that corresponds to the last
      parameter of T.

  (e) For any type-level application (Tfun Targ) in the right-hand side of D
      where the head of Tfun is not a tuple constructor:

      (b1) `a' must not occur in Tfun.

      (b2) If `a' occurs in Targ, then Tfun :: * -> *.



Note [degenerate use of FFoldType]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We use foldDataConArgs here only for its ability to treat tuples
specially. foldDataConArgs also tracks covariance (though it assumes all
higher-order type parameters are covariant) and has hooks for special handling
of functions and polytypes, but we do *not* use those.

The key issue is that Generic1 deriving currently offers no sophisticated
support for functions. For example, we cannot handle

  data F a = F ((a -> Int) -> Int)

even though a is occurring covariantly.

In fact, our rule is harsh: a is simply not allowed to occur within the first
argument of (->). We treat (->) the same as any other non-tuple tycon.

Unfortunately, this means we have to track "the parameter occurs in this type"
explicitly, even though foldDataConArgs is also doing this internally.

canDoGenerics1 determines if a Generic1/Rep1 can be derived.

Checks (a) through (c) from Note [Requirements for deriving Generic and Rep]
are taken care of by the call to canDoGenerics.

It returns IsValid if deriving is possible. It returns (NotValid reason)
if not.


Note [Generics and unlifted types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Normally, all constants are marked with K1/Rec0. The exception to this rule is
when a data constructor has an unlifted argument (e.g., Int#, Char#, etc.). In
that case, we must use a data family instance of URec (from GHC.Generics) to
mark it. As a result, before we can generate K1 or unK1, we must first check
to see if the type is actually one of the unlifted types for which URec has a
data family instance; if so, we generate that instead.

See wiki:Commentary/Compiler/GenericDeriving#Handlingunliftedtypes for more
details on why URec is implemented the way it is.



Note [Generating a correctly typed Rep instance]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tc_mkRepTy derives the RHS of the Rep(1) type family instance when deriving
Generic(1). That is, it derives the ellipsis in the following:

    instance Generic Foo where
      type Rep Foo = ...

However, tc_mkRepTy only has knowledge of the *TyCon* of the type for which
a Generic(1) instance is being derived, not the fully instantiated type. As a
result, tc_mkRepTy builds the most generalized Rep(1) instance possible using
the type variables it learns from the TyCon (i.e., it uses tyConTyVars). This
can cause problems when the instance has instantiated type variables
(see #11732). As an example:

    data T a = MkT a
    deriving instance Generic (T Int)
    ==>
    instance Generic (T Int) where
      type Rep (T Int) = (... (Rec0 a)) -- wrong!

-XStandaloneDeriving is one way for the type variables to become instantiated.
Another way is when Generic1 is being derived for a datatype with a visible
kind binder, e.g.,

   data P k (a :: k) = MkP k deriving Generic1
   ==>
   instance Generic1 (P *) where
     type Rep1 (P *) = (... (Rec0 k)) -- wrong!

See Note [Unify kinds in deriving] in TcDeriv.

In any such scenario, we must prevent a discrepancy between the LHS and RHS of
a Rep(1) instance. To do so, we create a type variable substitution that maps
the tyConTyVars of the TyCon to their counterparts in the fully instantiated
type. (For example, using T above as example, you'd map a :-> Int.) We then
apply the substitution to the RHS before generating the instance.

A wrinkle in all of this: when forming the type variable substitution for
Generic1 instances, we map the last type variable of the tycon to Any. Why?
It's because of wily data types like this one (#15012):

   data T a = MkT (FakeOut a)
   type FakeOut a = Int

If we ignore a, then we'll produce the following Rep1 instance:

   instance Generic1 T where
     type Rep1 T = ... (Rec0 (FakeOut a))
     ...

Oh no! Now we have `a` on the RHS, but it's completely unbound. Instead, we
ensure that `a` is mapped to Any:

   instance Generic1 T where
     type Rep1 T = ... (Rec0 (FakeOut Any))
     ...

And now all is good.

Alternatively, we could have avoided this problem by expanding all type
synonyms on the RHSes of Rep1 instances. But we might blow up the size of
these types even further by doing this, so we choose not to do so.



Note [Handling kinds in a Rep instance]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Because Generic1 is poly-kinded, the representation types were generalized to
be kind-polymorphic as well. As a result, tc_mkRepTy must explicitly apply
the kind of the instance being derived to all the representation type
constructors. For instance, if you have

    data Empty (a :: k) = Empty deriving Generic1

Then the generated code is now approximately (with -fprint-explicit-kinds
syntax):

    instance Generic1 k (Empty k) where
      type Rep1 k (Empty k) = U1 k

Most representation types have only one kind variable, making them easy to deal
with. The only non-trivial case is (:.:), which is only used in Generic1
instances:

    newtype (:.:) (f :: k2 -> *) (g :: k1 -> k2) (p :: k1) =
        Comp1 { unComp1 :: f (g p) }

Here, we do something a bit counter-intuitive: we make k1 be the kind of the
instance being derived, and we always make k2 be *. Why *? It's because
the code that GHC generates using (:.:) is always of the form x :.: Rec1 y
for some types x and y. In other words, the second type to which (:.:) is
applied always has kind k -> *, for some kind k, so k2 cannot possibly be
anything other than * in a generated Generic1 instance.



Note [Generics compilation speed tricks]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Deriving Generic(1) is known to have a large constant factor during
compilation, which contributes to noticeable compilation slowdowns when
deriving Generic(1) for large datatypes (see #5642).

To ease the pain, there is a trick one can play when generating definitions for
to(1) and from(1). If you have a datatype like:

  data Letter = A | B | C | D

then a naïve Generic instance for Letter would be:

  instance Generic Letter where
    type Rep Letter = D1 ('MetaData ...) ...

    to (M1 (L1 (L1 (M1 U1)))) = A
    to (M1 (L1 (R1 (M1 U1)))) = B
    to (M1 (R1 (L1 (M1 U1)))) = C
    to (M1 (R1 (R1 (M1 U1)))) = D

    from A = M1 (L1 (L1 (M1 U1)))
    from B = M1 (L1 (R1 (M1 U1)))
    from C = M1 (R1 (L1 (M1 U1)))
    from D = M1 (R1 (R1 (M1 U1)))

Notice that in every LHS pattern-match of the 'to' definition, and in every RHS
expression in the 'from' definition, the topmost constructor is M1. This
corresponds to the datatype-specific metadata (the D1 in the Rep Letter
instance). But this is wasteful from a typechecking perspective, since this
definition requires GHC to typecheck an application of M1 in every single case,
leading to an O(n) increase in the number of coercions the typechecker has to
solve, which in turn increases allocations and degrades compilation speed.

Luckily, since the topmost M1 has the exact same type across every case, we can
factor it out reduce the typechecker's burden:

  instance Generic Letter where
    type Rep Letter = D1 ('MetaData ...) ...

    to (M1 x) = case x of
      L1 (L1 (M1 U1)) -> A
      L1 (R1 (M1 U1)) -> B
      R1 (L1 (M1 U1)) -> C
      R1 (R1 (M1 U1)) -> D

    from x = M1 (case x of
      A -> L1 (L1 (M1 U1))
      B -> L1 (R1 (M1 U1))
      C -> R1 (L1 (M1 U1))
      D -> R1 (R1 (M1 U1)))

A simple change, but one that pays off, since it goes turns an O(n) amount of
coercions to an O(1) amount.

