`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/rename/RnTypes.hs>`_

Note [Renaming named wild cards]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Identifiers starting with an underscore are always parsed as type variables.
It is only here in the renamer that we give the special treatment.
See Note [The wildcard story for types] in HsTypes.

It's easy!  When we collect the implicitly bound type variables, ready
to bring them into scope, and NamedWildCards is on, we partition the
variables into the ones that start with an underscore (the named
wildcards) and the rest. Then we just add them to the hswc_wcs field
of the HsWildCardBndrs structure, and we are done.




Note [Context quantification]
-----------------------------
Variables in type signatures are implicitly quantified
when (1) they are in a type signature not beginning
with "forall" or (2) in any qualified type T => R.
We are phasing out (2) since it leads to inconsistencies
(#4426):

data A = A (a -> a)           is an error
data A = A (Eq a => a -> a)   binds "a"
data A = A (Eq a => a -> b)   binds "a" and "b"
data A = A (() => a -> b)     binds "a" and "b"
f :: forall a. a -> b         is an error
f :: forall a. () => a -> b   is an error
f :: forall a. a -> (() => b) binds "a" and "b"

This situation is now considered to be an error. See rnHsTyKi for case
HsForAllTy Qualified.



Note [QualTy in kinds]
~~~~~~~~~~~~~~~~~~~~~~
I was wondering whether QualTy could occur only at TypeLevel.  But no,
we can have a qualified type in a kind too. Here is an example:

  type family F a where
    F Bool = Nat
    F Nat  = Type

  type family G a where
    G Type = Type -> Type
    G ()   = Nat

  data X :: forall k1 k2. (F k1 ~ G k2) => k1 -> k2 -> Type where
    MkX :: X 'True '()

See that k1 becomes Bool and k2 becomes (), so the equality is
satisfied. If I write MkX :: X 'True 'False, compilation fails with a
suitable message:

  MkX :: X 'True '()
    • Couldn't match kind ‘G Bool’ with ‘Nat’
      Expected kind: G Bool
        Actual kind: F Bool

However: in a kind, the constraints in the QualTy must all be
equalities; or at least, any kinds with a class constraint are
uninhabited.


Note [bindHsQTyVars examples]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have
   data T k (a::k1) (b::k) :: k2 -> k1 -> *

Then:
  hs_tv_bndrs = [k, a::k1, b::k], the explicitly-bound variables
  bndrs       = [k,a,b]

  bndr_kv_occs = [k,k1], kind variables free in kind signatures
                         of hs_tv_bndrs

  body_kv_occs = [k2,k1], kind variables free in the
                          result kind signature

  implicit_kvs = [k1,k2], kind variables free in kind signatures
                          of hs_tv_bndrs, and not bound by bndrs

* We want to quantify add implicit bindings for implicit_kvs

* If implicit_body_kvs is non-empty, then there is a kind variable
  mentioned in the kind signature that is not bound "on the left".
  That's one of the rules for a CUSK, so we pass that info on
  as the second argument to thing_inside.

* Order is not important in these lists.  All we are doing is
  bring Names into scope.

Finally, you may wonder why filter_occs removes in-scope variables
from bndr/body_kv_occs.  How can anything be in scope?  Answer:
HsQTyVars is /also/ used (slightly oddly) for Haskell-98 syntax
ConDecls
   data T a = forall (b::k). MkT a b
The ConDecl has a LHsQTyVars in it; but 'a' scopes over the entire
ConDecl.  Hence the local RdrEnv may be non-empty and we must filter
out 'a' from the free vars.  (Mind you, in this situation all the
implicit kind variables are bound at the data type level, so there
are none to bind in the ConDecl, so there are no implicitly bound
variables at all.



Note [Kind variable scoping]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If we have
  data T (a :: k) k = ...
we report "k is out of scope" for (a::k).  Reason: k is not brought
into scope until the explicit k-binding that follows.  It would be
terribly confusing to bring into scope an /implicit/ k for a's kind
and a distinct, shadowing explicit k that follows, something like
  data T {k1} (a :: k1) k = ...

So the rule is:

   the implicit binders never include any
   of the explicit binders in the group

Note that in the denerate case
  data T (a :: a) = blah
we get a complaint the second 'a' is not in scope.

That applies to foralls too: e.g.
   forall (a :: k) k . blah

But if the foralls are split, we treat the two groups separately:
   forall (a :: k). forall k. blah
Here we bring into scope an implicit k, which is later shadowed
by the explicit k.

In implementation terms

* In bindHsQTyVars 'k' is free in bndr_kv_occs; then we delete
  the binders {a,k}, and so end with no implicit binders.  Then we
  rename the binders left-to-right, and hence see that 'k' is out of
  scope in the kind of 'a'.

* Similarly in extract_hs_tv_bndrs



Note [Variables used as both types and kinds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We bind the type variables tvs, and kvs is the set of free variables of the
kinds in the scope of the binding. Here is one typical example:

   forall a b. a -> (b::k) -> (c::a)

Here, tvs will be {a,b}, and kvs {k,a}.

We must make sure that kvs includes all of variables in the kinds of type
variable bindings. For instance:

   forall k (a :: k). Proxy a

If we only look in the body of the `forall` type, we will mistakenly conclude
that kvs is {}. But in fact, the type variable `k` is also used as a kind
variable in (a :: k), later in the binding. (This mistake lead to #14710.)
So tvs is {k,a} and kvs is {k}.

NB: we do this only at the binding site of 'tvs'.


Note [Kind and type-variable binders]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In a type signature we may implicitly bind type/kind variables. For example:
  *   f :: a -> a
      f = ...
    Here we need to find the free type variables of (a -> a),
    so that we know what to quantify

  *   class C (a :: k) where ...
    This binds 'k' in ..., as well as 'a'

  *   f (x :: a -> [a]) = ....
    Here we bind 'a' in ....

  *   f (x :: T a -> T (b :: k)) = ...
    Here we bind both 'a' and the kind variable 'k'

  *   type instance F (T (a :: Maybe k)) = ...a...k...
    Here we want to constrain the kind of 'a', and bind 'k'.

To do that, we need to walk over a type and find its free type/kind variables.
We preserve the left-to-right order of each variable occurrence.
See Note [Ordering of implicit variables].

Clients of this code can remove duplicates with nubL.



Note [Ordering of implicit variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Since the advent of -XTypeApplications, GHC makes promises about the ordering
of implicit variable quantification. Specifically, we offer that implicitly
quantified variables (such as those in const :: a -> b -> a, without a `forall`)
will occur in left-to-right order of first occurrence. Here are a few examples:

  const :: a -> b -> a       -- forall a b. ...
  f :: Eq a => b -> a -> a   -- forall a b. ...  contexts are included

  type a <-< b = b -> a
  g :: a <-< b               -- forall a b. ...  type synonyms matter

  class Functor f where
    fmap :: (a -> b) -> f a -> f b   -- forall f a b. ...
    -- The f is quantified by the class, so only a and b are considered in fmap

This simple story is complicated by the possibility of dependency: all variables
must come after any variables mentioned in their kinds.

  typeRep :: Typeable a => TypeRep (a :: k)   -- forall k a. ...

The k comes first because a depends on k, even though the k appears later than
the a in the code. Thus, GHC does ScopedSort on the variables.
See Note [ScopedSort] in Type.

Implicitly bound variables are collected by any function which returns a
FreeKiTyVars, FreeKiTyVarsWithDups, or FreeKiTyVarsNoDups, which notably
includes the `extract-` family of functions (extractHsTysRdrTyVarsDups,
extractHsTyVarBndrsKVs, etc.).
These functions thus promise to keep left-to-right ordering.



Note [Implicit quantification in type synonyms]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We typically bind type/kind variables implicitly when they are in a kind
annotation on the LHS, for example:

  data Proxy (a :: k) = Proxy
  type KindOf (a :: k) = k

Here 'k' is in the kind annotation of a type variable binding, KindedTyVar, and
we want to implicitly quantify over it.  This is easy: just extract all free
variables from the kind signature. That's what we do in extract_hs_tv_bndrs_kvs

By contrast, on the RHS we can't simply collect *all* free variables. Which of
the following are allowed?

  type TySyn1 = a :: Type
  type TySyn2 = 'Nothing :: Maybe a
  type TySyn3 = 'Just ('Nothing :: Maybe a)
  type TySyn4 = 'Left a :: Either Type a

After some design deliberations (see non-taken alternatives below), the answer
is to reject TySyn1 and TySyn3, but allow TySyn2 and TySyn4, at least for now.
We implicitly quantify over free variables of the outermost kind signature, if
one exists:

  * In TySyn1, the outermost kind signature is (:: Type), and it does not have
    any free variables.
  * In TySyn2, the outermost kind signature is (:: Maybe a), it contains a
    free variable 'a', which we implicitly quantify over.
  * In TySyn3, there is no outermost kind signature. The (:: Maybe a) signature
    is hidden inside 'Just.
  * In TySyn4, the outermost kind signature is (:: Either Type a), it contains
    a free variable 'a', which we implicitly quantify over. That is why we can
    also use it to the left of the double colon: 'Left a

The logic resides in extractHsTyRdrTyVarsKindVars. We use it both for type
synonyms and type family instances.

This is something of a stopgap solution until we can explicitly bind invisible
type/kind variables:

  type TySyn3 :: forall a. Maybe a
  type TySyn3 @a = 'Just ('Nothing :: Maybe a)



Note [Implicit quantification in type synonyms: non-taken alternatives]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Alternative I: No quantification
--------------------------------
We could offer no implicit quantification on the RHS, accepting none of the
TySyn<N> examples. The user would have to bind the variables explicitly:

  type TySyn1 a = a :: Type
  type TySyn2 a = 'Nothing :: Maybe a
  type TySyn3 a = 'Just ('Nothing :: Maybe a)
  type TySyn4 a = 'Left a :: Either Type a

However, this would mean that one would have to specify 'a' at call sites every
time, which could be undesired.

Alternative II: Indiscriminate quantification
---------------------------------------------
We could implicitly quantify over all free variables on the RHS just like we do
on the LHS. Then we would infer the following kinds:

  TySyn1 :: forall {a}. Type
  TySyn2 :: forall {a}. Maybe a
  TySyn3 :: forall {a}. Maybe (Maybe a)
  TySyn4 :: forall {a}. Either Type a

This would work fine for TySyn<2,3,4>, but TySyn1 is clearly bogus: the variable
is free-floating, not fixed by anything.

Alternative III: reportFloatingKvs
----------------------------------
We could augment Alternative II by hunting down free-floating variables during
type checking. While viable, this would mean we'd end up accepting this:

  data Prox k (a :: k)
  type T = Prox k

See Note [Kind and type-variable binders]
These lists are guaranteed to preserve left-to-right ordering of
the types the variables were extracted from. See also
Note [Ordering of implicit variables].

