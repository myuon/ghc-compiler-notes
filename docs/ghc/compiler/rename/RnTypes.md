[[src]](https://github.com/ghc/ghc/tree/master/compiler/rename/RnTypes.hs)

(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

# Main pass of renamer


These type renamers are in a separate module, rather than in (say) RnSource,
to break several loop.

# HsSigWcType (i.e with wildcards)


### Note: Renaming named wild cards

### Note: The wildcard story for types

It's easy!  When we collect the implicitly bound type variables, ready
to bring them into scope, and NamedWildCards is on, we partition the
variables into the ones that start with an underscore (the named
wildcards) and the rest. Then we just add them to the hswc_wcs field
of the HsWildCardBndrs structure, and we are done.

# HsSigtype (i.e. no wildcards)


# LHsType and HsType



rnHsType is here because we call it from loadInstDecl, and I didn't
want a gratuitous knot.

### Note: Context quantification

data A = A (a -> a)           is an error
data A = A (Eq a => a -> a)   binds "a"
data A = A (Eq a => a -> b)   binds "a" and "b"
data A = A (() => a -> b)     binds "a" and "b"
f :: forall a. a -> b         is an error
f :: forall a. () => a -> b   is an error
f :: forall a. a -> (() => b) binds "a" and "b"

This situation is now considered to be an error. See rnHsTyKi for case
HsForAllTy Qualified.

### Note: Dealing with *

As a legacy from the days when types and kinds were different, we use
the type * to mean what we now call GHC.Types.Type. The problem is that
* should associate just like an identifier, *not* a symbol.
Running example: the user has written

  T (Int, Bool) b + c * d

At this point, we have a bunch of stretches of types

  [[T, (Int, Bool), b], [c], [d]]

these are the [[LHsType Name]] and a bunch of operators

  [GHC.TypeLits.+, GHC.Types.*]

Note that the * is GHC.Types.*. So, we want to rearrange to have

  [[T, (Int, Bool), b], [c, *, d]]

and

  [GHC.TypeLits.+]

as our lists. We can then do normal fixity resolution on these. The fixities
must come along for the ride just so that the list stays in sync with the
operators.

### Note: QualTy in kinds

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


# Binding type variables


### Note: bindHsQTyVars examples

Suppose we have
   data T k (a::k1) (b::k) :: k2 -> k1 -> *

Then:
  hs_tv_bndrs = [k, a::k1, b::k], the explicitly-bound variables
  bndrs       = [k,a,b]

  bndr_kv_occs = [k,k1], kind variables free in kind signatures
                         of hs_tv_bndrs

  body_kv_occs = [k2,k1], kind variables free in the
                          result kind signature

  implicit_bndr_kvs = [k1], kind variables free in kind signatures
                            of hs_tv_bndrs, and not bound by bndrs

  implicit_body_kvs = [k2], kind variables free in the result kind
                            signature, and not bound either by
                            bndrs or by implicit_bndr_kvs

* We want to quantify add implicit bindings for
  implicit_bndr_kvs and implicit_body_kvs

* The "dependent" bndrs (hsq_dependent) are the subset of
  bndrs that are free in bndr_kv_occs or body_kv_occs

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

### Note: Kind variable scoping

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
we get a complaint the the second 'a' is not in scope.

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


# ConDeclField


When renaming a ConDeclField, we have to find the FieldLabel
associated with each field.  But we already have all the FieldLabels
available (since they were brought into scope by
RnNames.getLocalNonValBinders), so we just take the list as an
argument, build a map and look them up.


# Fixities and precedence parsing


@mkOpAppRn@ deals with operator fixities.  The argument expressions
are assumed to be already correctly arranged.  It needs the fixities
recorded in the OpApp nodes, because fixity info applies to the things
the programmer actually wrote, so you can't find it out from the Name.

Furthermore, the second argument is guaranteed not to be another
operator application.  Why? Because the parser parses all
operator applications left-associatively, EXCEPT negation, which
we need to handle specially.
Infix types are read in a *right-associative* way, so that
        a `op` b `op` c
is always read in as
        a `op` (b `op` c)

mkHsOpTyRn rearranges where necessary.  The two arguments
have already been renamed and rearranged.  It's made rather tiresome
by the presence of ->, which is a separate syntactic construct.


# Errors


# Finding the free type variables of a (HsType RdrName)


### Note: Kind and type-variable binders

In a type signature we may implicitly bind type variable and, more
recently, kind variables.  For example:
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

In general we want to walk over a type, and find
  * Its free type variables
  * The free kind variables of any kind signatures in the type

### Note: HsBSig binder lists

Most clients of this code just want to know the kind/type vars, without
duplicates. The function rmDupsInRdrTyVars removes duplicates. That function
also makes sure that no variable is reported as both a kind var and
a type var, preferring kind vars. Why kind vars? Consider this:

 foo :: forall (a :: k). Proxy k -> Proxy a -> ...

Should that be accepted?

Normally, if a type signature has an explicit forall, it must list *all*
tyvars mentioned in the type. But there's an exception for tyvars mentioned in
a kind, as k is above. Note that k is also used "as a type variable", as the
argument to the first Proxy. So, do we consider k to be type-variable-like and
require it in the forall? Or do we consider k to be kind-variable-like and not
require it?

It's not just in type signatures: kind variables are implicitly brought into
scope in a variety of places. Should vars used at both the type level and kind
level be treated this way?

GHC indeed allows kind variables to be brought into scope implicitly even when
the kind variable is also used as a type variable. Thus, we must prefer to keep
a variable listed as a kind var in rmDupsInRdrTyVars. If we kept it as a type
var, then this would prevent it from being implicitly quantified (see
rnImplicitBndrs). In the `foo` example above, that would have the consequence
of the k in Proxy k being reported as out of scope.

