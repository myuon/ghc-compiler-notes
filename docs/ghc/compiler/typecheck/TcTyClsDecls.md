[[src]](https://github.com/ghc/ghc/tree/master/compiler/typecheck/TcTyClsDecls.hs)

(c) The University of Glasgow 2006
(c) The AQUA Project, Glasgow University, 1996-1998


TcTyClsDecls: Typecheck type and class declarations


# \subsection{Type checking for type and class declarations}


### Note: Grouping of type and class declarations

tcTyAndClassDecls is called on a list of `TyClGroup`s. Each group is a strongly
connected component of mutually dependent types and classes. We kind check and
type check each group separately to enhance kind polymorphism. Take the
following example:

  type Id a = a
  data X = X (Id Int)

If we were to kind check the two declarations together, we would give Id the
kind * -> *, since we apply it to an Int in the definition of X. But we can do
better than that, since Id really is kind polymorphic, and should get kind
forall (k::*). k -> k. Since it does not depend on anything else, it can be
kind-checked by itself, hence getting the most general kind. We then kind check
X, which works fine because we then know the polymorphic kind of Id, and simply
instantiate k to *.

### Note: Check role annotations in a second pass

### Note: Checking GADT return types

# Kind checking


### Note: Kind checking for type and class decls

Kind checking is done thus:

   1. Make up a kind variable for each parameter of the declarations,
      and extend the kind environment (which is in the TcLclEnv)

   2. Kind check the declarations

We need to kind check all types in the mutually recursive group
before we know the kind of the type variables.  For example:

  class C a where
     op :: D b => a -> b -> b

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

# type families

This treatment of type synonyms only applies to Haskell 98-style synonyms.
General type functions can be recursive, and hence, appear in `alg_decls'.

The kind of an open type family is solely determinded by its kind signature;
hence, only kind signatures participate in the construction of the initial
kind environment (as constructed by `getInitialKind'). In fact, we ignore
instances of families altogether in the following. However, we need to include
the kinds of *associated* families into the construction of the initial kind
environment. (This is handled by `allDecls').

### Note: Kind checking recursive type and class declarations

### Note: How TcTyCons work

TcTyCons are used for two distinct purposes

1.  When recovering from a type error in a type declaration,
    we want to put the erroneous TyCon in the environment in a
    way that won't lead to more errors.  We use a TcTyCon for this;
    see makeRecoveryTyCon.

2.  When checking a type/class declaration (in module TcTyClsDecls), we come
    upon knowledge of the eventual tycon in bits and pieces.

      S1) First, we use getInitialKinds to look over the user-provided
          kind signature of a tycon (including, for example, the number
          of parameters written to the tycon) to get an initial shape of
          the tycon's kind.  We record that shape in a TcTyCon.

      S2) Then, using these initial kinds, we kind-check the body of the
          tycon (class methods, data constructors, etc.), filling in the
          metavariables in the tycon's initial kind.

      S3) We then generalize to get the tycon's final, fixed
          kind. Finally, once this has happened for all tycons in a
          mutually recursive group, we can desugar the lot.

    For convenience, we store partially-known tycons in TcTyCons, which
    might store meta-variables. These TcTyCons are stored in the local
    environment in TcTyClsDecls, until the real full TyCons can be created
    during desugaring. A desugared program should never have a TcTyCon.

    A challenging piece in all of this is that we end up taking three separate
    passes over every declaration:
      - one in getInitialKind (this pass look only at the head, not the body)
      - one in kcTyClDecls (to kind-check the body)
      - a final one in tcTyClDecls (to desugar)
    In the latter two passes, we need to connect the user-written type
    variables in an LHsQTyVars with the variables in the tycon's
    inferred kind. Because the tycon might not have a CUSK, this
    matching up is, in general, quite hard to do.  (Look through the
    git history between Dec 2015 and Apr 2016 for
    TcHsType.splitTelescopeTvs!) Instead of trying, we just store the
    list of type variables to bring into scope, in the
    tyConScopedTyVars field of the TcTyCon.  These tyvars are brought
    into scope in kcTyClTyVars and tcTyClTyVars, both in TcHsType.

    In a TcTyCon, everything is zonked after the kind-checking pass (S2).


### Note: Recursion and promoting data constructors

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

# \subsection{Type checking}


### Note: Type checking recursive type and class declarations

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

  *Global* env with T -> ATyCon (the (not yet built) final TyCon for T)
  *Local*  env with T -> ATcTyCon (TcTyCon with the polymorphic kind of T)

Then:

  * During TcHsType.kcTyVar we look in the *local* env, to get the
    known kind for T.

  * But in TcHsType.ds_type (and ds_var_app in particular) we look in
    the *global* env to get the TyCon. But we must be careful not to
    force the TyCon or we'll get a loop.

This fancy footwork (with two bindings for T) is only necessary for the
TyCons or Classes of this recursive group.  Earlier, finished groups,
live in the global env only.

### Note: Kind checking recursive type and class declarations

### Note: Kind checking recursive type and class declarations

Before we can type-check the decls, we must kind check them. This
is done by establishing an "initial kind", which is a rather uninformed
guess at a tycon's kind (by counting arguments, mainly) and then
using this initial kind for recursive occurrences.

The initial kind is stored in exactly the same way during kind-checking
as it is during type-checking (Note [Type checking recursive type and class
declarations]): in the *local* environment, with ATcTyCon. But we still
must store *something* in the *global* environment. Even though we
discard the result of kind-checking, we sometimes need to produce error
messages. These error messages will want to refer to the tycons being
checked, except that they don't exist yet, and it would be Terribly
Annoying to get the error messages to refer back to HsSyn. So we
create a TcTyCon and put it in the global env. This tycon can
print out its name and knows its kind,
but any other action taken on it will panic. Note
that TcTyCons are *not* knot-tied, unlike the rather valid but
knot-tied ones that occur during type-checking.

### Note: Declarations for wired-in things

For wired-in things we simply ignore the declaration
and take the wired-in information.  That avoids complications.
e.g. the need to make the data constructor worker name for
     a constraint tuple match the wired-in one


# Typechecking associated types (in class decls)
               (including the associated-type defaults)


### Note: Associated type defaults


The following is an example of associated type defaults:
             class C a where
               data D a

               type F a b :: *
               type F a b = [a]        -- Default

Note that we can get default definitions only for type families, not data
families.


### Note: Type-checking default assoc decls

Consider this default declaration for an associated type

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
message isn't great, mind you.  (Trac #11361 was caused by not doing a
proper tcMatchTys here.)  


Kind check type patterns and kind annotate the embedded type variables.
     type instance F [a] = rhs

 * Here we check that a type instance matches its kind signature, but we do
   not check whether there is a pattern for each type index; the latter
   check is only required for type synonym instances.

### Note: tc_fam_ty_pats vs tcFamTyPats

tc_fam_ty_pats does the type checking of the patterns, but it doesn't
zonk or generate any desugaring. It is used when kind-checking closed
type families.

tcFamTyPats type checks the patterns, zonks, and then calls thing_inside
to generate a desugaring. It is used during type-checking (not kind-checking).

### Note: Type-checking type patterns

When typechecking the patterns of a family instance declaration, we can't
rely on using the family TyCon itself, because this is sometimes called
from within a type-checking knot. (Specifically for closed type families.)
The TcTyCon gives just enough information to do the job.

### Note: tc_fam_ty_pats vs tcFamTyPats

### Note: Instantiating a family tycon

It's possible that kind-checking the result of a family tycon applied to
its patterns will instantiate the tycon further. For example, we might
have

  type family F :: k where
    F = Int
    F = Maybe

### Note: Arity of data families

So, the kind-checker must return both the new args (that is, Type
(Type -> Type) for the equations above) and the instantiated kind.

### Note: tc_fam_ty_pats vs tcFamTyPats

### Note: Failing early in kcDataDefn

We need to use checkNoErrs when calling kcConDecl. This is because kcConDecl
calls tcConDecl, which checks that the return type of a GADT-like constructor
is actually an instance of the type head. Without the checkNoErrs, potentially
two bad things could happen:

 1) Duplicate error messages, because tcConDecl will be called again during
    *type* checking (as opposed to kind checking)
 2) If we just keep blindly forging forward after both kind checking and type
    checking, we can get a panic in rejigConRes. See Trac #8368.


 TODO (RAE): This should be cleverer. Consider this:

                 type family F a

                 data G a where
                   MkG :: F a ~ Bool => G a

                 type family Foo (x :: G a) :: F a
                 type instance Foo MkG = False

### Note: Constraints in patterns

### Note: Constraints in patterns

NB: This isn't the whole story. See comment in tcFamTyPats.

At first glance, it seems there is a complicated story to tell in tcFamTyPats
around constraint solving. After all, type family patterns can now do
GADT pattern-matching, which is jolly complicated. But, there's a key fact
which makes this all simple: everything is at top level! There cannot
be untouchable type variables. There can't be weird interaction between
case branches. There can't be global skolems.

This means that the semantics of type-level GADT matching is a little
different than term level. If we have

  data G a where
    MkGBool :: G Bool

And then

  type family F (a :: G k) :: k
  type instance F MkGBool = True

we get

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

### Note: Quantifying over family patterns

We need to quantify over two different lots of kind variables:

First, the ones that come from the kinds of the tyvar args of
tcTyVarBndrsKindGen, as usual
  data family Dist a

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

### Note: Quantified kind variables of a family pattern

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



# Data types


 no eq_preds 

### Note: Infix GADT constructors

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

### Note: Checking GADT return types

There is a delicacy around checking the return types of a datacon. The
central problem is dealing with a declaration like

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
errors reported in one pass.  See Trac #7175, and #10836.


### Note: mkGADTVars

Running example:

data T (k1 :: *) (k2 :: *) (a :: k2) (b :: k2) where
  MkT :: forall (x1 : *) (y :: x1) (z :: *).
         T x1 * (Proxy (y :: x1), z) z

We need the rejigged type to be

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

### Note: Checking GADT return types

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


### Note: Substitution in template variables kinds


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

# Validity checking


Validity checking is done once the mutually-recursive knot has been
tied, so we can look at things freely.


### Note: Recover from validity error

We recover from a validity error in a type or class, which allows us
to report multiple validity errors. In the failure case we return a
TyCon of the right kind, but with no interesting behaviour
(makeRecoveryTyCon). Why?  Suppose we have
   type T a = Fun
where Fun is a type family of arity 1.  The RHS is invalid, but we
want to go on checking validity of subsequent type declarations.
So we replace T with an abstract TyCon which will do no harm.
See indexed-types/should_fail/BadSock and Trac #10896

Painfully, though, we *don't* want to do this for classes.
Consider tcfail041:
   class (?x::Int) => C a where ...
   instance C Int
The class is invalid because of the superclass constraint.  But
we still want it to look like a /class/, else the instance bleats
that the instance is mal-formed because it hasn't got a class in
the head.


### Note: Class method constraints

Haskell 2010 is supposed to reject
  class C a where
    op :: Eq a => a -> a
where the method type costrains only the class variable(s).  (The extension
-XConstrainedClassMethods switches off this check.)  But regardless
we should not reject
  class C a where
    op :: (?x::Int) => a -> a
as pointed out in Trac #11793. So the test here rejects the program if
  * -XConstrainedClassMethods is off
  * the tyvars of the constraint are non-empty
  * all the tyvars are class tyvars, none are locally quantified

### Note: Abort when superclass cycle is detected

We must avoid doing the ambiguity check for the methods (in
checkValidClass.check_op) when there are already errors accumulated.
This is because one of the errors may be a superclass cycle, and
superclass cycles cause canonicalization to loop. Here is a
representative example:

  class D a => C a where
    meth :: D a => ()
  class C a => D a

This fixes Trac #9415, #9739

### Note: Default method type signatures must align

GHC enforces the invariant that a class method's default type signature
must "align" with that of the method's non-default type signature, as per
GHC Trac #12918. For instance, if you have:

  class Foo a where
    bar :: forall b. Context => a -> b

Then a default type signature for bar must be alpha equivalent to
(forall b. a -> b). That is, the types must be the same modulo differences in
contexts. So the following would be acceptable default type signatures:

    default bar :: forall b. Context1 => a -> b
    default bar :: forall x. Context2 => a -> x

But the following are NOT acceptable default type signatures:

    default bar :: forall b. b -> a
    default bar :: forall x. x
    default bar :: a -> Int

Note that a is bound by the class declaration for Foo itself, so it is
not allowed to differ in the default type signature.

The default type signature (default bar :: a -> Int) deserves special mention,
since (a -> Int) is a straightforward instantiation of (forall b. a -> b). To
write this, you need to declare the default type signature like so:

    default bar :: forall b. (b ~ Int). a -> b

As noted in #12918, there are several reasons to do this:

1. It would make no sense to have a type that was flat-out incompatible with
   the non-default type signature. For instance, if you had:

     class Foo a where
       bar :: a -> Int
       default bar :: a -> Bool

   Then that would always fail in an instance declaration. So this check
   nips such cases in the bud before they have the chance to produce
   confusing error messages.

### Note: Default methods in instances

3. Aesthetically, by only allowing the default type signature to differ in its
   context, we are making it more explicit the ways in which the default type
   signature is less polymorphic than the non-default type signature.

You might be wondering: why are the contexts allowed to be different, but not
the rest of the type signature? That's because default implementations often
rely on assumptions that the more general, non-default type signatures do not.
For instance, in the Enum class declaration:

    class Enum a where
      enum :: [a]
      default enum :: (Generic a, GEnum (Rep a)) => [a]
      enum = map to genum

    class GEnum f where
      genum :: [f a]

The default implementation for enum only works for types that are instances of
Generic, and for which their generic Rep type is an instance of GEnum. But
clearly enum doesn't _have_ to use this implementation, so naturally, the
context for enum is allowed to be different to accomodate this. As a result,
when we validity-check default type signatures, we ignore contexts completely.

### Note: Splitting nested sigma types in class type signatures

### Note: Splitting nested sigma types in class type signatures

Consider this type synonym and class definition:

  type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t

  class Each s t a b where
    each         ::                                      Traversal s t a b
    default each :: (Traversable g, s ~ g a, t ~ g b) => Traversal s t a b

It might seem obvious that the tau types in both type signatures for `each`
are the same, but actually getting GHC to conclude this is surprisingly tricky.
That is because in general, the form of a class method's non-default type
signature is:

  forall a. C a => forall d. D d => E a b

And the general form of a default type signature is:

  forall f. F f => E a f -- The variable `a` comes from the class

So it you want to get the tau types in each type signature, you might find it
reasonable to call tcSplitSigmaTy twice on the non-default type signature, and
call it once on the default type signature. For most classes and methods, this
will work, but Each is a bit of an exceptional case. The way `each` is written,
it doesn't quantify any additional type variables besides those of the Each
class itself, so the non-default type signature for `each` is actually this:

  forall s t a b. Each s t a b => Traversal s t a b

Notice that there _appears_ to only be one forall. But there's actually another
forall lurking in the Traversal type synonym, so if you call tcSplitSigmaTy
twice, you'll also go under the forall in Traversal! That is, you'll end up
with:

  (a -> f b) -> s -> f t

A problem arises because you only call tcSplitSigmaTy once on the default type
signature for `each`, which gives you

  Traversal s t a b

Or, equivalently:

  forall f. Applicative f => (a -> f b) -> s -> f t

This is _not_ the same thing as (a -> f b) -> s -> f t! So now tcMatchTy will
say that the tau types for `each` are not equal.

A solution to this problem is to use tcSplitNestedSigmaTys instead of
tcSplitSigmaTy. tcSplitNestedSigmaTys will always split any foralls that it
sees until it can't go any further, so if you called it on the default type
signature for `each`, it would return (a -> f b) -> s -> f t like we desired.

### Note: Checking partial record field

This check checks the partial record field selector, and warns (Trac #7169).

For example:

  data T a = A { m1 :: a, m2 :: a } | B { m1 :: a }

The function 'm2' is partial record field, and will fail when it is applied to
'B'. The warning identifies such partial fields. The check is performed at the
declaration of T, not at the call-sites of m2.

The warning can be suppressed by prefixing the field-name with an underscore.
For example:

  data T a = A { m1 :: a, _m2 :: a } | B { m1 :: a }

# Checking role validity


# Error messages
