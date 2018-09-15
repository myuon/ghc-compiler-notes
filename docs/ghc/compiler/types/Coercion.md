[[src]](https://github.com/ghc/ghc/tree/master/compiler/types/Coercion.hs)

(c) The University of Glasgow 2006


# 

# 

# 

Defined here to avoid module loops. CoAxiom is loaded very early on.



# 

### Note: Function coercions

Remember that
  (->) :: forall r1 r2. TYPE r1 -> TYPE r2 -> TYPE LiftedRep

Hence
  FunCo r co1 co2 :: (s1->t1) ~r (s2->t2)
is short for
  TyConAppCo (->) co_rep1 co_rep2 co1 co2
where co_rep1, co_rep2 are the coercions on the representations.


# 

These "smart constructors" maintain the invariants listed in the definition
of Coercion, and they perform very basic optimizations.

### Note: Role twiddling functions


There are a plethora of functions for twiddling roles:

mkSubCo: Requires a nominal input coercion and always produces a
representational output. This is used when you (the programmer) are sure you
know exactly that role you have and what you want.

downgradeRole_maybe: This function takes both the input role and the output role
as parameters. (The *output* role comes first!) It can only *downgrade* a
role -- that is, change it from N to R or P, or from R to P. This one-way
behavior is why there is the "_maybe". If an upgrade is requested, this
function produces Nothing. This is used when you need to change the role of a
coercion, but you're not sure (as you're writing the code) of which roles are
involved.

This function could have been written using coercionRole to ascertain the role
of the input. But, that function is recursive, and the caller of downgradeRole_maybe
often knows the input role. So, this is more efficient.

downgradeRole: This is just like downgradeRole_maybe, but it panics if the
conversion isn't a downgrade.

setNominalRole_maybe: This is the only function that can *upgrade* a coercion.
The result (if it exists) is always Nominal. The input can be at any role. It
works on a "best effort" basis, as it should never be strictly necessary to
upgrade a coercion during compilation. It is currently only used within GHC in
splitAppCo_maybe. In order to be a proper inverse of mkAppCo, the second
coercion that splitAppCo_maybe returns must be nominal. But, it's conceivable
that splitAppCo_maybe is operating over a TyConAppCo that uses a
representational coercion. Hence the need for setNominalRole_maybe.
splitAppCo_maybe, in turn, is used only within coercion optimization -- thus,
it is not absolutely critical that setNominalRole_maybe be complete.

Note that setNominalRole_maybe will never upgrade a phantom UnivCo. Phantom
UnivCos are perfectly type-safe, whereas representational and nominal ones are
not. Indeed, `unsafeCoerce` is implemented via a representational UnivCo.
(Nominal ones are no worse than representational ones, so this function *will*
change a UnivCo Representational to a UnivCo Nominal.)

Conal Elliott also came across a need for this function while working with the
GHC API, as he was decomposing Core casts. The Core casts use representational
coercions, as they must, but his use case required nominal coercions (he was
building a GADT). So, that's why this function is exported from this module.

One might ask: shouldn't downgradeRole_maybe just use setNominalRole_maybe as
appropriate? I (Richard E.) have decided not to do this, because upgrading a
role is bizarre and a caller should have to ask for this behavior explicitly.

### Note: mkTransAppCo

Suppose we have

  co1 :: a ~R Maybe
  co2 :: b ~R Int

and we want

  co3 :: a b ~R Maybe Int

This seems sensible enough. But, we can't let (co3 = co1 co2), because
that's ill-roled! Note that mkAppCo requires a *nominal* second coercion.

The way around this is to use transitivity:

  co3 = (co1 <b>_N) ; (Maybe co2) :: a b ~R Maybe Int

Or, it's possible everything is the other way around:

  co1' :: Maybe ~R a
  co2' :: Int   ~R b

and we want

  co3' :: Maybe Int ~R a b

then

  co3' = (Maybe co2') ; (co1' <b>_N)

This is exactly what `mkTransAppCo` builds for us. Information for all
the arguments tends to be to hand at call sites, so it's quicker than
using, say, coercionKind.



### Note: mkCoVarCo

### Note: Unbound RULE binders

# 

# 

# Type normalisation


# 

# 

### Note: Lifting coercions over types: liftCoSubst

The KPUSH rule deals with this situation
   data T a = MkK (a -> Maybe a)
   g :: T t1 ~ K t2
   x :: t1 -> Maybe t1

   case (K @t1 x) |> g of
     K (y:t2 -> Maybe t2) -> rhs

We want to push the coercion inside the constructor application.
So we do this

   g' :: t1~t2  =  Nth 0 g

   case K @t2 (x |> g' -> Maybe g') of
     K (y:t2 -> Maybe t2) -> rhs

The crucial operation is that we
  * take the type of K's argument: a -> Maybe a
  * and substitute g' for a
thus giving *coercion*.  This is what liftCoSubst does.

In the presence of kind coercions, this is a bit
of a hairy operation. So, we refer you to the paper introducing kind coercions,
available at www.cis.upenn.edu/~sweirich/papers/fckinds-extended.pdf


### Note: liftCoSubstTyVar

This function can fail if a coercion in the environment is of too low a role.

liftCoSubstTyVar is called from two places: in liftCoSubst (naturally), and
also in matchAxiom in OptCoercion. From liftCoSubst, the so-called lifting
lemma guarantees that the roles work out. If we fail in this
case, we really should panic -- something is deeply wrong. But, in matchAxiom,
failing is fine. matchAxiom is trying to find a set of coercions
that match, but it may fail, and this is healthy behavior.


# 

# 

### Note: Computing a coercion kind and role

To compute a coercion's kind is straightforward: see coercionKind.
But to compute a coercion's role, in the case for NthCo we need
its kind as well.  So if we have two separate functions (one for kinds
and one for roles) we can get exponentially bad behaviour, since each
NthCo node makes a separate call to coercionKind, which traverses the
sub-tree again.  This was part of the problem in Trac #9233.

Solution: compute both together; hence coercionKindRole.  We keep a
separate coercionKind function because it's a bit more efficient if
the kind is all you want.


### Note: Nested InstCos

In Trac #5631 we found that 70% of the entire compilation time was
being spent in coercionKind!  The reason was that we had
   (g @ ty1 @ ty2 .. @ ty100)    -- The "@s" are InstCos
where
   g :: forall a1 a2 .. a100. phi
If we deal with the InstCos one at a time, we'll do this:
   1.  Find the kind of (g @ ty1 .. @ ty99) : forall a100. phi'
   2.  Substitute phi'[ ty100/a100 ], a single tyvar->type subst
But this is a *quadratic* algorithm, and the blew up Trac #5631.
So it's very important to do the substitution simultaneously;
cf Type.piResultTys (which in fact we call here).

