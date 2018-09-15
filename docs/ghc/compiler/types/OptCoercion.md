[[src]](https://github.com/ghc/ghc/tree/master/compiler/types/OptCoercion.hs)
# 

### Note: Optimising coercion optimisation

Looking up a coercion's role or kind is linear in the size of the
coercion. Thus, doing this repeatedly during the recursive descent
of coercion optimisation is disastrous. We must be careful to avoid
doing this if at all possible.

Because it is generally easy to know a coercion's components' roles
from the role of the outer coercion, we pass down the known role of
the input in the algorithm below. We also keep functions opt_co2
and opt_co3 separate from opt_co4, so that the former two do Phantom
checks that opt_co4 can avoid. This is a big win because Phantom coercions
rarely appear within non-phantom coercions -- only in some TyConAppCos
and some AxiomInstCos. We handle these cases specially by calling
opt_co2.

### Note: Optimising InstCo

When we have (InstCo (ForAllCo tv h g) g2), we want to optimise.

Let's look at the typing rules.

h : k1 ~ k2
tv:k1 |- g : t1 ~ t2
-----------------------------
ForAllCo tv h g : (all tv:k1.t1) ~ (all tv:k2.t2[tv |-> tv |> sym h])

g1 : (all tv:k1.t1') ~ (all tv:k2.t2')
g2 : s1 ~ s2
--------------------
InstCo g1 g2 : t1'[tv |-> s1] ~ t2'[tv |-> s2]

We thus want some coercion proving this:

  (t1[tv |-> s1]) ~ (t2[tv |-> s2 |> sym h])

If we substitute the *type* tv for the *coercion*
(g2 `mkCoherenceRightCo` sym h) in g, we'll get this result exactly.
This is bizarre,
though, because we're substituting a type variable with a coercion. However,
this operation already exists: it's called *lifting*, and defined in Coercion.
We just need to enhance the lifting operation to be able to deal with
an ambient substitution, which is why a LiftingContext stores a TCvSubst.




opt_co4_wrap env sym rep r co
  = pprTrace "opt_co4_wrap {"
    ( vcat [ text "Sym:" <+> ppr sym
           , text "Rep:" <+> ppr rep
           , text "Role:" <+> ppr r
           , text "Co:" <+> ppr co ]) $
    ASSERT( r == coercionRole co )
    let result = opt_co4 env sym rep r co in
    pprTrace "opt_co4_wrap }" (ppr co $$ text "---" $$ ppr result) $
    result


### Note: Optimise CoVarCo to Refl

If we have (c :: t~t) we can optimise it to Refl. That increases the
chances of floating the Refl upwards; e.g. Maybe c --> Refl (Maybe t)

### Note: mkCoVarCo

### Note: Differing kinds

The two types may not have the same kind (although that would be very unusual).
But even if they have the same kind, and the same type constructor, the number
of arguments in a `CoTyConApp` can differ. Consider

  Any :: forall k. k

  Any * Int                      :: *
  Any (*->*) Maybe Int  :: *

Hence the need to compare argument lengths; see Trac #13658
 

### Note: Conflict checking with AxiomInstCo

Consider the following type family and axiom:

type family Equal (a :: k) (b :: k) :: Bool
type instance where
  Equal a a = True
  Equal a b = False
--
Equal :: forall k::*. k -> k -> Bool
axEqual :: { forall k::*. forall a::k. Equal k a a ~ True
           ; forall k::*. forall a::k. forall b::k. Equal k a b ~ False }

### Note: Apartness

### Note: Why call checkAxInstCo during optimisation

It is possible that otherwise-good-looking optimisations meet with disaster
in the presence of axioms with multiple equations. Consider

type family Equal (a :: *) (b :: *) :: Bool where
  Equal a a = True
  Equal a b = False
type family Id (a :: *) :: * where
  Id a = a

axEq :: { [a::*].       Equal a a ~ True
        ; [a::*, b::*]. Equal a b ~ False }
axId :: [a::*]. Id a ~ a

co1 = Equal (axId[0] Int) (axId[0] Bool)
  :: Equal (Id Int) (Id Bool) ~  Equal Int Bool
co2 = axEq[1] <Int> <Bool>
  :: Equal Int Bool ~ False

We wish to optimise (co1 ; co2). We end up in rule TrPushAxL, noting that
co2 is an axiom and that matchAxiom succeeds when looking at co1. But, what
happens when we push the coercions inside? We get

co3 = axEq[1] (axId[0] Int) (axId[0] Bool)
  :: Equal (Id Int) (Id Bool) ~ False

which is bogus! This is because the type system isn't smart enough to know
that (Id Int) and (Id Bool) are Surely Apart, as they're headed by type
families. At the time of writing, I (Richard Eisenberg) couldn't think of
a way of detecting this any more efficient than just building the optimised
coercion and checking.


# etaForAllCo_maybe

Suppose we have

  g : all a1:k1.t1  ~  all a2:k2.t2

but g is *not* a ForAllCo. We want to eta-expand it. So, we do this:

  g' = all a1:(ForAllKindCo g).(InstCo g (a1 `mkCoherenceRightCo` ForAllKindCo g))

Call the kind coercion h1 and the body coercion h2. We can see that

  h2 : t1 ~ t2[a2 |-> (a1 |> h2)]

According to the typing rule for ForAllCo, we get that

  g' : all a1:k1.t1  ~  all a1:k2.(t2[a2 |-> (a1 |> h2)][a1 |-> a1 |> sym h2])

or

  g' : all a1:k1.t1  ~  all a1:k2.(t2[a2 |-> a1])

as desired.


### Note: Eta for AppCo

Suppose we have
   g :: s1 t1 ~ s2 t2

Then we can't necessarily make
   left  g :: s1 ~ s2
   right g :: t1 ~ t2
because it's possible that
   s1 :: * -> *         t1 :: *
   s2 :: (*->*) -> *    t2 :: * -> *
and in that case (left g) does not have the same
kind on either side.

It's enough to check that
  kind t1 = kind t2
because if g is well-kinded then
  kind (s1 t2) = kind (s2 t2)
and these two imply
  kind s1 = kind s2

