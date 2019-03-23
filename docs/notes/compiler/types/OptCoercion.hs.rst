Note [Optimising coercion optimisation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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



Note [Optimising InstCo]
~~~~~~~~~~~~~~~~~~~~~~~~
(1) tv is a type variable
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
(g2 ; t2 ~ t2 |> sym h) in g, we'll get this result exactly.
This is bizarre,
though, because we're substituting a type variable with a coercion. However,
this operation already exists: it's called *lifting*, and defined in Coercion.
We just need to enhance the lifting operation to be able to deal with
an ambient substitution, which is why a LiftingContext stores a TCvSubst.

(2) cv is a coercion variable
Now consider we have (InstCo (ForAllCo cv h g) g2), we want to optimise.

h : (t1 ~r t2) ~N (t3 ~r t4)
cv : t1 ~r t2 |- g : t1' ~r2 t2'
n1 = nth r 2 (downgradeRole r N h) :: t1 ~r t3
n2 = nth r 3 (downgradeRole r N h) :: t2 ~r t4
------------------------------------------------
ForAllCo cv h g : (all cv:t1 ~r t2. t1') ~r2
                  (all cv:t3 ~r t4. t2'[cv |-> n1 ; cv ; sym n2])

g1 : (all cv:t1 ~r t2. t1') ~ (all cv: t3 ~r t4. t2')
g2 : h1 ~N h2
h1 : t1 ~r t2
h2 : t3 ~r t4
------------------------------------------------
InstCo g1 g2 : t1'[cv |-> h1] ~ t2'[cv |-> h2]

We thus want some coercion proving this:

  t1'[cv |-> h1] ~ t2'[cv |-> n1 ; h2; sym n2]

So we substitute the coercion variable c for the coercion
(h1 ~N (n1; h2; sym n2)) in g.


Note [Optimise CoVarCo to Refl]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If we have (c :: t~t) we can optimise it to Refl. That increases the
chances of floating the Refl upwards; e.g. Maybe c --> Refl (Maybe t)

We do so here in optCoercion, not in mkCoVarCo; see Note [mkCoVarCo]
in Coercion.
-----------


Note [Conflict checking with AxiomInstCo]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider the following type family and axiom:

type family Equal (a :: k) (b :: k) :: Bool
type instance where
  Equal a a = True
  Equal a b = False
--
Equal :: forall k::*. k -> k -> Bool
axEqual :: { forall k::*. forall a::k. Equal k a a ~ True
           ; forall k::*. forall a::k. forall b::k. Equal k a b ~ False }

We wish to disallow (axEqual[1] <*> <Int> <Int). (Recall that the index is
0-based, so this is the second branch of the axiom.) The problem is that, on
the surface, it seems that (axEqual[1] <*> <Int> <Int>) :: (Equal * Int Int ~
False) and that all is OK. But, all is not OK: we want to use the first branch
of the axiom in this case, not the second. The problem is that the parameters
of the first branch can unify with the supplied coercions, thus meaning that
the first branch should be taken. See also Note [Apartness] in
types/FamInstEnv.hs.



Note [Why call checkAxInstCo during optimisation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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



Note [EtaAppCo]
~~~~~~~~~~~~~~~
Suppose we're trying to optimize (co1a co1b ; co2a co2b). Ideally, we'd
like to rewrite this to (co1a ; co2a) (co1b ; co2b). The problem is that
the resultant coercions might not be well kinded. Here is an example (things
labeled with x don't matter in this example):

  k1 :: Type
  k2 :: Type

  a :: k1 -> Type
  b :: k1

  h :: k1 ~ k2

  co1a :: x1 ~ (a |> (h -> <Type>)
  co1b :: x2 ~ (b |> h)

  co2a :: a ~ x3
  co2b :: b ~ x4

First, convince yourself of the following:

  co1a co1b :: x1 x2 ~ (a |> (h -> <Type>)) (b |> h)
  co2a co2b :: a b   ~ x3 x4

  (a |> (h -> <Type>)) (b |> h) `eqType` a b

That last fact is due to Note [Non-trivial definitional equality] in TyCoRep,
where we ignore coercions in types as long as two types' kinds are the same.
In our case, we meet this last condition, because

  (a |> (h -> <Type>)) (b |> h) :: Type
    and
  a b :: Type

So the input coercion (co1a co1b ; co2a co2b) is well-formed. But the
suggested output coercions (co1a ; co2a) and (co1b ; co2b) are not -- the
kinds don't match up.

The solution here is to twiddle the kinds in the output coercions. First, we
need to find coercions

  ak :: kind(a |> (h -> <Type>)) ~ kind(a)
  bk :: kind(b |> h)             ~ kind(b)

This can be done with mkKindCo and buildCoercion. The latter assumes two
types are identical modulo casts and builds a coercion between them.

Then, we build (co1a ; co2a |> sym ak) and (co1b ; co2b |> sym bk) as the
output coercions. These are well-kinded.

Also, note that all of this is done after accumulated any nested AppCo
parameters. This step is to avoid quadratic behavior in calling coercionKind.

The problem described here was first found in dependent/should_compile/dynamic-paper.



Note [Eta for AppCo]
~~~~~~~~~~~~~~~~~~~~
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

