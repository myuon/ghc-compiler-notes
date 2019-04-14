`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/types/Coercion.hs>`_

compiler/types/Coercion.hs
==========================


Note [Function coercions]
~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/types/Coercion.hs#L282>`__

Remember that
  (->) :: forall r1 r2. TYPE r1 -> TYPE r2 -> TYPE LiftedRep

Hence
  FunCo r co1 co2 :: (s1->t1) ~r (s2->t2)
is short for
  TyConAppCo (->) co_rep1 co_rep2 co1 co2
where co_rep1, co_rep2 are the coercions on the representations.



Note [Pushing a coercion into a pi-type]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/types/Coercion.hs#L321>`__

Suppose we have this:
    (f |> co) t1 .. tn
Then we want to push the coercion into the arguments, so as to make
progress. For example of why you might want to do so, see Note
[Respecting definitional equality] in TyCoRep.

This is done by decomposePiCos.  Specifically, if
    decomposePiCos co [t1,..,tn] = ([co1,...,cok], cor)
then
    (f |> co) t1 .. tn   =   (f (t1 |> co1) ... (tk |> cok)) |> cor) t(k+1) ... tn

Notes:

* k can be smaller than n! That is decomposePiCos can return *fewer*
  coercions than there are arguments (ie k < n), if the kind provided
  doesn't have enough binders.

* If there is a type error, we might see
       (f |> co) t1
  where co :: (forall a. ty) ~ (ty1 -> ty2)
  Here 'co' is insoluble, but we don't want to crash in decoposePiCos.
  So decomposePiCos carefully tests both sides of the coercion to check
  they are both foralls or both arrows.  Not doing this caused #15343.



Note [Role twiddling functions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/types/Coercion.hs#L605>`__

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



Note [mkCoVarCo]
~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/types/Coercion.hs#L850>`__

In the past, mkCoVarCo optimised (c :: t~t) to (Refl t).  That is
valid (although see Note [Unbound RULE binders] in Rules), but
it's a relatively expensive test and perhaps better done in
optCoercion.  Not a big deal either way.



Note [Lifting coercions over types: liftCoSubst]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/types/Coercion.hs#L1697>`__

The KPUSH rule deals with this situation
   data T a = K (a -> Maybe a)
   g :: T t1 ~ T t2
   x :: t1 -> Maybe t1

::

   case (K @t1 x) |> g of
     K (y:t2 -> Maybe t2) -> rhs

..

We want to push the coercion inside the constructor application.
So we do this

::

   g' :: t1~t2  =  Nth 0 g

..

::

   case K @t2 (x |> g' -> Maybe g') of
     K (y:t2 -> Maybe t2) -> rhs

..

The crucial operation is that we
  * take the type of K's argument: a -> Maybe a
  * and substitute g' for a
thus giving *coercion*.  This is what liftCoSubst does.

In the presence of kind coercions, this is a bit
of a hairy operation. So, we refer you to the paper introducing kind coercions,
available at www.cis.upenn.edu/~sweirich/papers/fckinds-extended.pdf



Note [extendLiftingContextEx]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/types/Coercion.hs#L1724>`__

Consider we have datatype
  K :: \/k. \/a::k. P -> T k  -- P be some type
  g :: T k1 ~ T k2

::

  case (K @k1 @t1 x) |> g of
    K y -> rhs

..

We want to push the coercion inside the constructor application.
We first get the coercion mapped by the universal type variable k:
   lc = k |-> Nth 0 g :: k1~k2

Here, the important point is that the kind of a is coerced, and P might be
dependent on the existential type variable a.
Thus we first get the coercion of a's kind
   g2 = liftCoSubst lc k :: k1 ~ k2

Then we store a new mapping into the lifting context
   lc2 = a |-> (t1 ~ t1 |> g2), lc

So later when we can correctly deal with the argument type P
   liftCoSubst lc2 P :: P [k|->k1][a|->t1] ~ P[k|->k2][a |-> (t1|>g2)]

This is exactly what extendLiftingContextEx does.
* For each (tyvar:k, ty) pair, we product the mapping
    tyvar |-> (ty ~ ty |> (liftCoSubst lc k))
* For each (covar:s1~s2, ty) pair, we produce the mapping
    covar |-> (co ~ co')
    co' = Sym (liftCoSubst lc s1) ;; covar ;; liftCoSubst lc s2 :: s1'~s2'

This follows the lifting context extension definition in the
"FC with Explicit Kind Equality" paper.

----------------------------------------------------
See Note [Lifting coercions over types: liftCoSubst]
----------------------------------------------------



Note [liftCoSubstTyVar]
~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/types/Coercion.hs#L1929>`__

This function can fail if a coercion in the environment is of too low a role.

liftCoSubstTyVar is called from two places: in liftCoSubst (naturally), and
also in matchAxiom in OptCoercion. From liftCoSubst, the so-called lifting
lemma guarantees that the roles work out. If we fail in this
case, we really should panic -- something is deeply wrong. But, in matchAxiom,
failing is fine. matchAxiom is trying to find a set of coercions
that match, but it may fail, and this is healthy behavior.

See Note [liftCoSubstTyVar]



Note [Nested ForAllCos]
~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/types/Coercion.hs#L2288>`__

Suppose we need `coercionKind (ForAllCo a1 (ForAllCo a2 ... (ForAllCo an
co)...) )`.   We do not want to perform `n` single-type-variable
substitutions over the kind of `co`; rather we want to do one substitution
which substitutes for all of `a1`, `a2` ... simultaneously.  If we do one
at a time we get the performance hole reported in #11735.

Solution: gather up the type variables for nested `ForAllCos`, and
substitute for them all at once.  Remarkably, for #11735 this single
change reduces /total/ compile time by a factor of more than ten.



Note [Nested InstCos]
~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/types/Coercion.hs#L2335>`__

In #5631 we found that 70% of the entire compilation time was
being spent in coercionKind!  The reason was that we had
   (g @ ty1 @ ty2 .. @ ty100)    -- The "@s" are InstCos
where
   g :: forall a1 a2 .. a100. phi
If we deal with the InstCos one at a time, we'll do this:
   1.  Find the kind of (g @ ty1 .. @ ty99) : forall a100. phi'
   2.  Substitute phi'[ ty100/a100 ], a single tyvar->type subst
But this is a *quadratic* algorithm, and the blew up #5631.
So it's very important to do the substitution simultaneously;
cf Type.piResultTys (which in fact we call here).



Note [simplifyArgsWorker]
~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/types/Coercion.hs#L2453>`__

Invariant (F2) of Note [Flattening] says that flattening is homogeneous.
This causes some trouble when flattening a function applied to a telescope
of arguments, perhaps with dependency. For example, suppose

::

  type family F :: forall (j :: Type) (k :: Type). Maybe j -> Either j k -> Bool -> [k]

..

and we wish to flatten the args of (with kind applications explicit)

::

  F a b (Just a c) (Right a b d) False

..

where all variables are skolems and

::

  a :: Type
  b :: Type
  c :: a
  d :: k

..

::

  [G] aco :: a ~ fa
  [G] bco :: b ~ fb
  [G] cco :: c ~ fc
  [G] dco :: d ~ fd

..

The first step is to flatten all the arguments. This is done before calling
simplifyArgsWorker. We start from

  a
  b
  Just a c
  Right a b d
  False

and get

::

  (fa,                             co1 :: fa ~ a)
  (fb,                             co2 :: fb ~ b)
  (Just fa (fc |> aco) |> co6,     co3 :: (Just fa (fc |> aco) |> co6) ~ (Just a c))
  (Right fa fb (fd |> bco) |> co7, co4 :: (Right fa fb (fd |> bco) |> co7) ~ (Right a b d))
  (False,                          co5 :: False ~ False)

..

where
  co6 :: Maybe fa ~ Maybe a
  co7 :: Either fa fb ~ Either a b

We now process the flattened args in left-to-right order. The first two args
need no further processing. But now consider the third argument. Let f3 = the flattened
result, Just fa (fc |> aco) |> co6.
This f3 flattened argument has kind (Maybe a), due to
(F2). And yet, when we build the application (F fa fb ...), we need this
argument to have kind (Maybe fa), not (Maybe a). We must cast this argument.
The coercion to use is
determined by the kind of F: we see in F's kind that the third argument has
kind Maybe j. Critically, we also know that the argument corresponding to j
(in our example, a) flattened with a coercion co1. We can thus know the
coercion needed for the 3rd argument is (Maybe (sym co1)), thus building
(f3 |> Maybe (sym co1))

More generally, we must use the Lifting Lemma, as implemented in
Coercion.liftCoSubst. As we work left-to-right, any variable that is a
dependent parameter (j and k, in our example) gets mapped in a lifting context
to the coercion that is output from flattening the corresponding argument (co1
and co2, in our example). Then, after flattening later arguments, we lift the
kind of these arguments in the lifting context that we've be building up.
This coercion is then used to keep the result of flattening well-kinded.

Working through our example, this is what happens:

  1. Extend the (empty) LC with [j |-> co1]. No new casting must be done,
     because the binder associated with the first argument has a closed type (no
     variables).

::

  2. Extend the LC with [k |-> co2]. No casting to do.

..

  3. Lifting the kind (Maybe j) with our LC
     yields co8 :: Maybe fa ~ Maybe a. Use (f3 |> sym co8) as the argument to
     F.

::

  4. Lifting the kind (Either j k) with our LC
     yields co9 :: Either fa fb ~ Either a b. Use (f4 |> sym co9) as the 4th
     argument to F, where f4 is the flattened form of argument 4, written above.

..

  5. We lift Bool with our LC, getting <Bool>;
     casting has no effect.

We're now almost done, but the new application (F fa fb (f3 |> sym co8) (f4 > sym co9) False)
has the wrong kind. Its kind is [fb], instead of the original [b].
So we must use our LC one last time to lift the result kind [k],
getting res_co :: [fb] ~ [b], and we cast our result.

Accordingly, the final result is

  F fa fb (Just fa (fc |> aco) |> Maybe (sym aco) |> sym (Maybe (sym aco)))
          (Right fa fb (fd |> bco) |> Either (sym aco) (sym bco) |> sym (Either (sym aco) (sym bco)))
          False
            |> [sym bco]

The res_co (in this case, [sym bco])
is returned as the third return value from simplifyArgsWorker.



Note [Last case in simplifyArgsWorker]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/types/Coercion.hs#L2553>`__

In writing simplifyArgsWorker's `go`, we know here that args cannot be empty,
because that case is first. We've run out of
binders. But perhaps inner_ki is a tyvar that has been instantiated with a
Π-type.

Here is an example.

  a :: forall (k :: Type). k -> k
  type family Star
  Proxy :: forall j. j -> Type
  axStar :: Star ~ Type
  type family NoWay :: Bool
  axNoWay :: NoWay ~ False
  bo :: Type
  [G] bc :: bo ~ Bool   (in inert set)

::

  co :: (forall j. j -> Type) ~ (forall (j :: Star). (j |> axStar) -> Star)
  co = forall (j :: sym axStar). (<j> -> sym axStar)

..

  We are flattening:
  a (forall (j :: Star). (j |> axStar) -> Star)   -- 1
    (Proxy |> co)                                 -- 2
    (bo |> sym axStar)                            -- 3
    (NoWay |> sym bc)                             -- 4
      :: Star

First, we flatten all the arguments (before simplifyArgsWorker), like so:

::

    (forall j. j -> Type, co1 :: (forall j. j -> Type) ~
                                 (forall (j :: Star). (j |> axStar) -> Star))  -- 1
    (Proxy |> co,         co2 :: (Proxy |> co) ~ (Proxy |> co))                -- 2
    (Bool |> sym axStar,  co3 :: (Bool |> sym axStar) ~ (bo |> sym axStar))    -- 3
    (False |> sym bc,     co4 :: (False |> sym bc) ~ (NoWay |> sym bc))        -- 4

..

Then we do the process described in Note [simplifyArgsWorker].

1. Lifting Type (the kind of the first arg) gives us a reflexive coercion, so we
   don't use it. But we do build a lifting context [k -> co1] (where co1 is a
   result of flattening an argument, written above).

2. Lifting k gives us co1, so the second argument becomes (Proxy |> co |> sym co1).
   This is not a dependent argument, so we don't extend the lifting context.

Now we need to deal with argument (3). After flattening, should we tack on a homogenizing
coercion? The way we normally tell is to lift the kind of the binder.
But here, the remainder of the kind of `a` that we're left with
after processing two arguments is just `k`.

The way forward is look up k in the lifting context, getting co1. If we're at
all well-typed, co1 will be a coercion between Π-types, with at least one binder.
So, let's
decompose co1 with decomposePiCos. This decomposition needs arguments to use
to instantiate any kind parameters. Look at the type of co1. If we just
decomposed it, we would end up with coercions whose types include j, which is
out of scope here. Accordingly, decomposePiCos takes a list of types whose
kinds are the *right-hand* types in the decomposed coercion. (See comments on
decomposePiCos.) Because the flattened types have unflattened kinds (because
flattening is homogeneous), passing the list of flattened types to decomposePiCos
just won't do: later arguments' kinds won't be as expected. So we need to get
the *unflattened* types to pass to decomposePiCos. We can do this easily enough
by taking the kind of the argument coercions, passed in originally.

(Alternative 1: We could re-engineer decomposePiCos to deal with this situation.
But that function is already gnarly, and taking the right-hand types is correct
at its other call sites, which are much more common than this one.)

(Alternative 2: We could avoid calling decomposePiCos entirely, integrating its
behavior into simplifyArgsWorker. This would work, I think, but then all of the
complication of decomposePiCos would end up layered on top of all the complication
here. Please, no.)

(Alternative 3: We could pass the unflattened arguments into simplifyArgsWorker
so that we don't have to recreate them. But that would complicate the interface
of this function to handle a very dark, dark corner case. Better to keep our
demons to ourselves here instead of exposing them to callers. This decision is
easily reversed if there is ever any performance trouble due to the call of
coercionKind.)

So we now call

  decomposePiCos co1
                 (Pair (forall j. j -> Type) (forall (j :: Star). (j |> axStar) -> Star))
                 [bo |> sym axStar, NoWay |> sym bc]

to get

::

  co5 :: Star ~ Type
  co6 :: (j |> axStar) ~ (j |> co5), substituted to
                              (bo |> sym axStar |> axStar) ~ (bo |> sym axStar |> co5)
                           == bo ~ bo
  res_co :: Type ~ Star

..

We then use these casts on (the flattened) (3) and (4) to get

::

  (Bool |> sym axStar |> co5 :: Type)   -- (C3)
  (False |> sym bc |> co6    :: bo)     -- (C4)

..

We can simplify to

::

  Bool                        -- (C3)
  (False |> sym bc :: bo)     -- (C4)

..

Of course, we still must do the processing in Note [simplifyArgsWorker] to finish
the job. We thus want to recur. Our new function kind is the left-hand type of
co1 (gotten, recall, by lifting the variable k that was the return kind of the
original function). Why the left-hand type (as opposed to the right-hand type)?
Because we have casted all the arguments according to decomposePiCos, which gets
us from the right-hand type to the left-hand one. We thus recur with that new
function kind, zapping our lifting context, because we have essentially applied
it.

This recursive call returns ([Bool, False], [...], Refl). The Bool and False
are the correct arguments we wish to return. But we must be careful about the
result coercion: our new, flattened application will have kind Type, but we
want to make sure that the result coercion casts this back to Star. (Why?
Because we started with an application of kind Star, and flattening is homogeneous.)

So, we have to twiddle the result coercion appropriately.

Let's check whether this is well-typed. We know

::

  a :: forall (k :: Type). k -> k

..

::

  a (forall j. j -> Type) :: (forall j. j -> Type) -> forall j. j -> Type

..

  a (forall j. j -> Type)
    Proxy
      :: forall j. j -> Type

  a (forall j. j -> Type)
    Proxy
    Bool
      :: Bool -> Type

  a (forall j. j -> Type)
    Proxy
    Bool
    False
      :: Type

  a (forall j. j -> Type)
    Proxy
    Bool
    False
     |> res_co
     :: Star

as desired.

Whew.



This is shared between the flattener and the normaliser in FamInstEnv.
See Note [simplifyArgsWorker]

