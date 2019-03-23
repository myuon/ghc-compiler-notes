Note [Pattern synonym error recovery]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If type inference for a pattern synonym fails, we can't continue with
the rest of tc_patsyn_finish, because we may get knock-on errors, or
even a crash.  E.g. from
   pattern What = True :: Maybe
we get a kind error; and we must stop right away (#15289).

We stop if there are /any/ unsolved constraints, not just insoluble
ones; because pattern synonyms are top-level things, we will never
solve them later if we can't solve them now.  And if we were to carry
on, tc_patsyn_finish does zonkTcTypeToType, which defaults any
unsolved unificatdion variables to Any, which confuses the error
reporting no end (#15685).

So we use simplifyTop to completely solve the constraint, report
any errors, throw an exception.

Even in the event of such an error we can recover and carry on, just
as we do for value bindings, provided we plug in placeholder for the
pattern synonym: see recoverPSB.  The goal of the placeholder is not
to cause a raft of follow-on errors.  I've used the simplest thing for
now, but we might need to elaborate it a bit later.  (e.g.  I've given
it zero args, which may cause knock-on errors if it is used in a
pattern.) But it'll do for now.



Note [Remove redundant provided dicts]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Recall that
   HRefl :: forall k1 k2 (a1:k1) (a2:k2). (k1 ~ k2, a1 ~ a2)
                                       => a1 :~~: a2
(NB: technically the (k1~k2) existential dictionary is not necessary,
but it's there at the moment.)

Now consider (#14394):
   pattern Foo = HRefl
in a non-poly-kinded module.  We don't want to get
    pattern Foo :: () => (* ~ *, b ~ a) => a :~~: b
with that redundant (* ~ *).  We'd like to remove it; hence the call to
mkMinimalWithSCs.

Similarly consider
  data S a where { MkS :: Ord a => a -> S a }
  pattern Bam x y <- (MkS (x::a), MkS (y::a)))

The pattern (Bam x y) binds two (Ord a) dictionaries, but we only
need one.  Agian mkMimimalWithSCs removes the redundant one.



Note [Equality evidence in pattern synonyms]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
  data X a where
     MkX :: Eq a => [a] -> X (Maybe a)
  pattern P x = MkG x

Then there is a danger that GHC will infer
  P :: forall a.  () =>
       forall b. (a ~# Maybe b, Eq b) => [b] -> X a

The 'builder' for P, which is called in user-code, will then
have type
  $bP :: forall a b. (a ~# Maybe b, Eq b) => [b] -> X a

and that is bad because (a ~# Maybe b) is not a predicate type
(see Note [Types for coercions, predicates, and evidence] in Type)
and is not implicitly instantiated.

So in mkProvEvidence we lift (a ~# b) to (a ~ b).  Tiresome, and
marginally less efficient, if the builder/martcher are not inlined.

See also Note [Lift equality constaints when quantifying] in TcType



Note [Coercions that escape]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#14507 showed an example where the inferred type of the matcher
for the pattern synonym was somethign like
   $mSO :: forall (r :: TYPE rep) kk (a :: k).
           TypeRep k a
           -> ((Bool ~ k) => TypeRep Bool (a |> co_a2sv) -> r)
           -> (Void# -> r)
           -> r

What is that co_a2sv :: Bool ~# *??  It was bound (via a superclass
selection) by the pattern being matched; and indeed it is implicit in
the context (Bool ~ k).  You could imagine trying to extract it like
this:
   $mSO :: forall (r :: TYPE rep) kk (a :: k).
           TypeRep k a
           -> ( co :: ((Bool :: *) ~ (k :: *)) =>
                  let co_a2sv = sc_sel co
                  in TypeRep Bool (a |> co_a2sv) -> r)
           -> (Void# -> r)
           -> r

But we simply don't allow that in types.  Maybe one day but not now.

How to detect this situation?  We just look for free coercion variables
in the types of any of the arguments to the matcher.  The error message
is not very helpful, but at least we don't get a Lint error.


Note [The pattern-synonym signature splitting rule]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Given a pattern signature, we must split
     the kind-generalised variables, and
     the implicitly-bound variables
into universal and existential.  The rule is this
(see discussion on #11224):

     The universal tyvars are the ones mentioned in
          - univ_tvs: the user-specified (forall'd) universals
          - req_theta
          - res_ty
     The existential tyvars are all the rest

For example

   pattern P :: () => b -> T a
   pattern P x = ...

Here 'a' is universal, and 'b' is existential.  But there is a wrinkle:
how do we split the arg_tys from req_ty?  Consider

   pattern Q :: () => b -> S c -> T a
   pattern Q x = ...

This is an odd example because Q has only one syntactic argument, and
so presumably is defined by a view pattern matching a function.  But
it can happen (#11977, #12108).

We don't know Q's arity from the pattern signature, so we have to wait
until we see the pattern declaration itself before deciding res_ty is,
and hence which variables are existential and which are universal.

And that in turn is why TcPatSynInfo has a separate field,
patsig_implicit_bndrs, to capture the implicitly bound type variables,
because we don't yet know how to split them up.

It's a slight compromise, because it means we don't really know the
pattern synonym's real signature until we see its declaration.  So,
for example, in hs-boot file, we may need to think what to do...
(eg don't have any implicitly-bound variables).




Note [Checking against a pattern signature]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When checking the actual supplied pattern against the pattern synonym
signature, we need to be quite careful.

----- Provided constraints
Example

    data T a where
      MkT :: Ord a => a -> T a

    pattern P :: () => Eq a => a -> [T a]
    pattern P x = [MkT x]

We must check that the (Eq a) that P claims to bind (and to
make available to matches against P), is derivable from the
actual pattern.  For example:
    f (P (x::a)) = ...here (Eq a) should be available...
And yes, (Eq a) is derivable from the (Ord a) bound by P's rhs.

----- Existential type variables
Unusually, we instantiate the existential tyvars of the pattern with
*meta* type variables.  For example

    data S where
      MkS :: Eq a => [a] -> S

    pattern P :: () => Eq x => x -> S
    pattern P x <- MkS x

The pattern synonym conceals from its client the fact that MkS has a
list inside it.  The client just thinks it's a type 'x'.  So we must
unify x := [a] during type checking, and then use the instantiating type
[a] (called ex_tys) when building the matcher.  In this case we'll get

   $mP :: S -> (forall x. Ex x => x -> r) -> r -> r
   $mP x k = case x of
               MkS a (d:Eq a) (ys:[a]) -> let dl :: Eq [a]
                                              dl = $dfunEqList d
                                          in k [a] dl ys

All this applies when type-checking the /matching/ side of
a pattern synonym.  What about the /building/ side?

* For Unidirectional, there is no builder

* For ExplicitBidirectional, the builder is completely separate
  code, typechecked in tcPatSynBuilderBind

* For ImplicitBidirectional, the builder is still typechecked in
  tcPatSynBuilderBind, by converting the pattern to an expression and
  typechecking it.

  At one point, for ImplicitBidirectional I used TyVarTvs (instead of
  TauTvs) in tcCheckPatSynDecl.  But (a) strengthening the check here
  is redundant since tcPatSynBuilderBind does the job, (b) it was
  still incomplete (TyVarTvs can unify with each other), and (c) it
  didn't even work (#13441 was accepted with
  ExplicitBidirectional, but rejected if expressed in
  ImplicitBidirectional form.  Conclusion: trying to be too clever is
  a bad idea.


Note [Builder for a bidirectional pattern synonym]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For a bidirectional pattern synonym we need to produce an /expression/
that matches the supplied /pattern/, given values for the arguments
of the pattern synonym.  For example
  pattern F x y = (Just x, [y])
The 'builder' for F looks like
  $builderF x y = (Just x, [y])

We can't always do this:
 * Some patterns aren't invertible; e.g. view patterns
      pattern F x = (reverse -> x:_)

 * The RHS pattern might bind more variables than the pattern
   synonym, so again we can't invert it
      pattern F x = (x,y)

 * Ditto wildcards
      pattern F x = (x,_)




Note [Redundant constraints for builder]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The builder can have redundant constraints, which are awkard to eliminate.
Consider
   pattern P = Just 34
To match against this pattern we need (Eq a, Num a).  But to build
(Just 34) we need only (Num a).  Fortunately instTcSigFromId sets
sig_warn_redundant to False.



Note [As-patterns in pattern synonym definitions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The rationale for rejecting as-patterns in pattern synonym definitions
is that an as-pattern would introduce nonindependent pattern synonym
arguments, e.g. given a pattern synonym like:

        pattern K x y = x@(Just y)

one could write a nonsensical function like

        f (K Nothing x) = ...

or
        g (K (Just True) False) = ...



Note [Type signatures and the builder expression]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
   pattern L x = Left x :: Either [a] [b]

In tc{Infer/Check}PatSynDecl we will check that the pattern has the
specified type.  We check the pattern *as a pattern*, so the type
signature is a pattern signature, and so brings 'a' and 'b' into
scope.  But we don't have a way to bind 'a, b' in the LHS, as we do
'x', say.  Nevertheless, the sigature may be useful to constrain
the type.

When making the binding for the *builder*, though, we don't want
  $buildL x = Left x :: Either [a] [b]
because that wil either mean (forall a b. Either [a] [b]), or we'll
get a complaint that 'a' and 'b' are out of scope. (Actually the
latter; #9867.)  No, the job of the signature is done, so when
converting the pattern to an expression (for the builder RHS) we
simply discard the signature.



Note [Record PatSyn Desugaring]
-------------------------------
It is important that prov_theta comes before req_theta as this ordering is used
when desugaring record pattern synonym updates.

Any change to this ordering should make sure to change deSugar/DsExpr.hs if you
want to avoid difficult to decipher core lint errors!
 