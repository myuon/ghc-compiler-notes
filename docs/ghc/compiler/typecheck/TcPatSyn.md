[[src]](https://github.com/ghc/ghc/tree/master/compiler/typecheck/TcPatSyn.hs)

(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

# Typechecking pattern synonym declarations

# Type checking a pattern synonym


### Note: Remove redundant provided dicts

Recall that
   HRefl :: forall k1 k2 (a1:k1) (a2:k2). (k1 ~ k2, a1 ~ a2)
                                       => a1 :~~: a2
(NB: technically the (k1~k2) existential dictionary is not necessary,
but it's there at the moment.)

Now consider (Trac #14394):
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


# [Pattern synonyms and higher rank types]

Consider
  data T = MkT (forall a. a->a)

  pattern P :: (Int -> Int) -> T
  pattern P x <- MkT x

This should work.  But in the matcher we must match against MkT, and then
instantiate its argument 'x', to get a function of type (Int -> Int).
Equality is not enough!  Trac #13752 was an example.

### Note: Checking against a pattern signature

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

  At one point, for ImplicitBidirectional I used SigTvs (instead of
  TauTvs) in tcCheckPatSynDecl.  But (a) strengthening the check here
  is redundant since tcPatSynBuilderBind does the job, (b) it was
  still incomplete (SigTvs can unify with each other), and (c) it
  didn't even work (Trac #13441 was accepted with
  ExplicitBidirectional, but rejected if expressed in
  ImplicitBidirectional form.  Conclusion: trying to be too clever is
  a bad idea.


# Constructing the "matcher" Id and its binding


# Constructing the "builder" Id


### Note: Builder for a bidirectional pattern synonym

For a bidirectional pattern synonym we need to produce an /expression/
that matches the supplied /pattern/, given values for the arguments
of the pattern synoymy.  For example
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

### Note: Redundant constraints for builder

The builder can have redundant constraints, which are awkard to eliminate.
Consider
   pattern P = Just 34
To match against this pattern we need (Eq a, Num a).  But to build
(Just 34) we need only (Num a).  Fortunately instTcSigFromId sets
sig_warn_redundant to False.

# Helper functions


### Note: As-patterns in pattern synonym definitions

The rationale for rejecting as-patterns in pattern synonym definitions
is that an as-pattern would introduce nonindependent pattern synonym
arguments, e.g. given a pattern synonym like:

        pattern K x y = x@(Just y)

one could write a nonsensical function like

        f (K Nothing x) = ...

or
        g (K (Just True) False) = ...

### Note: Type signatures and the builder expression

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
latter; Trac #9867.)  No, the job of the signature is done, so when
converting the pattern to an expression (for the builder RHS) we
simply discard the signature.

### Note: Record PatSyn Desugaring

Any change to this ordering should make sure to change deSugar/DsExpr.hs if you
want to avoid difficult to decipher core lint errors!
 

### Note: Bad patterns

We don't currently allow as-patterns or n+k patterns in a pattern synonym.
Reason: consider
  pattern P x y = x@(Just y)

What would
  f (P Nothing False) = e
mean?  Presumably something like
  f Nothing@(Just False) = e
But as-patterns don't allow a pattern before the @ sign!  Perhaps they
should -- with p1@p2 meaning match both p1 and p2 -- but they don't
currently.  Hence bannning them in pattern synonyms.  Actually lifting
the restriction would be simple and well-defined.  See Trac #9793.
