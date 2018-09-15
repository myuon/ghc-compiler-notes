[[src]](https://github.com/ghc/ghc/tree/master/compiler/types/Unify.hs)


Unification is much tricker than you might think.

1. The substitution we generate binds the *template type variables*
   which are given to us explicitly.

2. We want to match in the presence of foralls;
        e.g     (forall a. t1) ~ (forall b. t2)

   That is what the RnEnv2 is for; it does the alpha-renaming
   that makes it as if a and b were the same variable.
   Initialising the RnEnv2, so that it can generate a fresh
   binder when necessary, entails knowing the free variables of
   both types.

3. We must be careful not to bind a template type variable to a
   locally bound variable.  E.g.
        (forall a. x) ~ (forall b. b)
   where x is the template type variable.  Then we do not want to
   bind x to a/b!  This is a kind of occurs check.
   The necessary locals accumulate in the RnEnv2.


# Rough matching


# GADTs


### Note: Pruning dead case alternatives

Consider        data T a where
                   T1 :: T Int
                   T2 :: T a

                newtype X = MkX Int
                newtype Y = MkY Char

                type family F a
                type instance F Bool = Int

Now consider    case x of { T1 -> e1; T2 -> e2 }

The question before the house is this: if I know something about the type
of x, can I prune away the T1 alternative?

Suppose x::T Char.  It's impossible to construct a (T Char) using T1,
        Answer = YES we can prune the T1 branch (clearly)

Suppose x::T (F a), where 'a' is in scope.  Then 'a' might be instantiated
to 'Bool', in which case x::T Int, so
        ANSWER = NO (clearly)

We see here that we want precisely the apartness check implemented within
tcUnifyTysFG. So that's what we do! Two types cannot match if they are surely
apart. Note that since we are simply dropping dead code, a conservative test
suffices.


# Unification


### Note: Fine-grained unification

Do the types (x, x) and ([y], y) unify? The answer is seemingly "no" --
no substitution to finite types makes these match. But, a substitution to
*infinite* types can unify these two types: [x |-> [[[...]]], y |-> [[[...]]] ].
Why do we care? Consider these two type family instances:

type instance F x x   = Int
type instance F [y] y = Bool

If we also have

type instance Looper = [Looper]

then the instances potentially overlap. The solution is to use unification
over infinite terms. This is possible (see [1] for lots of gory details), but
a full algorithm is a little more power than we need. Instead, we make a
conservative approximation and just omit the occurs check.

[1]: http://research.microsoft.com/en-us/um/people/simonpj/papers/ext-f/axioms-extended.pdf

tcUnifyTys considers an occurs-check problem as the same as general unification
failure.

tcUnifyTysFG ("fine-grained") returns one of three results: success, occurs-check
failure ("MaybeApart"), or general failure ("SurelyApart").

See also Trac #8162.

It's worth noting that unification in the presence of infinite types is not
complete. This means that, sometimes, a closed type family does not reduce
when it should. See test case indexed-types/should_fail/Overlap15 for an
example.

### Note: The substitution in MaybeApart

The constructor MaybeApart carries data with it, typically a TvSubstEnv. Why?
Because consider unifying these:

(a, a, Int) ~ (b, [b], Bool)

### Note: Fine-grained unification

### Note: Unifying with skolems

If we discover that two types unify if and only if a skolem variable is
substituted, we can't properly unify the types. But, that skolem variable
may later be instantiated with a unifyable type. So, we return maybeApart
in these cases.

### Note: Lists of different lengths are MaybeApart

### Note: Eta reduction for data family axioms

We wish to say that

  D :: * -> * -> *
  axDF1 :: D Int ~ DFInst1
  axDF2 :: D Int Bool ~ DFInst2

overlap. If we conclude that lists of different lengths are SurelyApart, then
it will look like these do *not* overlap, causing disaster. See Trac #9371.

In usages of tcUnifyTys outside of family instances, we always use tcUnifyTys,
which can't tell the difference between MaybeApart and SurelyApart, so those
usages won't notice this design choice.


# Non-idempotent substitution


### Note: Non-idempotent substitution

During unification we use a TvSubstEnv/CvSubstEnv pair that is
  (a) non-idempotent
  (b) loop-free; ie repeatedly applying it yields a fixed point

### Note: Finding the substitution fixpoint

Finding the fixpoint of a non-idempotent substitution arising from a
unification is harder than it looks, because of kinds.  Consider
   T k (H k (f:k)) ~ T * (g:*)
If we unify, we get the substitution
   [ k -> *
   , g -> H k (f:k) ]
To make it idempotent we don't want to get just
   [ k -> *
   , g -> H * (f:k) ]
We also want to substitute inside f's kind, to get
   [ k -> *
   , g -> H k (f:*) ]
If we don't do this, we may apply the substitution to something,
and get an ill-formed type, i.e. one where typeKind will fail.
This happened, for example, in Trac #9106.

This is the reason for extending env with [f:k -> f:*], in the
definition of env' in niFixTvSubst


# unify_ty: the main workhorse


### Note: Specification of unification

### Note: Non-trivial definitional equality

Unlike the "impure unifiers" in the typechecker (the eager unifier in
TcUnify, and the constraint solver itself in TcCanonical), the pure
unifier It does /not/ work up to ~.

### Note: Flattening

Notation:
 θ,φ    substitutions
 ξ    type-function-free types
 τ,σ  other types
 τ♭   type τ, flattened

 ≡    eqType

(U1) Soundness.
     If (unify τ₁ τ₂) = Unifiable θ, then θ(τ₁) ≡ θ(τ₂).
     θ is a most general unifier for τ₁ and τ₂.

(U2) Completeness.
     If (unify ξ₁ ξ₂) = SurelyApart,
     then there exists no substitution θ such that θ(ξ₁) ≡ θ(ξ₂).

These two properties are stated as Property 11 in the "Closed Type Families"
paper (POPL'14). Below, this paper is called [CTF].

(U3) Apartness under substitution.
     If (unify ξ τ♭) = SurelyApart, then (unify ξ θ(τ)♭) = SurelyApart,
     for any θ. (Property 12 from [CTF])

(U4) Apart types do not unify.
     If (unify ξ τ♭) = SurelyApart, then there exists no θ
     such that θ(ξ) = θ(τ). (Property 13 from [CTF])

THEOREM. Completeness w.r.t ~
    If (unify τ₁♭ τ₂♭) = SurelyApart,
    then there exists no proof that (τ₁ ~ τ₂).

PROOF. See appendix of [CTF].


The unification algorithm is used for type family injectivity, as described
in the "Injective Type Families" paper (Haskell'15), called [ITF]. When run
in this mode, it has the following properties.

(I1) If (unify σ τ) = SurelyApart, then σ and τ are not unifiable, even
     after arbitrary type family reductions. Note that σ and τ are
     not flattened here.

(I2) If (unify σ τ) = MaybeApart θ, and if some
     φ exists such that φ(σ) ~ φ(τ), then φ extends θ.


Furthermore, the RULES matching algorithm requires this property,
but only when using this algorithm for matching:

(M1) If (match σ τ) succeeds with θ, then all matchable tyvars
     in σ are bound in θ.

### Note: Self-substitution when matching

(M2) Completeness of matching.
     If θ(σ) = τ, then (match σ τ) = Unifiable φ,
     where θ is an extension of φ.

Sadly, property M2 and I2 conflict. Consider

type family F1 a b where
  F1 Int    Bool   = Char
  F1 Double String = Char

Consider now two matching problems:

P1. match (F1 a Bool) (F1 Int Bool)
P2. match (F1 a Bool) (F1 Double String)

In case P1, we must find (a ↦ Int) to satisfy M2.
In case P2, we must /not/ find (a ↦ Double), in order to satisfy I2. (Note
that the correct mapping for I2 is (a ↦ Int). There is no way to discover
this, but we musn't map a to anything else!)

We thus must parameterize the algorithm over whether it's being used
for an injectivity check (refrain from looking at non-injective arguments
to type families) or not (do indeed look at those arguments).  This is
implemented  by the uf_inj_tf field of UmEnv.

(It's all a question of whether or not to include equation (7) from Fig. 2
of [ITF].)

This extra parameter is a bit fiddly, perhaps, but seemingly less so than
having two separate, almost-identical algorithms.

### Note: Self-substitution when matching

What should happen when we're *matching* (not unifying) a1 with a1? We
should get a substitution [a1 |-> a1]. A successful match should map all
the template variables (except ones that disappear when expanding synonyms).
But when unifying, we don't want to do this, because we'll then fall into
a loop.

This arrangement affects the code in three places:
 - If we're matching a refined template variable, don't recur. Instead, just
   check for equality. That is, if we know [a |-> Maybe a] and are matching
   (a ~? Maybe Int), we want to just fail.

 - Skip the occurs check when matching. This comes up in two places, because
   matching against variables is handled separately from matching against
   full-on types.

Note that this arrangement was provoked by a real failure, where the same
unique ended up in the template as in the target. (It was a rule firing when
compiling Data.List.NonEmpty.)

### Note: Matching coercion variables

Consider this:

   type family F a

   data G a where
     MkG :: F a ~ Bool => G a

   type family Foo (x :: G a) :: F a
   type instance Foo MkG = False

We would like that to be accepted. For that to work, we need to introduce
a coercion variable on the left and then use it on the right. Accordingly,
at use sites of Foo, we need to be able to use matching to figure out the
value for the coercion. (See the desugared version:

   axFoo :: [a :: *, c :: F a ~ Bool]. Foo (MkG c) = False |> (sym c)

) We never want this action to happen during *unification* though, when
all bets are off.

### Note: Kind coercions in Unify

We wish to match/unify while ignoring casts. But, we can't just ignore
them completely, or we'll end up with ill-kinded substitutions. For example,
say we're matching `a` with `ty |> co`. If we just drop the cast, we'll
return [a |-> ty], but `a` and `ty` might have different kinds. We can't
just match/unify their kinds, either, because this might gratuitously
fail. After all, `co` is the witness that the kinds are the same -- they
may look nothing alike.

So, we pass a kind coercion to the match/unify worker. This coercion witnesses
the equality between the substed kind of the left-hand type and the substed
kind of the right-hand type. Note that we do not unify kinds at the leaves
(as we did previously). We thus have

INVARIANT: In the call
    unify_ty ty1 ty2 kco
it must be that subst(kco) :: subst(kind(ty1)) ~N subst(kind(ty2)), where
`subst` is the ambient substitution in the UM monad.

To get this coercion, we first have to match/unify
the kinds before looking at the types. Happily, we need look only one level
up, as all kinds are guaranteed to have kind *.

When we're working with type applications (either TyConApp or AppTy) we
need to worry about establishing INVARIANT, as the kinds of the function
& arguments aren't (necessarily) included in the kind of the result.
When unifying two TyConApps, this is easy, because the two TyCons are
the same. Their kinds are thus the same. As long as we unify left-to-right,
we'll be sure to unify types' kinds before the types themselves. (For example,
think about Proxy :: forall k. k -> *. Unifying the first args matches up
the kinds of the second args.)

For AppTy, we must unify the kinds of the functions, but once these are
unified, we can continue unifying arguments without worrying further about
kinds.

The interface to this module includes both "...Ty" functions and
"...TyKi" functions. The former assume that INVARIANT is already
established, either because the kinds are the same or because the
list of types being passed in are the well-typed arguments to some
type constructor (see two paragraphs above). The latter take a separate
pre-pass over the kinds to establish INVARIANT. Sometimes, it's important
not to take the second pass, as it caused #12442.

We thought, at one point, that this was all unnecessary: why should
casts be in types in the first place? But they are sometimes. In
dependent/should_compile/KindEqualities2, we see, for example the
constraint Num (Int |> (blah ; sym blah)).  We naturally want to find
a dictionary for that constraint, which requires dealing with
coercions in this manner.

### Note: Matching in the presence of casts

When matching, it is crucial that no variables from the template
end up in the range of the matching substitution (obviously!).
When unifying, that's not a constraint; instead we take the fixpoint
of the substitution at the end.

So what should we do with this, when matching?
   unify_ty (tmpl |> co) tgt kco

Previously, wrongly, we pushed 'co' in the (horrid) accumulating
'kco' argument like this:
   unify_ty (tmpl |> co) tgt kco
     = unify_ty tmpl tgt (kco ; co)

But that is obviously wrong because 'co' (from the template) ends
up in 'kco', which in turn ends up in the range of the substitution.

This all came up in Trac #13910.  Because we match tycon arguments
left-to-right, the ambient substitution will already have a matching
substitution for any kinds; so there is an easy fix: just apply
the substitution-so-far to the coercion from the LHS.

Note that

* When matching, the first arg of unify_ty is always the template;
  we never swap round.

* The above argument is distressingly indirect. We seek a
  better way.

* One better way is to ensure that type patterns (the template
  in the matching process) have no casts.  See Trac #14119.



# 

# Unification monad


# 

This section defines essentially an inverse to liftCoSubst. It is defined
here to avoid a dependency from Coercion on this module.

