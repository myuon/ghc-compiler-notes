Note [tcMatchTy vs tcMatchTyKi]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This module offers two variants of matching: with kinds and without.
The TyKi variant takes two types, of potentially different kinds,
and matches them. Along the way, it necessarily also matches their
kinds. The Ty variant instead assumes that the kinds are already
eqType and so skips matching up the kinds.

How do you choose between them?

1. If you know that the kinds of the two types are eqType, use
   the Ty variant. It is more efficient, as it does less work.

2. If the kinds of variables in the template type might mention type families,
   use the Ty variant (and do other work to make sure the kinds
   work out). These pure unification functions do a straightforward
   syntactic unification and do no complex reasoning about type
   families. Note that the types of the variables in instances can indeed
   mention type families, so instance lookup must use the Ty variant.

   (Nothing goes terribly wrong -- no panics -- if there might be type
   families in kinds in the TyKi variant. You just might get match
   failure even though a reducing a type family would lead to success.)

3. Otherwise, if you're sure that the variable kinds do not mention
   type families and you're not already sure that the kind of the template
   equals the kind of the target, then use the TyKi version.


Note [Pruning dead case alternatives]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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


Note [Fine-grained unification]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

See also #8162.

It's worth noting that unification in the presence of infinite types is not
complete. This means that, sometimes, a closed type family does not reduce
when it should. See test case indexed-types/should_fail/Overlap15 for an
example.



Note [The substitution in MaybeApart]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The constructor MaybeApart carries data with it, typically a TvSubstEnv. Why?
Because consider unifying these:

(a, a, Int) ~ (b, [b], Bool)

If we go left-to-right, we start with [a |-> b]. Then, on the middle terms, we
apply the subst we have so far and discover that we need [b |-> [b]]. Because
this fails the occurs check, we say that the types are MaybeApart (see above
Note [Fine-grained unification]). But, we can't stop there! Because if we
continue, we discover that Int is SurelyApart from Bool, and therefore the
types are apart. This has practical consequences for the ability for closed
type family applications to reduce. See test case
indexed-types/should_compile/Overlap14.



Note [Unifying with skolems]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If we discover that two types unify if and only if a skolem variable is
substituted, we can't properly unify the types. But, that skolem variable
may later be instantiated with a unifyable type. So, we return maybeApart
in these cases.


Note [Non-idempotent substitution]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
During unification we use a TvSubstEnv/CvSubstEnv pair that is
  (a) non-idempotent
  (b) loop-free; ie repeatedly applying it yields a fixed point



Note [Finding the substitution fixpoint]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Finding the fixpoint of a non-idempotent substitution arising from a
unification is much trickier than it looks, because of kinds.  Consider
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
This happened, for example, in #9106.

It gets worse.  In #14164 we wanted to take the fixpoint of
this substitution
   [ xs_asV :-> F a_aY6 (z_aY7 :: a_aY6)
                        (rest_aWF :: G a_aY6 (z_aY7 :: a_aY6))
   , a_aY6  :-> a_aXQ ]

We have to apply the substitution for a_aY6 two levels deep inside
the invocation of F!  We don't have a function that recursively
applies substitutions inside the kinds of variable occurrences (and
probably rightly so).

So, we work as follows:

 1. Start with the current substitution (which we are
    trying to fixpoint
       [ xs :-> F a (z :: a) (rest :: G a (z :: a))
       , a  :-> b ]

 2. Take all the free vars of the range of the substitution:
       {a, z, rest, b}
    NB: the free variable finder closes over
    the kinds of variable occurrences

 3. If none are in the domain of the substitution, stop.
    We have found a fixpoint.

 4. Remove the variables that are bound by the substitution, leaving
       {z, rest, b}

 5. Do a topo-sort to put them in dependency order:
       [ b :: *, z :: a, rest :: G a z ]

 6. Apply the substitution left-to-right to the kinds of these
    tyvars, extending it each time with a new binding, so we
    finish up with
       [ xs   :-> ..as before..
       , a    :-> b
       , b    :-> b    :: *
       , z    :-> z    :: b
       , rest :-> rest :: G b (z :: b) ]
    Note that rest now has the right kind

 7. Apply this extended substitution (once) to the range of
    the /original/ substitution.  (Note that we do the
    extended substitution would go on forever if you tried
    to find its fixpoint, because it maps z to z.)

 8. And go back to step 1

In Step 6 we use the free vars from Step 2 as the initial
in-scope set, because all of those variables appear in the
range of the substitution, so they must all be in the in-scope
set.  But NB that the type substitution engine does not look up
variables in the in-scope set; it is used only to ensure no
shadowing.


Note [Specification of unification]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The pure unifier, unify_ty, defined in this module, tries to work out
a substitution to make two types say True to eqType. NB: eqType is
itself not purely syntactic; it accounts for CastTys;
see Note [Non-trivial definitional equality] in TyCoRep

Unlike the "impure unifiers" in the typechecker (the eager unifier in
TcUnify, and the constraint solver itself in TcCanonical), the pure
unifier It does /not/ work up to ~.

The algorithm implemented here is rather delicate, and we depend on it
to uphold certain properties. This is a summary of these required
properties. Any reference to "flattening" refers to the flattening
algorithm in FamInstEnv (See Note [Flattening] in FamInstEnv), not
the flattening algorithm in the solver.

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

     Property M1 means that we must extend the substitution with,
     say (a ↦ a) when appropriate during matching.
     See also Note [Self-substitution when matching].

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



Note [Self-substitution when matching]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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



Note [Matching coercion variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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



Note [Kind coercions in Unify]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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



Note [Matching in the presence of casts]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

This all came up in #13910.  Because we match tycon arguments
left-to-right, the ambient substitution will already have a matching
substitution for any kinds; so there is an easy fix: just apply
the substitution-so-far to the coercion from the LHS.

Note that

* When matching, the first arg of unify_ty is always the template;
  we never swap round.

* The above argument is distressingly indirect. We seek a
  better way.

* One better way is to ensure that type patterns (the template
  in the matching process) have no casts.  See #14119.



Note [Polykinded tycon applications]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose  T :: forall k. Type -> K
and we are unifying
  ty1:  T @Type         Int       :: Type
  ty2:  T @(Type->Type) Int Int   :: Type

These two TyConApps have the same TyCon at the front but they
(legitimately) have different numbers of arguments.  They
are surelyApart, so we can report that without looking any
further (see #15704).
------------ unify_ty: the main workhorse -----------
