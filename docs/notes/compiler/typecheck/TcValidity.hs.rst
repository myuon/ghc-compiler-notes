`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcValidity.hs>`_

Note [The ambiguity check for type signatures]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
checkAmbiguity is a check on *user-supplied type signatures*.  It is
*purely* there to report functions that cannot possibly be called.  So for
example we want to reject:
   f :: C a => Int
The idea is there can be no legal calls to 'f' because every call will
give rise to an ambiguous constraint.  We could soundly omit the
ambiguity check on type signatures entirely, at the expense of
delaying ambiguity errors to call sites.  Indeed, the flag
-XAllowAmbiguousTypes switches off the ambiguity check.

What about things like this:
   class D a b | a -> b where ..
   h :: D Int b => Int
The Int may well fix 'b' at the call site, so that signature should
not be rejected.  Moreover, using *visible* fundeps is too
conservative.  Consider
   class X a b where ...
   class D a b | a -> b where ...
   instance D a b => X [a] b where...
   h :: X a b => a -> a
Here h's type looks ambiguous in 'b', but here's a legal call:
   ...(h [True])...
That gives rise to a (X [Bool] beta) constraint, and using the
instance means we need (D Bool beta) and that fixes 'beta' via D's
fundep!

Behind all these special cases there is a simple guiding principle.
Consider

  f :: <type>
  f = ...blah...

  g :: <type>
  g = f

You would think that the definition of g would surely typecheck!
After all f has exactly the same type, and g=f. But in fact f's type
is instantiated and the instantiated constraints are solved against
the originals, so in the case an ambiguous type it won't work.
Consider our earlier example f :: C a => Int.  Then in g's definition,
we'll instantiate to (C alpha) and try to deduce (C alpha) from (C a),
and fail.

So in fact we use this as our *definition* of ambiguity.  We use a
very similar test for *inferred* types, to ensure that they are
unambiguous. See Note [Impedance matching] in TcBinds.

This test is very conveniently implemented by calling
    tcSubType <type> <type>
This neatly takes account of the functional dependecy stuff above,
and implicit parameter (see Note [Implicit parameters and ambiguity]).
And this is what checkAmbiguity does.

What about this, though?
   g :: C [a] => Int
Is every call to 'g' ambiguous?  After all, we might have
   instance C [a] where ...
at the call site.  So maybe that type is ok!  Indeed even f's
quintessentially ambiguous type might, just possibly be callable:
with -XFlexibleInstances we could have
  instance C a where ...
and now a call could be legal after all!  Well, we'll reject this
unless the instance is available *here*.



Note [When to call checkAmbiguity]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We call checkAmbiguity
   (a) on user-specified type signatures
   (b) in checkValidType

Conncerning (b), you might wonder about nested foralls.  What about
    f :: forall b. (forall a. Eq a => b) -> b
The nested forall is ambiguous.  Originally we called checkAmbiguity
in the forall case of check_type, but that had two bad consequences:
  * We got two error messages about (Eq b) in a nested forall like this:
       g :: forall a. Eq a => forall b. Eq b => a -> a
  * If we try to check for ambiguity of a nested forall like
    (forall a. Eq a => b), the implication constraint doesn't bind
    all the skolems, which results in "No skolem info" in error
    messages (see #10432).

To avoid this, we call checkAmbiguity once, at the top, in checkValidType.
(I'm still a bit worried about unbound skolems when the type mentions
in-scope type variables.)

In fact, because of the co/contra-variance implemented in tcSubType,
this *does* catch function f above. too.

Concerning (a) the ambiguity check is only used for *user* types, not
for types coming from inteface files.  The latter can legitimately
have ambiguous types. Example

   class S a where s :: a -> (Int,Int)
   instance S Char where s _ = (1,1)
   f:: S a => [a] -> Int -> (Int,Int)
   f (_::[a]) x = (a*x,b)
        where (a,b) = s (undefined::a)

Here the worker for f gets the type
        fw :: forall a. S a => Int -> (# Int, Int #)




Note [Implicit parameters and ambiguity]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Only a *class* predicate can give rise to ambiguity
An *implicit parameter* cannot.  For example:
        foo :: (?x :: [a]) => Int
        foo = length ?x
is fine.  The call site will supply a particular 'x'

Furthermore, the type variables fixed by an implicit parameter
propagate to the others.  E.g.
        foo :: (Show a, ?x::[a]) => Int
        foo = show (?x++?x)
The type of foo looks ambiguous.  But it isn't, because at a call site
we might have
        let ?x = 5::Int in foo
and all is well.  In effect, implicit parameters are, well, parameters,
so we can take their type variables into account as part of the
"tau-tvs" stuff.  This is done in the function 'FunDeps.grow'.


Note [When we don't check for ambiguity]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In a few places we do not want to check a user-specified type for ambiguity

* GhciCtxt: Allow ambiguous types in GHCi's :kind command
  E.g.   type family T a :: *  -- T :: forall k. k -> *
  Then :k T should work in GHCi, not complain that
  (T k) is ambiguous!

* TySynCtxt: type T a b = C a b => blah
  It may be that when we /use/ T, we'll give an 'a' or 'b' that somehow
  cure the ambiguity.  So we defer the ambiguity check to the use site.

  There is also an implementation reason (#11608).  In the RHS of
  a type synonym we don't (currently) instantiate 'a' and 'b' with
  TcTyVars before calling checkValidType, so we get asertion failures
  from doing an ambiguity check on a type with TyVars in it.  Fixing this
  would not be hard, but let's wait till there's a reason.

* TypeAppCtxt: visible type application
     f @ty
  No need to check ty for ambiguity




Note [Higher rank types]
~~~~~~~~~~~~~~~~~~~~~~~~
Technically
            Int -> forall a. a->a
is still a rank-1 type, but it's not Haskell 98 (#5957).  So the
validity checker allow a forall after an arrow only if we allow it
before -- that is, with Rank2Types or RankNTypes


Note [Correctness and performance of type synonym validity checking]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider the type A arg1 arg2, where A is a type synonym. How should we check
this type for validity? We have three distinct choices, corresponding to the
three constructors of ExpandMode:

1. Expand the application of A, and check the resulting type (`Expand`).
2. Don't expand the application of A. Only check the arguments (`NoExpand`).
3. Check the arguments *and* check the expanded type (`Both`).

It's tempting to think that we could always just pick choice (3), but this
results in serious performance issues when checking a type like in the
signature for `f` below:

  type S = ...
  f :: S (S (S (S (S (S ....(S Int)...))))

When checking the type of `f`, we'll check the outer `S` application with and
without expansion, and in *each* of those checks, we'll check the next `S`
application with and without expansion... the result is exponential blowup! So
clearly we don't want to use `Both` 100% of the time.

On the other hand, neither is it correct to use exclusively `Expand` or
exclusively `NoExpand` 100% of the time:

* If one always expands, then one can miss erroneous programs like the one in
  the `tcfail129` test case:

    type Foo a = String -> Maybe a
    type Bar m = m Int
    blah = undefined :: Bar Foo

  If we expand `Bar Foo` immediately, we'll miss the fact that the `Foo` type
  synonyms is unsaturated.
* If one never expands and only checks the arguments, then one can miss
  erroneous programs like the one in #16059:

    type Foo b = Eq b => b
    f :: forall b (a :: Foo b). Int

  The kind of `a` contains a constraint, which is illegal, but this will only
  be caught if `Foo b` is expanded.

Therefore, it's impossible to have these validity checks be simultaneously
correct and performant if one sticks exclusively to a single `ExpandMode`. In
that case, the solution is to vary the `ExpandMode`s! In more detail:

1. When we start validity checking, we start with `Expand` if
   LiberalTypeSynonyms is enabled (see Note [Liberal type synonyms] for why we
   do this), and we start with `Both` otherwise. The `initialExpandMode`
   function is responsible for this.
2. When expanding an application of a type synonym (in `check_syn_tc_app`), we
   determine which things to check based on the current `ExpandMode` argument.
   Importantly, if the current mode is `Both`, then we check the arguments in
   `NoExpand` mode and check the expanded type in `Both` mode.

   Switching to `NoExpand` when checking the arguments is vital to avoid
   exponential blowup. One consequence of this choice is that if you have
   the following type synonym in one module (with RankNTypes enabled):

     {-# LANGUAGE RankNTypes #-}
     module A where
     type A = forall a. a

   And you define the following in a separate module *without* RankNTypes
   enabled:

     module B where

     import A

     type Const a b = a
     f :: Const Int A -> Int

   Then `f` will be accepted, even though `A` (which is technically a rank-n
   type) appears in its type. We view this as an acceptable compromise, since
   `A` never appears in the type of `f` post-expansion. If `A` _did_ appear in
   a type post-expansion, such as in the following variant:

     g :: Const A A -> Int

   Then that would be rejected unless RankNTypes were enabled.


Note [Unsaturated type synonyms in GHCi]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Generally speaking, GHC disallows unsaturated uses of type synonyms or type
families. For instance, if one defines `type Const a b = a`, then GHC will not
permit using `Const` unless it is applied to (at least) two arguments. There is
an exception to this rule, however: GHCi's :kind command. For instance, it
is quite common to look up the kind of a type constructor like so:

  λ> :kind Const
  Const :: j -> k -> j
  λ> :kind Const Int
  Const Int :: k -> Type

Strictly speaking, the two uses of `Const` above are unsaturated, but this
is an extremely benign (and useful) example of unsaturation, so we allow it
here as a special case.

That being said, we do not allow unsaturation carte blanche in GHCi. Otherwise,
this GHCi interaction would be possible:

  λ> newtype Fix f = MkFix (f (Fix f))
  λ> type Id a = a
  λ> :kind Fix Id
  Fix Id :: Type

This is rather dodgy, so we move to disallow this. We only permit unsaturated
synonyms in GHCi if they are *top-level*—that is, if the synonym is the
outermost type being applied. This allows `Const` and `Const Int` in the
first example, but not `Fix Id` in the second example, as `Id` is not the
outermost type being applied (`Fix` is).

We track this outermost property in the GhciCtxt constructor of UserTypeCtxt.
A field of True in GhciCtxt indicates that we're in an outermost position. Any
time we invoke `check_arg` to check the validity of an argument, we switch the
field to False.
--------------------------------------


Note [Type variables escaping through kinds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider:

  type family T (r :: RuntimeRep) :: TYPE r
  foo :: forall r. T r

Something smells funny about the type of `foo`. If you spell out the kind
explicitly, it becomes clearer from where the smell originates:

  foo :: ((forall r. T r) :: TYPE r)

The type variable `r` appears in the result kind, which escapes the scope of
its binding site! This is not desirable, so we establish a validity check
(`checkEscapingKind`) to catch any type variables that might escape through
kinds in this way.


Note [Liberal type synonyms]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If -XLiberalTypeSynonyms is on, expand closed type synonyms *before*
doing validity checking.  This allows us to instantiate a synonym defn
with a for-all type, or with a partially-applied type synonym.
        e.g.   type T a b = a
               type S m   = m ()
               f :: S (T Int)
Here, T is partially applied, so it's illegal in H98.  But if you
expand S first, then T we get just
               f :: Int
which is fine.

IMPORTANT: suppose T is a type synonym.  Then we must do validity
checking on an appliation (T ty1 ty2)

        *either* before expansion (i.e. check ty1, ty2)
        *or* after expansion (i.e. expand T ty1 ty2, and then check)
        BUT NOT BOTH

If we do both, we get exponential behaviour!!

  data TIACons1 i r c = c i ::: r c
  type TIACons2 t x = TIACons1 t (TIACons1 t x)
  type TIACons3 t x = TIACons2 t (TIACons1 t x)
  type TIACons4 t x = TIACons2 t (TIACons2 t x)
  type TIACons7 t x = TIACons4 t (TIACons3 t x)

The order in which you do validity checking is also somewhat delicate. Consider
the `check_type` function, which drives the validity checking for unsaturated
uses of type synonyms. There is a special case for rank-n types, such as
(forall x. x -> x) or (Show x => x), since those require at least one language
extension to use. It used to be the case that this case came before every other
case, but this can lead to bugs. Imagine you have this scenario (from #15954):

  type A a = Int
  type B (a :: Type -> Type) = forall x. x -> x
  type C = B A

If the rank-n case came first, then in the process of checking for `forall`s
or contexts, we would expand away `B A` to `forall x. x -> x`. This is because
the functions that split apart `forall`s/contexts
(tcSplitForAllVarBndrs/tcSplitPhiTy) expand type synonyms! If `B A` is expanded
away to `forall x. x -> x` before the actually validity checks occur, we will
have completely obfuscated the fact that we had an unsaturated application of
the `A` type synonym.

We have since learned from our mistakes and now put this rank-n case /after/
the case for TyConApp, which ensures that an unsaturated `A` TyConApp will be
caught properly. But be careful! We can't make the rank-n case /last/ either,
as the FunTy case must came after the rank-n case. Otherwise, something like
(Eq a => Int) would be treated as a function type (FunTy), which just
wouldn't do.



Note [Implicit parameters in instance decls]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Implicit parameters _only_ allowed in type signatures; not in instance
decls, superclasses etc. The reason for not allowing implicit params in
instances is a bit subtle.  If we allowed
  instance (?x::Int, Eq a) => Foo [a] where ...
then when we saw
     (e :: (?x::Int) => t)
it would be unclear how to discharge all the potential uses of the ?x
in e.  For example, a constraint Foo [Int] might come out of e, and
applying the instance decl would show up two uses of ?x.  #8912.


Note [Validity checking for constraints]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We look through constraint synonyms so that we can see the underlying
constraint(s).  For example
   type Foo = ?x::Int
   instance Foo => C T
We should reject the instance because it has an implicit parameter in
the context.

But we record, in 'under_syn', whether we have looked under a synonym
to avoid requiring language extensions at the use site.  Main example
(#9838):

   {-# LANGUAGE ConstraintKinds #-}
   module A where
      type EqShow a = (Eq a, Show a)

   module B where
      import A
      foo :: EqShow a => a -> String

We don't want to require ConstraintKinds in module B.


Note [ConstraintKinds in predicates]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Don't check for -XConstraintKinds under a type synonym, because that
was done at the type synonym definition site; see #9838
e.g.   module A where
          type C a = (Eq a, Ix a)   -- Needs -XConstraintKinds
       module B where
          import A
          f :: C a => a -> a        -- Does *not* need -XConstraintKinds



Note [Irreducible predicates in superclasses]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Allowing type-family calls in class superclasses is somewhat dangerous
because we can write:

 type family Fooish x :: * -> Constraint
 type instance Fooish () = Foo
 class Fooish () a => Foo a where

This will cause the constraint simplifier to loop because every time we canonicalise a
(Foo a) class constraint we add a (Fooish () a) constraint which will be immediately
solved to add+canonicalise another (Foo a) constraint.  -----------------------


Note [Simplifiable given constraints]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A type signature like
   f :: Eq [(a,b)] => a -> b
is very fragile, for reasons described at length in TcInteract
Note [Instance and Given overlap].  As that Note discusses, for the
most part the clever stuff in TcInteract means that we don't use a
top-level instance if a local Given might fire, so there is no
fragility. But if we /infer/ the type of a local let-binding, things
can go wrong (#11948 is an example, discussed in the Note).

So this warning is switched on only if we have NoMonoLocalBinds; in
that case the warning discourages users from writing simplifiable
class constraints.

The warning only fires if the constraint in the signature
matches the top-level instances in only one way, and with no
unifiers -- that is, under the same circumstances that
TcInteract.matchInstEnv fires an interaction with the top
level instances.  For example (#13526), consider

  instance {-# OVERLAPPABLE #-} Eq (T a) where ...
  instance                   Eq (T Char) where ..
  f :: Eq (T a) => ...

We don't want to complain about this, even though the context
(Eq (T a)) matches an instance, because the user may be
deliberately deferring the choice so that the Eq (T Char)
has a chance to fire when 'f' is called.  And the fragility
only matters when there's a risk that the instance might
fire instead of the local 'given'; and there is no such
risk in this case.  Just use the same rules as for instance
firing!
-----------------------


Note [Kind polymorphic type classes]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
MultiParam check:

    class C f where...   -- C :: forall k. k -> Constraint
    instance C Maybe where...

  The dictionary gets type [C * Maybe] even if it's not a MultiParam
  type class.

Flexibility check:

    class C f where...   -- C :: forall k. k -> Constraint
    data D a = D a
    instance C D where

  The dictionary gets type [C * (D *)]. IA0_TODO it should be
  generalized actually.


Note [Instances of built-in classes in signature files]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

User defined instances for KnownNat, KnownSymbol and Typeable are
disallowed -- they are generated when needed by GHC itself on-the-fly.

However, if they occur in a Backpack signature file, they have an
entirely different meaning. Suppose in M.hsig we see

  signature M where
    data T :: Nat
    instance KnownNat T

That says that any module satisfying M.hsig must provide a KnownNat
instance for T.  We absolultely need that instance when compiling a
module that imports M.hsig: see #15379 and
Note [Fabricating Evidence for Literals in Backpack] in ClsInst.

Hence, checkValidInstHead accepts a user-written instance declaration
in hsig files, where `is_sig` is True.



Note [Casts during validity checking]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider the (bogus)
     instance Eq Char#
We elaborate to  'Eq (Char# |> UnivCo(hole))'  where the hole is an
insoluble equality constraint for * ~ #.  We'll report the insoluble
constraint separately, but we don't want to *also* complain that Eq is
not applied to a type constructor.  So we look gaily look through
CastTys here.

Another example:  Eq (Either a).  Then we actually get a cast in
the middle:
   Eq ((Either |> g) a)




Note [Validity checking of HasField instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The HasField class has magic constraint solving behaviour (see Note
[HasField instances] in TcInteract).  However, we permit users to
declare their own instances, provided they do not clash with the
built-in behaviour.  In particular, we forbid:

  1. `HasField _ r _` where r is a variable

  2. `HasField _ (T ...) _` if T is a data family
     (because it might have fields introduced later)

  3. `HasField x (T ...) _` where x is a variable,
      if T has any fields at all

  4. `HasField "foo" (T ...) _` if T has a "foo" field

The usual functional dependency checks also apply.




Note [Valid 'deriving' predicate]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
validDerivPred checks for OK 'deriving' context.  See Note [Exotic
derived instance contexts] in TcDeriv.  However the predicate is
here because it uses sizeTypes, fvTypes.

It checks for three things

  * No repeated variables (hasNoDups fvs)

  * No type constructors.  This is done by comparing
        sizeTypes tys == length (fvTypes tys)
    sizeTypes counts variables and constructors; fvTypes returns variables.
    So if they are the same, there must be no constructors.  But there
    might be applications thus (f (g x)).

    Note that tys only includes the visible arguments of the class type
    constructor. Including the non-visible arguments can cause the following,
    perfectly valid instance to be rejected:
       class Category (cat :: k -> k -> *) where ...
       newtype T (c :: * -> * -> *) a b = MkT (c a b)
       instance Category c => Category (T c) where ...
    since the first argument to Category is a non-visible *, which sizeTypes
    would count as a constructor! See #11833.

  * Also check for a bizarre corner case, when the derived instance decl
    would look like
       instance C a b => D (T a) where ...
    Note that 'b' isn't a parameter of T.  This gives rise to all sorts of
    problems; in particular, it's hard to compare solutions for equality
    when finding the fixpoint, and that means the inferContext loop does
    not converge.  See #5287.



Note [Equality class instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We can't have users writing instances for the equality classes. But we
still need to be able to write instances for them ourselves. So we allow
instances only in the defining module.



Note [Instances and constraint synonyms]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Currently, we don't allow instances for constraint synonyms at all.
Consider these (#13267):
  type C1 a = Show (a -> Bool)
  instance C1 Int where    -- I1
    show _ = "ur"

This elicits "show is not a (visible) method of class C1", which isn't
a great message. But it comes from the renamer, so it's hard to improve.

This needs a bit more care:
  type C2 a = (Show a, Show Int)
  instance C2 Int           -- I2

If we use (splitTyConApp_maybe tau) in checkValidInstance to decompose
the instance head, we'll expand the synonym on fly, and it'll look like
  instance (%,%) (Show Int, Show Int)
and we /really/ don't want that.  So we carefully do /not/ expand
synonyms, by matching on TyConApp directly.


Note [Paterson conditions]
~~~~~~~~~~~~~~~~~~~~~~~~~~
Termination test: the so-called "Paterson conditions" (see Section 5 of
"Understanding functional dependencies via Constraint Handling Rules,
JFP Jan 2007).

We check that each assertion in the context satisfies:
 (1) no variable has more occurrences in the assertion than in the head, and
 (2) the assertion has fewer constructors and variables (taken together
     and counting repetitions) than the head.
This is only needed with -fglasgow-exts, as Haskell 98 restrictions
(which have already been checked) guarantee termination.

The underlying idea is that

    for any ground substitution, each assertion in the
    context has fewer type constructors than the head.


Note [Type families in instance contexts]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Are these OK?
  type family F a
  instance F a    => C (Maybe [a]) where ...
  intance C (F a) => C [[[a]]]     where ...

No: the type family in the instance head might blow up to an
arbitrarily large type, depending on how 'a' is instantiated.
So we require UndecidableInstances if we have a type family
in the instance head.  #15172.



Note [Invisible arguments and termination]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When checking the ​Paterson conditions for termination an instance
declaration, we check for the number of "constructors and variables"
in the instance head and constraints. Question: Do we look at

 * All the arguments, visible or invisible?
 * Just the visible arguments?

I think both will ensure termination, provided we are consistent.
Currently we are /not/ consistent, which is really a bug.  It's
described in #15177, which contains a number of examples.
The suspicious bits are the calls to filterOutInvisibleTypes.


Note [Check type-family instance binders]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In a type family instance, we require (of course), type variables
used on the RHS are matched on the LHS. This is checked by
checkFamPatBinders.  Here is an interesting example:

    type family   T :: k
    type instance T = (Nothing :: Maybe a)

Upon a cursory glance, it may appear that the kind variable `a` is
free-floating above, since there are no (visible) LHS patterns in
`T`. However, there is an *invisible* pattern due to the return kind,
so inside of GHC, the instance looks closer to this:

    type family T @k :: k
    type instance T @(Maybe a) = (Nothing :: Maybe a)

Here, we can see that `a` really is bound by a LHS type pattern, so `a` is in
fact not unbound. Contrast that with this example (#13985)

    type instance T = Proxy (Nothing :: Maybe a)

This would looks like this inside of GHC:

    type instance T @(*) = Proxy (Nothing :: Maybe a)

So this time, `a` is neither bound by a visible nor invisible type pattern on
the LHS, so it would be reported as free-floating.

Finally, here's one more brain-teaser (from #9574). In the example below:

    class Funct f where
      type Codomain f :: *
    instance Funct ('KProxy :: KProxy o) where
      type Codomain 'KProxy = NatTr (Proxy :: o -> *)

As it turns out, `o` is not free-floating in this example. That is because `o`
bound by the kind signature of the LHS type pattern 'KProxy. To make this more
obvious, one can also write the instance like so:

    instance Funct ('KProxy :: KProxy o) where
      type Codomain ('KProxy :: KProxy o) = NatTr (Proxy :: o -> *)




Note [Matching in the consistent-instantation check]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Matching the class-instance header to family-instance tyvars is
tricker than it sounds.  Consider (#13972)
    class C (a :: k) where
      type T k :: Type
    instance C Left where
      type T (a -> Either a b) = Int

Here there are no lexically-scoped variables from (C Left).
Yet the real class-instance header is   C @(p -> Either @p @q)) (Left @p @q)
while the type-family instance is       T (a -> Either @a @b)
So we allow alpha-renaming of variables that don't come
from the class-instance header.

We track the lexically-scoped type variables from the
class-instance header in ai_tyvars.

Here's another example (#14045a)
    class C (a :: k) where
      data S (a :: k)
    instance C (z :: Bool) where
      data S :: Bool -> Type where

Again, there is no lexical connection, but we will get
   class-instance header:   C @Bool (z::Bool)
   family instance          S @Bool (a::Bool)

When looking for mis-matches, we check left-to-right,
kinds first.  If we look at types first, we'll fail to
suggest -fprint-explicit-kinds for a mis-match with
      T @k    vs    T @Type
somewhere deep inside the type



Note [Checking consistent instantiation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
See #11450 for background discussion on this check.

  class C a b where
    type T a x b

With this class decl, if we have an instance decl
  instance C ty1 ty2 where ...
then the type instance must look like
     type T ty1 v ty2 = ...
with exactly 'ty1' for 'a', 'ty2' for 'b', and some type 'v' for 'x'.
For example:

  instance C [p] Int
    type T [p] y Int = (p,y,y)

Note that

* We used to allow completely different bound variables in the
  associated type instance; e.g.
    instance C [p] Int
      type T [q] y Int = ...
  But from GHC 8.2 onwards, we don't.  It's much simpler this way.
  See #11450.

* When the class variable isn't used on the RHS of the type instance,
  it's tempting to allow wildcards, thus
    instance C [p] Int
      type T [_] y Int = (y,y)
  But it's awkward to do the test, and it doesn't work if the
  variable is repeated:
    instance C (p,p) Int
      type T (_,_) y Int = (y,y)
  Even though 'p' is not used on the RHS, we still need to use 'p'
  on the LHS to establish the repeated pattern.  So to keep it simple
  we just require equality.

* For variables in associated type families that are not bound by the class
  itself, we do _not_ check if they are over-specific. In other words,
  it's perfectly acceptable to have an instance like this:

    instance C [p] Int where
      type T [p] (Maybe x) Int = x

  While the first and third arguments to T are required to be exactly [p] and
  Int, respectively, since they are bound by C, the second argument is allowed
  to be more specific than just a type variable. Furthermore, it is permissible
  to define multiple equations for T that differ only in the non-class-bound
  argument:

    instance C [p] Int where
      type T [p] (Maybe x)    Int = x
      type T [p] (Either x y) Int = x -> y

  We once considered requiring that non-class-bound variables in associated
  type family instances be instantiated with distinct type variables. However,
  that requirement proved too restrictive in practice, as there were examples
  of extremely simple associated type family instances that this check would
  reject, and fixing them required tiresome boilerplate in the form of
  auxiliary type families. For instance, you would have to define the above
  example as:

    instance C [p] Int where
      type T [p] x Int = CAux x

    type family CAux x where
      CAux (Maybe x)    = x
      CAux (Either x y) = x -> y

  We decided that this restriction wasn't buying us much, so we opted not
  to pursue that design (see also GHC #13398).

Implementation
  * Form the mini-envt from the class type variables a,b
    to the instance decl types [p],Int:   [a->[p], b->Int]

  * Look at the tyvars a,x,b of the type family constructor T
    (it shares tyvars with the class C)

  * Apply the mini-evnt to them, and check that the result is
    consistent with the instance types [p] y Int. (where y can be any type, as
    it is not scoped over the class type variables.

We make all the instance type variables scope over the
type instances, of course, which picks up non-obvious kinds.  Eg
   class Foo (a :: k) where
      type F a
   instance Foo (b :: k -> k) where
      type F b = Int
Here the instance is kind-indexed and really looks like
      type F (k->k) (b::k->k) = Int
But if the 'b' didn't scope, we would make F's instance too
poly-kinded.



Note [Printing conflicts with class header]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It's remarkably painful to give a decent error message for conflicts
with the class header.  Consider
   clase C b where
     type F a b c
   instance C [b] where
     type F x Int _ _ = ...

Here we want to report a conflict between
    Expected: F _ [b] _
    Actual:   F x Int _ _

But if the type instance shadows the class variable like this
(rename/should_fail/T15828):
   instance C [b] where
     type forall b. F x (Tree b) _ _ = ...

then we must use a fresh variable name
    Expected: F _ [b] _
    Actual:   F x [b1] _ _

Notice that:
  - We want to print an underscore in the "Expected" type in
    positions where the class header has no influence over the
    parameter.  Hence the fancy footwork in pp_expected_ty

  - Although the binders in the axiom are aready tidy, we must
    re-tidy them to get a fresh variable name when we shadow

  - The (ax_tvs \\ inst_tvs) is to avoid tidying one of the
    class-instance variables a second time, from 'a' to 'a1' say.
    Remember, the ax_tvs of the axiom share identity with the
    class-instance variables, inst_tvs..

  - We use tidyCoAxBndrsForUser to get underscores rather than
    _1, _2, etc in the axiom tyvars; see the definition of
    tidyCoAxBndrsForUser

This all seems absurdly complicated.



Note [Unused explicitly bound variables in a family pattern]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Why is 'unusedExplicitForAllErr' not just a warning?

Consider the following examples:

  type instance F a = Maybe b
  type instance forall b. F a = Bool
  type instance forall b. F a = Maybe b

In every case, b is a type variable not determined by the LHS pattern. The
first is caught by the renamer, but we catch the last two here. Perhaps one
could argue that the second should be accepted, albeit with a warning, but
consider the fact that in a type family instance, there is no way to interact
with such a varable. At least with @x :: forall a. Int@ we can use visibile
type application, like @x \@Bool 1@. (Of course it does nothing, but it is
permissible.) In the type family case, the only sensible explanation is that
the user has made a mistake -- thus we throw an error.



Note [Oversaturated type family equations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Type family tycons have very rigid arities. We want to reject something like
this:

  type family Foo :: Type -> Type where
    Foo x = ...

Because Foo has arity zero (i.e., it doesn't bind anything to the left of the
double colon), we want to disallow any equation for Foo that has more than zero
arguments, such as `Foo x = ...`. The algorithm here is pretty simple: if an
equation has more arguments than the arity of the type family, reject.

Things get trickier when visible kind application enters the picture. Consider
the following example:

  type family Bar (x :: j) :: forall k. Either j k where
    Bar 5 @Symbol = ...

The arity of Bar is two, since it binds two variables, `j` and `x`. But even
though Bar's equation has two arguments, it's still invalid. Imagine the same
equation in Core:

    Bar Nat 5 Symbol = ...

Here, it becomes apparent that Bar is actually taking /three/ arguments! So
we can't just rely on a simple counting argument to reject
`Bar 5 @Symbol = ...`, since it only has two user-written arguments.
Moreover, there's one explicit argument (5) and one visible kind argument
(@Symbol), which matches up perfectly with the fact that Bar has one required
binder (x) and one specified binder (j), so that's not a valid way to detect
oversaturation either.

To solve this problem in a robust way, we do the following:

1. When kind-checking, we count the number of user-written *required*
   arguments and check if there is an equal number of required tycon binders.
   If not, reject. (See `wrongNumberOfParmsErr` in TcTyClsDecls.)

   We perform this step during kind-checking, not during validity checking,
   since we can give better error messages if we catch it early.
2. When validity checking, take all of the (Core) type patterns from on
   equation, drop the first n of them (where n is the arity of the type family
   tycon), and check if there are any types leftover. If so, reject.

   Why does this work? We know that after dropping the first n type patterns,
   none of the leftover types can be required arguments, since step (1) would
   have already caught that. Moreover, the only places where visible kind
   applications should be allowed are in the first n types, since those are the
   only arguments that can correspond to binding forms. Therefore, the
   remaining arguments must correspond to oversaturated uses of visible kind
   applications, which are precisely what we want to reject.

Note that we only perform this check for type families, and not for data
families. This is because it is perfectly acceptable to oversaturate data
family instance equations: see Note [Arity of data families] in FamInstEnv.



Note [Bad TyCon telescopes]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
Now that we can mix type and kind variables, there are an awful lot of
ways to shoot yourself in the foot. Here are some.

  data SameKind :: k -> k -> *   -- just to force unification

1.  data T1 a k (b :: k) (x :: SameKind a b)

The problem here is that we discover that a and b should have the same
kind. But this kind mentions k, which is bound *after* a.
(Testcase: dependent/should_fail/BadTelescope)

2.  data T2 a (c :: Proxy b) (d :: Proxy a) (x :: SameKind b d)

Note that b is not bound. Yet its kind mentions a. Because we have
a nice rule that all implicitly bound variables come before others,
this is bogus.

To catch these errors, we call checkTyConTelescope during kind-checking
datatype declarations.  This checks for

* Ill-scoped binders. From (1) and (2) above we can get putative
  kinds like
       T1 :: forall (a:k) (k:*) (b:k). SameKind a b -> *
  where 'k' is mentioned a's kind before k is bound

  This is easy to check for: just look for
  out-of-scope variables in the kind

* We should arguably also check for ambiguous binders
  but we don't.  See Note [Ambiguous kind vars].

See also
  * Note [Required, Specified, and Inferred for types] in TcTyClsDecls.
  * Note [Keeping scoped variables in order: Explicit] discusses how
    this check works for `forall x y z.` written in a type.



Note [Ambiguous kind vars]
~~~~~~~~~~~~~~~~~~~~~~~~~~
We used to be concerned about ambiguous binders. Suppose we have the kind
     S1 :: forall k -> * -> *
     S2 :: forall k. * -> *
Here S1 is OK, because k is Required, and at a use of S1 we will
see (S1 *) or (S1 (*->*)) or whatever.

But S2 is /not/ OK because 'k' is Specfied (and hence invisible) and
we have no way (ever) to figure out how 'k' should be instantiated.
For example if we see (S2 Int), that tells us nothing about k's
instantiation.  (In this case we'll instantiate it to Any, but that
seems wrong.)  This is really the same test as we make for ambiguous
type in term type signatures.

Now, it's impossible for a Specified variable not to occur
at all in the kind -- after all, it is Specified so it must have
occurred.  (It /used/ to be possible; see tests T13983 and T7873.  But
with the advent of the forall-or-nothing rule for kind variables,
those strange cases went away.)

But one might worry about
    type v k = *
    S3 :: forall k. V k -> *
which appears to mention 'k' but doesn't really.  Or
    S4 :: forall k. F k -> *
where F is a type function.  But we simply don't check for
those cases of ambiguity, yet anyway.  The worst that can happen
is ambiguity at the call sites.

Historical note: this test used to be called reportFloatingKvs.

