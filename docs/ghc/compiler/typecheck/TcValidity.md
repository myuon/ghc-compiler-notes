[[src]](https://github.com/ghc/ghc/tree/master/compiler/typecheck/TcValidity.hs)

(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998


# Checking for ambiguity


### Note: The ambiguity check for type signatures

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

### Note: Impedance matching

### Note: Implicit parameters and ambiguity

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

### Note: When to call checkAmbiguity

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
    messages (see Trac #10432).

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

### Note: Implicit parameters and ambiguity

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


### Note: When we don't check for ambiguity

In a few places we do not want to check a user-specified type for ambiguity

* GhciCtxt: Allow ambiguous types in GHCi's :kind command
  E.g.   type family T a :: *  -- T :: forall k. k -> *
  Then :k T should work in GHCi, not complain that
  (T k) is ambiguous!

* TySynCtxt: type T a b = C a b => blah
  It may be that when we /use/ T, we'll give an 'a' or 'b' that somehow
  cure the ambiguity.  So we defer the ambiguity check to the use site.

  There is also an implementation reason (Trac #11608).  In the RHS of
  a type synonym we don't (currently) instantiate 'a' and 'b' with
  TcTyVars before calling checkValidType, so we get asertion failures
  from doing an ambiguity check on a type with TyVars in it.  Fixing this
  would not be hard, but let's wait till there's a reason.

* TypeAppCtxt: visible type application
     f @ty
  No need to check ty for ambiguity

# Checking validity of a user-defined type


When dealing with a user-written type, we first translate it from an HsType
to a Type, performing kind checking, and then check various things that should
be true about it.  We don't want to perform these checks at the same time
as the initial translation because (a) they are unnecessary for interface-file
types and (b) when checking a mutually recursive group of type and class decls,
we can't "look" at the tycons/classes yet.  Also, the checks are rather
diverse, and used to really mess up the other code.

One thing we check for is 'rank'.

        Rank 0:         monotypes (no foralls)
        Rank 1:         foralls at the front only, Rank 0 inside
        Rank 2:         foralls at the front, Rank 1 on left of fn arrow,

        basic ::= tyvar | T basic ... basic

        r2  ::= forall tvs. cxt => r2a
        r2a ::= r1 -> r2a | basic
        r1  ::= forall tvs. cxt => r0
        r0  ::= r0 -> r0 | basic

Another thing is to check that type synonyms are saturated.
This might not necessarily show up in kind checking.
        type A i = i
        data T k = MkT (k Int)
        f :: T A        -- BAD!


### Note: Higher rank types

Technically
            Int -> forall a. a->a
is still a rank-1 type, but it's not Haskell 98 (Trac #5957).  So the
validity checker allow a forall after an arrow only if we allow it
before -- that is, with Rank2Types or RankNTypes


### Note: Liberal type synonyms

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

# \subsection{Checking a theta or source type}


### Note: Implicit parameters in instance decls

Implicit parameters _only_ allowed in type signatures; not in instance
decls, superclasses etc. The reason for not allowing implicit params in
instances is a bit subtle.  If we allowed
  instance (?x::Int, Eq a) => Foo [a] where ...
then when we saw
     (e :: (?x::Int) => t)
it would be unclear how to discharge all the potential uses of the ?x
in e.  For example, a constraint Foo [Int] might come out of e, and
applying the instance decl would show up two uses of ?x.  Trac #8912.


### Note: Validity checking for constraints

We look through constraint synonyms so that we can see the underlying
constraint(s).  For example
   type Foo = ?x::Int
   instance Foo => C T
We should reject the instance because it has an implicit parameter in
the context.

But we record, in 'under_syn', whether we have looked under a synonym
to avoid requiring language extensions at the use site.  Main example
(Trac #9838):

### Note: ConstraintKinds in predicates

Don't check for -XConstraintKinds under a type synonym, because that
was done at the type synonym definition site; see Trac #9838
e.g.   module A where
          type C a = (Eq a, Ix a)   -- Needs -XConstraintKinds
       module B where
          import A
          f :: C a => a -> a        -- Does *not* need -XConstraintKinds

### Note: Irreducible predicates in superclasses

Allowing type-family calls in class superclasses is somewhat dangerous
because we can write:

 type family Fooish x :: * -> Constraint
 type instance Fooish () = Foo
 class Fooish () a => Foo a where

This will cause the constraint simplifier to loop because every time we canonicalise a
(Foo a) class constraint we add a (Fooish () a) constraint which will be immediately
solved to add+canonicalise another (Foo a) constraint.  

### Note: Simplifiable given constraints

### Note: Instance and Given overlap

So this warning is switched on only if we have NoMonoLocalBinds; in
that case the warning discourages users from writing simplifiable
class constraints.

The warning only fires if the constraint in the signature
matches the top-level instances in only one way, and with no
unifiers -- that is, under the same circumstances that
TcInteract.matchInstEnv fires an interaction with the top
level instances.  For example (Trac #13526), consider

### Note: Kind polymorphic type classes

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


# \subsection{Checking for a decent instance head type}


@checkValidInstHead@ checks the type {\em and} its syntactic constraints:
it must normally look like: @instance Foo (Tycon a b c ...) ...@

The exceptions to this syntactic checking: (1)~if the @GlasgowExts@
flag is on, or (2)~the instance is imported (they must have been
compiled elsewhere). In these cases, we let them go through anyway.

We can also have instances for functions: @instance Foo (a -> b) ...@.


### Note: Casts during validity checking

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

### Note: Validity checking of HasField instances

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

### Note: Valid 'deriving' predicate

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
    would count as a constructor! See Trac #11833.

  * Also check for a bizarre corner case, when the derived instance decl
    would look like
       instance C a b => D (T a) where ...
    Note that 'b' isn't a parameter of T.  This gives rise to all sorts of
    problems; in particular, it's hard to compare solutions for equality
    when finding the fixpoint, and that means the inferContext loop does
    not converge.  See Trac #5287.

### Note: Equality class instances

We can't have users writing instances for the equality classes. But we
still need to be able to write instances for them ourselves. So we allow
instances only in the defining module.



# \subsection{Checking instance for termination}


### Note: Instances and constraint synonyms

Currently, we don't allow instances for constraint synonyms at all.
Consider these (Trac #13267):
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


### Note: Paterson conditions

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


### Note: Associated type instances

We allow this:
  class C a where
    type T x a
  instance C Int where
    type T (S y) Int = y
    type T Z     Int = Char

Note that
  a) The variable 'x' is not bound by the class decl
  b) 'x' is instantiated to a non-type-variable in the instance
  c) There are several type instance decls for T in the instance

All this is fine.  Of course, you can't give any *more* instances
for (T ty Int) elsewhere, because it's an *associated* type.

### Note: Checking consistent instantiation

See Trac #11450 for background discussion on this check.

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
  See Trac #11450.

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
  to pursue that design (see also GHC Trac #13398).

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


# Checking type instance well-formedness and termination


# Telescope checking


### Note: Bad telescopes

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
this is bogus. (We could probably figure out to put b between a and c.
But I think this is doing users a disservice, in the long run.)
(Testcase: dependent/should_fail/BadTelescope4)

3. t3 :: forall a. (forall k (b :: k). SameKind a b) -> ()

This is a straightforward skolem escape. Note that a and b need to have
the same kind.
(Testcase: polykinds/T11142)

How do we deal with all of this? For TyCons, we have checkValidTyConTyVars.
That function looks to see if any of the tyConTyVars are repeated, but
it's really a telescope check. It works because all tycons are kind-generalized.
If there is a bad telescope, the kind-generalization will end up generalizing
over a variable bound later in the telescope.

For non-tycons, we do scope checking when we bring tyvars into scope,
in tcImplicitTKBndrs and tcExplicitTKBndrs. Note that we also have to
sort implicit binders into a well-scoped order whenever we have implicit
binders to worry about. This is done in quantifyTyVars and in
tcImplicitTKBndrs.


# \subsection{Auxiliary functions}
