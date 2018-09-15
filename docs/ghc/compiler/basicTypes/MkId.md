[[src]](https://github.com/ghc/ghc/tree/master/compiler/basicTypes/MkId.hs)

(c) The University of Glasgow 2006
(c) The AQUA Project, Glasgow University, 1998


This module contains definitions for the IdInfo for things that
have a standard form, namely:

- data constructors
- record selectors
- method and superclass selectors
- primitive operations


# \subsection{Wired in Ids}


### Note: Wired-in Ids

There are several reasons why an Id might appear in the wiredInIds:

(1) The ghcPrimIds are wired in because they can't be defined in
    Haskell at all, although the can be defined in Core.  They have
    compulsory unfoldings, so they are always inlined and they  have
    no definition site.  Their home module is GHC.Prim, so they
    also have a description in primops.txt.pp, where they are called
    'pseudoops'.

(2) The 'error' function, eRROR_ID, is wired in because we don't yet have
    a way to express in an interface file that the result type variable
    is 'open'; that is can be unified with an unboxed type

    [The interface file format now carry such information, but there's
    no way yet of expressing at the definition site for these
    error-reporting functions that they have an 'open'
    result type. -- sof 1/99]

(3) Other error functions (rUNTIME_ERROR_ID) are wired in (a) because
    the desugarer generates code that mentions them directly, and
    (b) for the same reason as eRROR_ID

(4) lazyId is wired in because the wired-in version overrides the
    strictness of the version defined in GHC.Base

(5) noinlineId is wired in because when we serialize to interfaces
    we may insert noinline statements.

In cases (2-4), the function has a definition in a library module, and
can be called; but the wired-in version means that the details are
never read from that module's interface file; instead, the full definition
is right here.


# \subsection{Data constructors}


The wrapper for a constructor is an ordinary top-level binding that evaluates
any strict args, unboxes any args that are going to be flattened, and calls
the worker.

We're going to build a constructor that looks like:

        data (Data a, C b) =>  T a b = T1 !a !Int b

        T1 = /\ a b ->
             \d1::Data a, d2::C b ->
             \p q r -> case p of { p ->
                       case q of { q ->
                       Con T1 [a,b] [p,q,r]}}

Notice that

* d2 is thrown away --- a context in a data decl is used to make sure
  one *could* construct dictionaries at the site the constructor
  is used, but the dictionary isn't actually used.

* We have to check that we can construct Data dictionaries for
  the types a and Int.  Once we've done that we can throw d1 away too.

* We use (case p of q -> ...) to evaluate p, rather than "seq" because
  all that matters is that the arguments are evaluated.  "seq" is
  very careful to preserve evaluation order, which we don't need
  to be here.

  You might think that we could simply give constructors some strictness
  info, like PrimOps, and let CoreToStg do the let-to-case transformation.
  But we don't do that because in the case of primops and functions strictness
  is a *property* not a *requirement*.  In the case of constructors we need to
  do something active to evaluate the argument.

  Making an explicit case expression allows the simplifier to eliminate
  it in the (common) case where the constructor arg is already evaluated.

### Note: Wrappers for data instance tycons

In the case of data instances, the wrapper also applies the coercion turning
the representation type into the family instance type to cast the result of
the wrapper.  For example, consider the declarations

  data family Map k :: * -> *
  data instance Map (a, b) v = MapPair (Map a (Pair b v))

The tycon to which the datacon MapPair belongs gets a unique internal
name of the form :R123Map, and we call it the representation tycon.
In contrast, Map is the family tycon (accessible via
tyConFamInst_maybe). A coercion allows you to move between
representation and family type.  It is accessible from :R123Map via
tyConFamilyCoercion_maybe and has kind

  Co123Map a b v :: {Map (a, b) v ~ :R123Map a b v}

The wrapper and worker of MapPair get the types

        -- Wrapper
  $WMapPair :: forall a b v. Map a (Map a b v) -> Map (a, b) v
  $WMapPair a b v = MapPair a b v `cast` sym (Co123Map a b v)

        -- Worker
  MapPair :: forall a b v. Map a (Map a b v) -> :R123Map a b v

This coercion is conditionally applied by wrapFamInstBody.

It's a bit more complicated if the data instance is a GADT as well!

   data instance T [a] where
        T1 :: forall b. b -> T [Maybe b]

Hence we translate to

        -- Wrapper
  $WT1 :: forall b. b -> T [Maybe b]
  $WT1 b v = T1 (Maybe b) b (Maybe b) v
                        `cast` sym (Co7T (Maybe b))

        -- Worker
  T1 :: forall c b. (c ~ Maybe b) => b -> :R7T c

        -- Coercion from family type to representation type
  Co7T a :: T [a] ~ :R7T a

### Note: Newtype datacons

The "data constructor" for a newtype should always be vanilla.  At one
point this wasn't true, because the newtype arising from
     class C a => D a
looked like
       newtype T:D a = D:D (C a)
so the data constructor for T:C had a single argument, namely the
predicate (C a).  But now we treat that as an ordinary argument, not
part of the theta-type, so all is well.

# \subsection{Dictionary selectors}


Selecting a field for a dictionary.  If there is just one field, then
there's nothing to do.

Dictionary selectors may get nested forall-types.  Thus:

        class Foo a where
          op :: forall b. Ord b => a -> b -> b

Then the top-level type for op is

        op :: forall a. Foo a =>
              forall b. Ord b =>
              a -> b -> b



# Data constructors



-------------------------------------------------
--         Data constructor representation
--
-- This is where we decide how to wrap/unwrap the
-- constructor fields
--
--------------------------------------------------


### Note: Inline partially-applied constructor wrappers


We allow the wrapper to inline when partially applied to avoid
boxing values unnecessarily. For example, consider

   data Foo a = Foo !Int a

   instance Traversable Foo where
     traverse f (Foo i a) = Foo i <$> f a

This desugars to

   traverse f foo = case foo of
        Foo i# a -> let i = I# i#
                    in map ($WFoo i) (f a)

If the wrapper `$WFoo` is not inlined, we get a fruitless reboxing of `i`.
But if we inline the wrapper, we get

   map (\a. case i of I# i# a -> Foo i# a) (f a)

and now case-of-known-constructor eliminates the redundant allocation.


### Note: Activation for data constructor wrappers

The Activation on a data constructor wrapper allows it to inline in
Phase 2 and later (1, 0).  But not in the InitialPhase.  That gives
rewrite rules a chance to fire (in the InitialPhase) if they mention
a data constructor on the left
   RULE "foo"  f (K a b) = ...
Since the LHS of rules are simplified with InitialPhase, we won't
inline the wrapper on the LHS either.

People have asked for this before, but now that even the InitialPhase
does some inlining, it has become important.

### Note: Bangs on imported data constructors


We pass Maybe [HsImplBang] to mkDataConRep to make use of HsImplBangs
from imported modules.

- Nothing <=> use HsSrcBangs
- Just bangs <=> use HsImplBangs

For imported types we can't work it all out from the HsSrcBangs,
because we want to be very sure to follow what the original module
(where the data type was declared) decided, and that depends on what
flags were enabled when it was compiled. So we record the decisions in
the interface file.

The HsImplBangs passed are in 1-1 correspondence with the
dataConOrigArgTys of the DataCon.

### Note: Data con wrappers and unlifted types

Consider
   data T = MkT !Int#

We certainly do not want to make a wrapper
   $WMkT x = case x of y { DEFAULT -> MkT y }

For a start, it's still to generate a no-op.  But worse, since wrappers
are currently injected at TidyCore, we don't even optimise it away!
So the stupid case expression stays there.  This actually happened for
the Integer data type (see Trac #1600 comment:66)!

### Note: Data con wrappers and GADT syntax

Consider these two very similar data types:

  data T1 a b = MkT1 b

  data T2 a b where
    MkT2 :: forall b a. b -> T2 a b

Despite their similar appearance, T2 will have a data con wrapper but T1 will
not. What sets them apart? The types of their constructors, which are:

  MkT1 :: forall a b. b -> T1 a b
  MkT2 :: forall b a. b -> T2 a b

### Note: DataCon user type variable binders

The worker data cons for T1 and T2, however, both have types such that `a` is
expected to come before `b` as arguments. Because MkT2 permutes this order, it
needs a data con wrapper to swizzle around the type variables to be in the
order the worker expects.

A somewhat surprising consequence of this is that *newtypes* can have data con
wrappers! After all, a newtype can also be written with GADT syntax:

  newtype T3 a b where
    MkT3 :: forall b a. b -> T3 a b

Again, this needs a wrapper data con to reorder the type variables. It does
mean that this newtype constructor requires another level of indirection when
being called, but the inliner should make swift work of that.


### Note: Unpack one-wide fields

The flag UnboxSmallStrictFields ensures that any field that can
(safely) be unboxed to a word-sized unboxed field, should be so unboxed.
For example:

    data A = A Int#
    newtype B = B A
    data C = C !B
    data D = D !C
    data E = E !()
    data F = F !D
    data G = G !F !F

All of these should have an Int# as their representation, except
G which should have two Int#s.

However

    data T = T !(S Int)
    data S = S !a

Here we can represent T with an Int#.

### Note: Recursive unboxing

# \subsection{Primitive operations}


# \subsection{DictFuns and default methods}


### Note: Dict funs and default methods

Dict funs and default methods are *not* ImplicitIds.  Their definition
involves user-written code, so we can't figure out their strictness etc
based on fixed info, as we can for constructors and record selectors (say).

### Note: Exported LocalIds

# \subsection{Un-definable}


These Ids can't be defined in Haskell.  They could be defined in
unfoldings in the wired-in GHC.Prim interface file, but we'd have to
ensure that they were definitely, definitely inlined, because there is
no curried identifier for them.  That's what mkCompulsoryUnfolding
does.  If we had a way to get a compulsory unfolding from an interface
file, we could do that, but we don't right now.

unsafeCoerce# isn't so much a PrimOp as a phantom identifier, that
just gets expanded into a type coercion wherever it occurs.  Hence we
add it as a built-in Id with an unfolding here.

The type variables we use here are "open" type variables: this means
they can unify with both unlifted and lifted types.  Hence we provide
another gun with which to shoot yourself in the foot.


### Note: dollarId magic

The only reason that ($) is wired in is so that its type can be
    forall (a:*, b:Open). (a->b) -> a -> b
That is, the return type can be unboxed.  E.g. this is OK
    foo $ True    where  foo :: Bool -> Int#
because ($) doesn't inspect or move the result of the call to foo.
See Trac #8739.

There is a special typing rule for ($) in TcExpr, so the type of ($)
isn't looked at there, BUT Lint subsequently (and rightly) complains
if sees ($) applied to Int# (say), unless we give it a wired-in type
as we do here.

### Note: Unsafe coerce magic

We define a *primitive*
   GHC.Prim.unsafeCoerce#
and then in the base library we define the ordinary function
   Unsafe.Coerce.unsafeCoerce :: forall (a:*) (b:*). a -> b
   unsafeCoerce x = unsafeCoerce# x

Notice that unsafeCoerce has a civilized (albeit still dangerous)
polymorphic type, whose type args have kind *.  So you can't use it on
unboxed values (unsafeCoerce 3#).

In contrast unsafeCoerce# is even more dangerous because you *can* use
it on unboxed things, (unsafeCoerce# 3#) :: Int. Its type is
   forall (r1 :: RuntimeRep) (r2 :: RuntimeRep) (a: TYPE r1) (b: TYPE r2). a -> b

### Note: seqId magic

'GHC.Prim.seq' is special in several ways.

### Note: Typing rule for seq

b) Its fixity is set in LoadIface.ghcPrimIface

### Note: Desugaring seq (1)

### Note: User-defined RULES for seq

### Note: User-defined RULES for seq

Roman found situations where he had
      case (f n) of _ -> e
where he knew that f (which was strict in n) would terminate if n did.
Notice that the result of (f n) is discarded. So it makes sense to
transform to
      case n of _ -> e

Rather than attempt some general analysis to support this, I've added
enough support that you can do this using a rewrite rule:

  RULE "f/seq" forall n.  seq (f n) = seq n

You write that rule.  When GHC sees a case expression that discards
its result, it mentally transforms it to a call to 'seq' and looks for
a RULE.  (This is done in Simplify.trySeqRules.)  As usual, the
correctness of the rule is up to you.

VERY IMPORTANT: to make this work, we give the RULE an arity of 1, not 2.
If we wrote
  RULE "f/seq" forall n e.  seq (f n) e = seq n e
with rule arity 2, then two bad things would happen:

### Note: seqId magic

  - The code in Simplify.rebuildCase would need to actually supply
    the value argument, which turns out to be awkward.

### Note: lazyId magic

lazy :: forall a?. a? -> a?   (i.e. works for unboxed types too)

'lazy' is used to make sure that a sub-expression, and its free variables,
are truly used call-by-need, with no code motion.  Key examples:

* pseq:    pseq a b = a `seq` lazy b
  We want to make sure that the free vars of 'b' are not evaluated
  before 'a', even though the expression is plainly strict in 'b'.

* catch:   catch a b = catch# (lazy a) b
  Again, it's clear that 'a' will be evaluated strictly (and indeed
  applied to a state token) but we want to make sure that any exceptions
  arising from the evaluation of 'a' are caught by the catch (see
  Trac #11555).

Implementing 'lazy' is a bit tricky:

* It must not have a strictness signature: by being a built-in Id,
  all the info about lazyId comes from here, not from GHC.Base.hi.
  This is important, because the strictness analyser will spot it as
  strict!

* It must not have an unfolding: it gets "inlined" by a HACK in
  CorePrep. It's very important to do this inlining *after* unfoldings
  are exposed in the interface file.  Otherwise, the unfolding for
  (say) pseq in the interface file will not mention 'lazy', so if we
  inline 'pseq' we'll totally miss the very thing that 'lazy' was
  there for in the first place. See Trac #3259 for a real world
  example.

* Suppose CorePrep sees (catch# (lazy e) b).  At all costs we must
  avoid using call by value here:
     case e of r -> catch# r b
  Avoiding that is the whole point of 'lazy'.  So in CorePrep (which
  generate the 'case' expression for a call-by-value call) we must
  spot the 'lazy' on the arg (in CorePrep.cpeApp), and build a 'let'
  instead.

* lazyId is defined in GHC.Base, so we don't *have* to inline it.  If it
  appears un-applied, we'll end up just calling it.

### Note: noinlineId magic

noinline :: forall a. a -> a

'noinline' is used to make sure that a function f is never inlined,
e.g., as in 'noinline f x'.  Ordinarily, the identity function with NOINLINE
could be used to achieve this effect; however, this has the unfortunate
result of leaving a (useless) call to noinline at runtime.  So we have
a little bit of magic to optimize away 'noinline' after we are done
running the simplifier.

'noinline' needs to be wired-in because it gets inserted automatically
when we serialize an expression to the interface format, and we DON'T
want use its fingerprints.

### Note: runRW magic

Some definitions, for instance @runST@, must have careful control over float out
of the bindings in their body. Consider this use of @runST@,

    f x = runST ( \ s -> let (a, s')  = newArray# 100 [] s
                             (_, s'') = fill_in_array_or_something a x s'
                         in freezeArray# a s'' )

If we inline @runST@, we'll get:

    f x = let (a, s')  = newArray# 100 [] realWorld#{-NB

NB