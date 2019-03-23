Note [Shortcut solving: overlap]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have
  instance {-# OVERLAPPABLE #-} C a where ...
and we are typechecking
  f :: C a => a -> a
  f = e  -- Gives rise to [W] C a

We don't want to solve the wanted constraint with the overlappable
instance; rather we want to use the supplied (C a)! That was the whole
point of it being overlappable!  #14434 wwas an example.

Alas even if the instance has no overlap flag, thus
  instance C a where ...
there is nothing to stop it being overlapped. GHC provides no way to
declare an instance as "final" so it can't be overlapped.  But really
only final instances are OK for short-cut solving.  Sigh. #15135
was a puzzling example.


Note [KnownNat & KnownSymbol and EvLit]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A part of the type-level literals implementation are the classes
"KnownNat" and "KnownSymbol", which provide a "smart" constructor for
defining singleton values.  Here is the key stuff from GHC.TypeLits

  class KnownNat (n :: Nat) where
    natSing :: SNat n

  newtype SNat (n :: Nat) = SNat Integer

Conceptually, this class has infinitely many instances:

  instance KnownNat 0       where natSing = SNat 0
  instance KnownNat 1       where natSing = SNat 1
  instance KnownNat 2       where natSing = SNat 2
  ...

In practice, we solve `KnownNat` predicates in the type-checker
(see typecheck/TcInteract.hs) because we can't have infinitely many instances.
The evidence (aka "dictionary") for `KnownNat` is of the form `EvLit (EvNum n)`.

We make the following assumptions about dictionaries in GHC:
  1. The "dictionary" for classes with a single method---like `KnownNat`---is
     a newtype for the type of the method, so using a evidence amounts
     to a coercion, and
  2. Newtypes use the same representation as their definition types.

So, the evidence for `KnownNat` is just a value of the representation type,
wrapped in two newtype constructors: one to make it into a `SNat` value,
and another to make it into a `KnownNat` dictionary.

Also note that `natSing` and `SNat` are never actually exposed from the
library---they are just an implementation detail.  Instead, users see
a more convenient function, defined in terms of `natSing`:

  natVal :: KnownNat n => proxy n -> Integer

The reason we don't use this directly in the class is that it is simpler
and more efficient to pass around an integer rather than an entire function,
especially when the `KnowNat` evidence is packaged up in an existential.

The story for kind `Symbol` is analogous:
  * class KnownSymbol
  * newtype SSymbol
  * Evidence: a Core literal (e.g. mkNaturalExpr)




Note [Fabricating Evidence for Literals in Backpack]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Let `T` be a type of kind `Nat`. When solving for a purported instance
of `KnownNat T`, ghc tries to resolve the type `T` to an integer `n`,
in which case the evidence `EvLit (EvNum n)` is generated on the
fly. It might appear that this is sufficient as users cannot define
their own instances of `KnownNat`. However, for backpack module this
would not work (see issue #15379). Consider the signature `Abstract`

> signature Abstract where
>   data T :: Nat
>   instance KnownNat T

and a module `Util` that depends on it:

> module Util where
>  import Abstract
>  printT :: IO ()
>  printT = do print $ natVal (Proxy :: Proxy T)

Clearly, we need to "use" the dictionary associated with `KnownNat T`
in the module `Util`, but it is too early for the compiler to produce
a real dictionary as we still have not fixed what `T` is. Only when we
mixin a concrete module

> module Concrete where
>   type T = 42

do we really get hold of the underlying integer. So the strategy that
we follow is the following

1. If T is indeed available as a type alias for an integer constant,
   generate the dictionary on the fly, failing which

2. Look up the type class environment for the evidence.

Finally actual code gets generate for Util only when a module like
Concrete gets "mixed-in" in place of the signature Abstract. As a
result all things, including the typeclass instances, in Concrete gets
reexported. So `KnownNat` gets resolved the normal way post-Backpack.

A similar generation works for `KnownSymbol` as well



Note [Typeable (T a b c)]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For type applications we always decompose using binary application,
via doTyApp, until we get to a *kind* instantiation.  Example
   Proxy :: forall k. k -> *

To solve Typeable (Proxy (* -> *) Maybe) we
  - First decompose with doTyApp,
    to get (Typeable (Proxy (* -> *))) and Typeable Maybe
  - Then solve (Typeable (Proxy (* -> *))) with doTyConApp

If we attempt to short-cut by solving it all at once, via
doTyConApp

(this note is sadly truncated FIXME)




Note [No Typeable for polytypes or qualified types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We do not support impredicative typeable, such as
   Typeable (forall a. a->a)
   Typeable (Eq a => a -> a)
   Typeable (() => Int)
   Typeable (((),()) => Int)

See #9858.  For forall's the case is clear: we simply don't have
a TypeRep for them.  For qualified but not polymorphic types, like
(Eq a => a -> a), things are murkier.  But:

 * We don't need a TypeRep for these things.  TypeReps are for
   monotypes only.

 * Perhaps we could treat `=>` as another type constructor for `Typeable`
   purposes, and thus support things like `Eq Int => Int`, however,
   at the current state of affairs this would be an odd exception as
   no other class works with impredicative types.
   For now we leave it off, until we have a better story for impredicativity.




Note [Typeable for Nat and Symbol]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We have special Typeable instances for Nat and Symbol.  Roughly we
have this instance, implemented here by doTyLit:
      instance KnownNat n => Typeable (n :: Nat) where
         typeRep = typeNatTypeRep @n
where
   Data.Typeable.Internals.typeNatTypeRep :: KnownNat a => TypeRep a

Ultimately typeNatTypeRep uses 'natSing' from KnownNat to get a
runtime value 'n'; it turns it into a string with 'show' and uses
that to whiz up a TypeRep TyCon for 'n', with mkTypeLitTyCon.
See #10348.

Because of this rule it's inadvisable (see #15322) to have a constraint
    f :: (Typeable (n :: Nat)) => blah
in a function signature; it gives rise to overlap problems just as
if you'd written
    f :: Eq [a] => blah


Note [HasField instances]
~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have

    data T y = MkT { foo :: [y] }

and `foo` is in scope.  Then GHC will automatically solve a constraint like

    HasField "foo" (T Int) b

by emitting a new wanted

    T alpha -> [alpha] ~# T Int -> b

and building a HasField dictionary out of the selector function `foo`,
appropriately cast.

The HasField class is defined (in GHC.Records) thus:

    class HasField (x :: k) r a | x r -> a where
      getField :: r -> a

Since this is a one-method class, it is represented as a newtype.
Hence we can solve `HasField "foo" (T Int) b` by taking an expression
of type `T Int -> b` and casting it using the newtype coercion.
Note that

    foo :: forall y . T y -> [y]

so the expression we construct is

    foo @alpha |> co

where

    co :: (T alpha -> [alpha]) ~# HasField "foo" (T Int) b

is built from

    co1 :: (T alpha -> [alpha]) ~# (T Int -> b)

which is the new wanted, and

    co2 :: (T Int -> b) ~# HasField "foo" (T Int) b

which can be derived from the newtype coercion.

If `foo` is not in scope, or has a higher-rank or existentially
quantified type, then the constraint is not solved automatically, but
may be solved by a user-supplied HasField instance.  Similarly, if we
encounter a HasField constraint where the field is not a literal
string, or does not belong to the type, then we fall back on the
normal constraint solver behaviour.
See Note [HasField instances]
