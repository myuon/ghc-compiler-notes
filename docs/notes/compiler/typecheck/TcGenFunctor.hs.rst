`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcGenFunctor.hs>`_

compiler/typecheck/TcGenFunctor.hs
==================================


Note [Deriving null]
~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcGenFunctor.hs#L609>`__

In some cases, deriving the definition of 'null' can produce much better
results than the default definition. For example, with

::

  data SnocList a = Nil | Snoc (SnocList a) a

the default definition of 'null' would walk the entire spine of a
nonempty snoc-list before concluding that it is not null. But looking at
the Snoc constructor, we can immediately see that it contains an 'a', and
so 'null' can return False immediately if it matches on Snoc. When we
derive 'null', we keep track of things that cannot be null. The interesting
case is type application. Given

::

  data Wrap a = Wrap (Foo (Bar a))

we use

::

  null (Wrap fba) = all null fba

but if we see

::

  data Wrap a = Wrap (Foo a)

we can just use

::

  null (Wrap fa) = null fa

Indeed, we allow this to happen even for tuples:

::

  data Wrap a = Wrap (Foo (a, Int))

produces

::

  null (Wrap fa) = null fa

As explained in Note [Deriving <$], giving tuples special performance treatment
could surprise users if they switch to other types, but Ryan Scott seems to
think it's okay to do it for now.



Note [DeriveFoldable with ExistentialQuantification]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcGenFunctor.hs#L983>`__

Functor and Traversable instances can only be derived for data types whose
last type parameter is truly universally polymorphic. For example:

::

  data T a b where
    T1 ::                 b   -> T a b   -- YES, b is unconstrained
    T2 :: Ord b   =>      b   -> T a b   -- NO, b is constrained by (Ord b)
    T3 :: b ~ Int =>      b   -> T a b   -- NO, b is constrained by (b ~ Int)
    T4 ::                 Int -> T a Int -- NO, this is just like T3
    T5 :: Ord a   => a -> b   -> T a b   -- YES, b is unconstrained, even
                                         -- though a is existential
    T6 ::                 Int -> T Int b -- YES, b is unconstrained

For Foldable instances, however, we can completely lift the constraint that
the last type parameter be truly universally polymorphic. This means that T
(as defined above) can have a derived Foldable instance:

::

  instance Foldable (T a) where
    foldr f z (T1 b)   = f b z
    foldr f z (T2 b)   = f b z
    foldr f z (T3 b)   = f b z
    foldr f z (T4 b)   = z
    foldr f z (T5 a b) = f b z
    foldr f z (T6 a)   = z

::

    foldMap f (T1 b)   = f b
    foldMap f (T2 b)   = f b
    foldMap f (T3 b)   = f b
    foldMap f (T4 b)   = mempty
    foldMap f (T5 a b) = f b
    foldMap f (T6 a)   = mempty

In a Foldable instance, it is safe to fold over an occurrence of the last type
parameter that is not truly universally polymorphic. However, there is a bit
of subtlety in determining what is actually an occurrence of a type parameter.
T3 and T4, as defined above, provide one example:

::

  data T a b where
    ...
    T3 :: b ~ Int => b   -> T a b
    T4 ::            Int -> T a Int
    ...

::

  instance Foldable (T a) where
    ...
    foldr f z (T3 b) = f b z
    foldr f z (T4 b) = z
    ...
    foldMap f (T3 b) = f b
    foldMap f (T4 b) = mempty
    ...

Notice that the argument of T3 is folded over, whereas the argument of T4 is
not. This is because we only fold over constructor arguments that
syntactically mention the universally quantified type parameter of that
particular data constructor. See foldDataConArgs for how this is implemented.

As another example, consider the following data type. The argument of each
constructor has the same type as the last type parameter:

::

  data E a where
    E1 :: (a ~ Int) => a   -> E a
    E2 ::              Int -> E Int
    E3 :: (a ~ Int) => a   -> E Int
    E4 :: (a ~ Int) => Int -> E a

Only E1's argument is an occurrence of a universally quantified type variable
that is syntactically equivalent to the last type parameter, so only E1's
argument will be folded over in a derived Foldable instance.

See #10447 for the original discussion on this feature. Also see
https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/DeriveFunctor
for a more in-depth explanation.



Note [FFoldType and functorLikeTraverse]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcGenFunctor.hs#L1058>`__

Deriving Functor, Foldable, and Traversable all require generating expressions
which perform an operation on each argument of a data constructor depending
on the argument's type. In particular, a generated operation can be different
depending on whether the type mentions the last type variable of the datatype
(e.g., if you have data T a = MkT a Int, then a generated foldr expression would
fold over the first argument of MkT, but not the second).

This pattern is abstracted with the FFoldType datatype, which provides hooks
for the user to specify how a constructor argument should be folded when it
has a type with a particular "shape". The shapes are as follows (assume that
a is the last type variable in a given datatype):

* ft_triv:    The type does not mention the last type variable at all.
              Examples: Int, b

* ft_var:     The type is syntactically equal to the last type variable.
              Moreover, the type appears in a covariant position (see
              the Deriving Functor instances section of the user's guide
              for an in-depth explanation of covariance vs. contravariance).
              Example: a (covariantly)

* ft_co_var:  The type is syntactically equal to the last type variable.
              Moreover, the type appears in a contravariant position.
              Example: a (contravariantly)

* ft_fun:     A function type which mentions the last type variable in
              the argument position, result position or both.
              Examples: a -> Int, Int -> a, Maybe a -> [a]

* ft_tup:     A tuple type which mentions the last type variable in at least
              one of its fields. The TyCon argument of ft_tup represents the
              particular tuple's type constructor.
              Examples: (a, Int), (Maybe a, [a], Either a Int), (# Int, a #)

* ft_ty_app:  A type is being applied to the last type parameter, where the
              applied type does not mention the last type parameter (if it
              did, it would fall under ft_bad_app). The Type argument to
              ft_ty_app represents the applied type.

::

              Note that functions, tuples, and foralls are distinct cases
              and take precedence of ft_ty_app. (For example, (Int -> a) would
              fall under (ft_fun Int a), not (ft_ty_app ((->) Int) a).
              Examples: Maybe a, Either b a

* ft_bad_app: A type application uses the last type parameter in a position
              other than the last argument. This case is singled out because
              Functor, Foldable, and Traversable instances cannot be derived
              for datatypes containing arguments with such types.
              Examples: Either a Int, Const a b

* ft_forall:  A forall'd type mentions the last type parameter on its right-
              hand side (and is not quantified on the left-hand side). This
              case is present mostly for plumbing purposes.
              Example: forall b. Either b a

If FFoldType describes a strategy for folding subcomponents of a Type, then
functorLikeTraverse is the function that applies that strategy to the entirety
of a Type, returning the final folded-up result.

foldDataConArgs applies functorLikeTraverse to every argument type of a
constructor, returning a list of the fold results. This makes foldDataConArgs
a natural way to generate the subexpressions in a generated fmap, foldr,
foldMap, or traverse definition (the subexpressions must then be combined in
a method-specific fashion to form the final generated expression).

Deriving Generic1 also does validity checking by looking for the last type
variable in certain positions of a constructor's argument types, so it also
uses foldDataConArgs. See Note [degenerate use of FFoldType] in TcGenGenerics.



Note [Generated code for DeriveFoldable and DeriveTraversable]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcGenFunctor.hs#L1129>`__

We adapt the algorithms for -XDeriveFoldable and -XDeriveTraversable based on
that of -XDeriveFunctor. However, there an important difference between deriving
the former two typeclasses and the latter one, which is best illustrated by the
following scenario:

::

  data WithInt a = WithInt a Int# deriving (Functor, Foldable, Traversable)

The generated code for the Functor instance is straightforward:

::

  instance Functor WithInt where
    fmap f (WithInt a i) = WithInt (f a) i

But if we use too similar of a strategy for deriving the Foldable and
Traversable instances, we end up with this code:

::

  instance Foldable WithInt where
    foldMap f (WithInt a i) = f a <> mempty

::

  instance Traversable WithInt where
    traverse f (WithInt a i) = fmap WithInt (f a) <*> pure i

This is unsatisfying for two reasons:

1. The Traversable instance doesn't typecheck! Int# is of kind #, but pure
   expects an argument whose type is of kind *. This effectively prevents
   Traversable from being derived for any datatype with an unlifted argument
   type (#11174).

2. The generated code contains superfluous expressions. By the Monoid laws,
   we can reduce (f a <> mempty) to (f a), and by the Applicative laws, we can
   reduce (fmap WithInt (f a) <*> pure i) to (fmap (\b -> WithInt b i) (f a)).

We can fix both of these issues by incorporating a slight twist to the usual
algorithm that we use for -XDeriveFunctor. The differences can be summarized
as follows:

1. In the generated expression, we only fold over arguments whose types
   mention the last type parameter. Any other argument types will simply
   produce useless 'mempty's or 'pure's, so they can be safely ignored.

2. In the case of -XDeriveTraversable, instead of applying ConName,
   we apply (\b_i ... b_k -> ConName a_1 ... a_n), where

   * ConName has n arguments
   * {b_i, ..., b_k} is a subset of {a_1, ..., a_n} whose indices correspond
     to the arguments whose types mention the last type parameter. As a
     consequence, taking the difference of {a_1, ..., a_n} and
     {b_i, ..., b_k} yields the all the argument values of ConName whose types
     do not mention the last type parameter. Note that [i, ..., k] is a
     strictly increasing—but not necessarily consecutive—integer sequence.

::

     For example, the datatype

::

       data Foo a = Foo Int a Int a

::

     would generate the following Traversable instance:

::

       instance Traversable Foo where
         traverse f (Foo a1 a2 a3 a4) =
           fmap (\b2 b4 -> Foo a1 b2 a3 b4) (f a2) <*> f a4

Technically, this approach would also work for -XDeriveFunctor as well, but we
decide not to do so because:

1. There's not much benefit to generating, e.g., ((\b -> WithInt b i) (f a))
   instead of (WithInt (f a) i).

2. There would be certain datatypes for which the above strategy would
   generate Functor code that would fail to typecheck. For example:

::

     data Bar f a = Bar (forall f. Functor f => f a) deriving Functor

::

   With the conventional algorithm, it would generate something like:

::

     fmap f (Bar a) = Bar (fmap f a)

::

   which typechecks. But with the strategy mentioned above, it would generate:

::

     fmap f (Bar a) = (\b -> Bar b) (fmap f a)

::

   which does not typecheck, since GHC cannot unify the rank-2 type variables
   in the types of b and (fmap f a).



Note [Phantom types with Functor, Foldable, and Traversable]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcGenFunctor.hs#L1214>`__

Given a type F :: * -> * whose type argument has a phantom role, we can always
produce lawful Functor and Traversable instances using

::

    fmap _ = coerce
    traverse _ = pure . coerce

Indeed, these are equivalent to any *strictly lawful* instances one could
write, except that this definition of 'traverse' may be lazier.  That is, if
instances obey the laws under true equality (rather than up to some equivalence
relation), then they will be essentially equivalent to these. These definitions
are incredibly cheap, so we want to use them even if it means ignoring some
non-strictly-lawful instance in an embedded type.

Foldable has far fewer laws to work with, which leaves us unwelcome
freedom in implementing it. At a minimum, we would like to ensure that
a derived foldMap is always at least as good as foldMapDefault with a
derived traverse. To accomplish that, we must define

::

   foldMap _ _ = mempty

in these cases.

This may have different strictness properties from a standard derivation.
Consider

::

   data NotAList a = Nil | Cons (NotAList a) deriving Foldable

The usual deriving mechanism would produce

::

   foldMap _ Nil = mempty
   foldMap f (Cons x) = foldMap f x

which is strict in the entire spine of the NotAList.

Final point: why do we even care about such types? Users will rarely if ever
map, fold, or traverse over such things themselves, but other derived
instances may:

::

   data Hasn'tAList a = NotHere a (NotAList a) deriving Foldable



Note [EmptyDataDecls with Functor, Foldable, and Traversable]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcGenFunctor.hs#L1257>`__

There are some slightly tricky decisions to make about how to handle
Functor, Foldable, and Traversable instances for types with no constructors.
For fmap, the two basic options are

::

   fmap _ _ = error "Sorry, no constructors"

or

::

   fmap _ z = case z of

In most cases, the latter is more helpful: if the thunk passed to fmap
throws an exception, we're generally going to be much more interested in
that exception than in the fact that there aren't any constructors.

In order to match the semantics for phantoms (see note above), we need to
be a bit careful about 'traverse'. The obvious definition would be

::

   traverse _ z = case z of

but this is stricter than the one for phantoms. We instead use

::

   traverse _ z = pure $ case z of

For foldMap, the obvious choices are

::

   foldMap _ _ = mempty

or

::

   foldMap _ z = case z of

We choose the first one to be consistent with what foldMapDefault does for
a derived Traversable instance.

