[[src]](https://github.com/ghc/ghc/tree/master/compiler/typecheck/TcGenFunctor.hs)

(c) The University of Glasgow 2011


The deriving code for the Functor, Foldable, and Traversable classes
(equivalent to the code in TcGenDeriv, for other classes)


# 

 see http://www.mail-archive.com/haskell-prime@haskell.org/msg02116.html

# 

For the data type:

  data T a = T1 Int a | T2 (T a)

We generate the instance:

  instance Functor T where
      fmap f (T1 b1 a) = T1 b1 (f a)
      fmap f (T2 ta)   = T2 (fmap f ta)

Notice that we don't simply apply 'fmap' to the constructor arguments.
Rather
  - Do nothing to an argument whose type doesn't mention 'a'
  - Apply 'f' to an argument of type 'a'
  - Apply 'fmap f' to other arguments
That's why we have to recurse deeply into the constructor argument types,
rather than just one level, as we typically do.

What about types with more than one type parameter?  In general, we only
derive Functor for the last position:

  data S a b = S1 [b] | S2 (a, T a b)
  instance Functor (S a) where
    fmap f (S1 bs)    = S1 (fmap f bs)
    fmap f (S2 (p,q)) = S2 (a, fmap f q)

However, we have special cases for
         - tuples
         - functions

More formally, we write the derivation of fmap code over type variable
'a for type 'b as ($fmap 'a 'b).  In this general notation the derived
instance for T is:

  instance Functor T where
      fmap f (T1 x1 x2) = T1 ($(fmap 'a 'b1) x1) ($(fmap 'a 'a) x2)
      fmap f (T2 x1)    = T2 ($(fmap 'a '(T a)) x1)

  $(fmap 'a 'b)          =  \x -> x     -- when b does not contain a
  $(fmap 'a 'a)          =  f
  $(fmap 'a '(b1,b2))    =  \x -> case x of (x1,x2) -> ($(fmap 'a 'b1) x1, $(fmap 'a 'b2) x2)
  $(fmap 'a '(T b1 b2))  =  fmap $(fmap 'a 'b2)   -- when a only occurs in the last parameter, b2
  $(fmap 'a '(b -> c))   =  \x b -> $(fmap 'a' 'c) (x ($(cofmap 'a 'b) b))

For functions, the type parameter 'a can occur in a contravariant position,
which means we need to derive a function like:

  cofmap :: (a -> b) -> (f b -> f a)

This is pretty much the same as $fmap, only without the $(cofmap 'a 'a) case:

  $(cofmap 'a 'b)          =  \x -> x     -- when b does not contain a
  $(cofmap 'a 'a)          =  error "type variable in contravariant position"
  $(cofmap 'a '(b1,b2))    =  \x -> case x of (x1,x2) -> ($(cofmap 'a 'b1) x1, $(cofmap 'a 'b2) x2)
  $(cofmap 'a '[b])        =  map $(cofmap 'a 'b)
  $(cofmap 'a '(T b1 b2))  =  fmap $(cofmap 'a 'b2)   -- when a only occurs in the last parameter, b2
  $(cofmap 'a '(b -> c))   =  \x b -> $(cofmap 'a' 'c) (x ($(fmap 'a 'c) b))

Note that the code produced by $(fmap _ _) is always a higher order function,
with type `(a -> b) -> (g a -> g b)` for some g. When we need to do pattern
matching on the type, this means create a lambda function (see the (,) case above).
The resulting code for fmap can look a bit weird, for example:

  data X a = X (a,Int)
  -- generated instance
  instance Functor X where
      fmap f (X x) = (\y -> case y of (x1,x2) -> X (f x1, (\z -> z) x2)) x

The optimizer should be able to simplify this code by simple inlining.

An older version of the deriving code tried to avoid these applied
lambda functions by producing a meta level function. But the function to
be mapped, `f`, is a function on the code level, not on the meta level,
so it was eta expanded to `\x -> [| f $x |]`. This resulted in too much eta expansion.
It is better to produce too many lambdas than to eta expand, see ticket #7436.


### Note: Deriving <$


We derive the definition of <$. Allowing this to take the default definition
can lead to memory leaks: mapping over a structure with a constant function can
fill the result structure with trivial thunks that retain the values from the
original structure. The simplifier seems to handle this all right for simple
types, but not for recursive ones. Consider

data Tree a = Bin !(Tree a) a !(Tree a) | Tip deriving Functor

-- fmap _ Tip = Tip
-- fmap f (Bin l v r) = Bin (fmap f l) (f v) (fmap f r)

Using the default definition of <$, we get (<$) x = fmap (\_ -> x) and that
simplifies no further. Why is that? `fmap` is defined recursively, so GHC
cannot inline it. The static argument transformation would turn the definition
into a non-recursive one

-- fmap f = go where
--   go Tip = Tip
--   go (Bin l v r) = Bin (go l) (f v) (go r)

which GHC could inline, producing an efficient definion of `<$`. But there are
several problems. First, GHC does not perform the static argument transformation
by default, even with -O2. Second, even when it does perform the static argument
transformation, it does so only when there are at least two static arguments,
which is not the case for fmap. Finally, when the type in question is
non-regular, such as

data Nesty a = Z a | S (Nesty a) (Nest (a, a))

the function argument is no longer (entirely) static, so the static argument
transformation will do nothing for us.

Applying the default definition of `<$` will produce a tree full of thunks that
look like ((\_ -> x) x0), which represents unnecessary thunk allocation and
also retention of the previous value, potentially leaking memory. Instead, we
derive <$ separately. Two aspects are different from fmap: the case of the
sought type variable (ft_var) and the case of a type application (ft_ty_app).
The interesting one is ft_ty_app. We have to distinguish two cases: the
"immediate" case where the type argument *is* the sought type variable, and
the "nested" case where the type argument *contains* the sought type variable.

The immediate case:

Suppose we have

data Imm a = Imm (F ... a)

Then we want to define

x <$ Imm q = Imm (x <$ q)

The nested case:

Suppose we have

data Nes a = Nes (F ... (G a))

Then we want to define

x <$ Nes q = Nes (fmap (x <$) q)

We use the Replacer type to tag whether the expression derived for applying
<$ to the last type variable was the ft_var case (immediate) or one of the
others (letting ft_forall pass through as usual).

We could, but do not, give tuples special treatment to improve efficiency
in some cases. Suppose we have

data Nest a = Z a | S (Nest (a,a))

The optimal definition would be

x <$ Z _ = Z x
x <$ S t = S ((x, x) <$ t)

which produces a result with maximal internal sharing. The reason we do not
attempt to treat this case specially is that we have no way to give
user-provided tuple-like types similar treatment. If the user changed the
definition to

data Pair a = Pair a a
data Nest a = Z a | S (Nest (Pair a))

they would experience a surprising degradation in performance. 


Utility functions related to Functor deriving.

Since several things use the same pattern of traversal, this is abstracted into functorLikeTraverse.
This function works like a fold: it makes a value of type 'a' in a bottom up way.


# 

 see http://www.mail-archive.com/haskell-prime@haskell.org/msg02116.html

# 

Deriving Foldable instances works the same way as Functor instances,
only Foldable instances are not possible for function types at all.
Given (data T a = T a a (T a) deriving Foldable), we get:

  instance Foldable T where
      foldr f z (T x1 x2 x3) =
        $(foldr 'a 'a) x1 ( $(foldr 'a 'a) x2 ( $(foldr 'a '(T a)) x3 z ) )

-XDeriveFoldable is different from -XDeriveFunctor in that it filters out
arguments to the constructor that would produce useless code in a Foldable
instance. For example, the following datatype:

  data Foo a = Foo Int a Int deriving Foldable

would have the following generated Foldable instance:

  instance Foldable Foo where
    foldr f z (Foo x1 x2 x3) = $(foldr 'a 'a) x2

since neither of the two Int arguments are folded over.

The cases are:

  $(foldr 'a 'a)         =  f
  $(foldr 'a '(b1,b2))   =  \x z -> case x of (x1,x2) -> $(foldr 'a 'b1) x1 ( $(foldr 'a 'b2) x2 z )
  $(foldr 'a '(T b1 b2)) =  \x z -> foldr $(foldr 'a 'b2) z x  -- when a only occurs in the last parameter, b2

Note that the arguments to the real foldr function are the wrong way around,
since (f :: a -> b -> b), while (foldr f :: b -> t a -> b).

One can envision a case for types that don't contain the last type variable:

  $(foldr 'a 'b)         =  \x z -> z     -- when b does not contain a

### Note: Generated code for DeriveFoldable and DeriveTraversable

Foldable instances differ from Functor and Traversable instances in that
Foldable instances can be derived for data types in which the last type
variable is existentially quantified. In particular, if the last type variable
is refined to a more specific type in a GADT:

  data GADT a where
      G :: a ~ Int => a -> G Int

then the deriving machinery does not attempt to check that the type a contains
Int, since it is not syntactically equal to a type variable. That is, the
derived Foldable instance for GADT is:

  instance Foldable GADT where
      foldr _ z (GADT _) = z

### Note: DeriveFoldable with ExistentialQuantification

### Note: Deriving null


In some cases, deriving the definition of 'null' can produce much better
results than the default definition. For example, with

  data SnocList a = Nil | Snoc (SnocList a) a

the default definition of 'null' would walk the entire spine of a
nonempty snoc-list before concluding that it is not null. But looking at
the Snoc constructor, we can immediately see that it contains an 'a', and
so 'null' can return False immediately if it matches on Snoc. When we
derive 'null', we keep track of things that cannot be null. The interesting
case is type application. Given

  data Wrap a = Wrap (Foo (Bar a))

we use

  null (Wrap fba) = all null fba

but if we see

  data Wrap a = Wrap (Foo a)

we can just use

  null (Wrap fa) = null fa

Indeed, we allow this to happen even for tuples:

  data Wrap a = Wrap (Foo (a, Int))

produces

  null (Wrap fa) = null fa

### Note: Deriving <$

# 

# 

Again, Traversable is much like Functor and Foldable.

The cases are:

  $(traverse 'a 'a)          =  f
  $(traverse 'a '(b1,b2))    =  \x -> case x of (x1,x2) ->
     liftA2 (,) ($(traverse 'a 'b1) x1) ($(traverse 'a 'b2) x2)
  $(traverse 'a '(T b1 b2))  =  traverse $(traverse 'a 'b2)  -- when a only occurs in the last parameter, b2

Like -XDeriveFoldable, -XDeriveTraversable filters out arguments whose types
do not mention the last type parameter. Therefore, the following datatype:

  data Foo a = Foo Int a Int

would have the following derived Traversable instance:

  instance Traversable Foo where
    traverse f (Foo x1 x2 x3) =
      fmap (\b2 -> Foo x1 b2 x3) ( $(traverse 'a 'a) x2 )

since the two Int arguments do not produce any effects in a traversal.

One can envision a case for types that do not mention the last type parameter:

  $(traverse 'a 'b)          =  pure     -- when b does not contain a

### Note: Generated code for DeriveFoldable and DeriveTraversable

### Note: DeriveFoldable with ExistentialQuantification

Functor and Traversable instances can only be derived for data types whose
last type parameter is truly universally polymorphic. For example:

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

  instance Foldable (T a) where
    foldr f z (T1 b)   = f b z
    foldr f z (T2 b)   = f b z
    foldr f z (T3 b)   = f b z
    foldr f z (T4 b)   = z
    foldr f z (T5 a b) = f b z
    foldr f z (T6 a)   = z

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

  data T a b where
    ...
    T3 :: b ~ Int => b   -> T a b
    T4 ::            Int -> T a Int
    ...

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

  data E a where
    E1 :: (a ~ Int) => a   -> E a
    E2 ::              Int -> E Int
    E3 :: (a ~ Int) => a   -> E Int
    E4 :: (a ~ Int) => Int -> E a

Only E1's argument is an occurrence of a universally quantified type variable
that is syntactically equivalent to the last type parameter, so only E1's
argument will be folded over in a derived Foldable instance.

See Trac #10447 for the original discussion on this feature. Also see
https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/DeriveFunctor
for a more in-depth explanation.

### Note: FFoldType and functorLikeTraverse

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

* ft_ty_app:  A type is being applied to the last type parameter, where the
              applied type does not mention the last type parameter (if it
              did, it would fall under ft_bad_app). The Type argument to
              ft_ty_app represents the applied type.

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

### Note: degenerate use of FFoldType

### Note: Generated code for DeriveFoldable and DeriveTraversable

We adapt the algorithms for -XDeriveFoldable and -XDeriveTraversable based on
that of -XDeriveFunctor. However, there an important difference between deriving
the former two typeclasses and the latter one, which is best illustrated by the
following scenario:

  data WithInt a = WithInt a Int# deriving (Functor, Foldable, Traversable)

The generated code for the Functor instance is straightforward:

  instance Functor WithInt where
    fmap f (WithInt a i) = WithInt (f a) i

But if we use too similar of a strategy for deriving the Foldable and
Traversable instances, we end up with this code:

  instance Foldable WithInt where
    foldMap f (WithInt a i) = f a <> mempty

  instance Traversable WithInt where
    traverse f (WithInt a i) = fmap WithInt (f a) <*> pure i

This is unsatisfying for two reasons:

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

     For example, the datatype

       data Foo a = Foo Int a Int a

     would generate the following Traversable instance:

       instance Traversable Foo where
         traverse f (Foo a1 a2 a3 a4) =
           fmap (\b2 b4 -> Foo a1 b2 a3 b4) (f a2) <*> f a4

Technically, this approach would also work for -XDeriveFunctor as well, but we
decide not to do so because:

1. There's not much benefit to generating, e.g., ((\b -> WithInt b i) (f a))
   instead of (WithInt (f a) i).

2. There would be certain datatypes for which the above strategy would
   generate Functor code that would fail to typecheck. For example:

     data Bar f a = Bar (forall f. Functor f => f a) deriving Functor

   With the conventional algorithm, it would generate something like:

     fmap f (Bar a) = Bar (fmap f a)

   which typechecks. But with the strategy mentioned above, it would generate:

     fmap f (Bar a) = (\b -> Bar b) (fmap f a)

   which does not typecheck, since GHC cannot unify the rank-2 type variables
   in the types of b and (fmap f a).

### Note: Phantom types with Functor, Foldable, and Traversable


Given a type F :: * -> * whose type argument has a phantom role, we can always
produce lawful Functor and Traversable instances using

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

   foldMap _ _ = mempty

in these cases.

This may have different strictness properties from a standard derivation.
Consider

   data NotAList a = Nil | Cons (NotAList a) deriving Foldable

The usual deriving mechanism would produce

   foldMap _ Nil = mempty
   foldMap f (Cons x) = foldMap f x

which is strict in the entire spine of the NotAList.

Final point: why do we even care about such types? Users will rarely if ever
map, fold, or traverse over such things themselves, but other derived
instances may:

   data Hasn'tAList a = NotHere a (NotAList a) deriving Foldable

### Note: EmptyDataDecls with Functor, Foldable, and Traversable


There are some slightly tricky decisions to make about how to handle
Functor, Foldable, and Traversable instances for types with no constructors.
For fmap, the two basic options are

   fmap _ _ = error "Sorry, no constructors"

or

   fmap _ z = case z of

In most cases, the latter is more helpful: if the thunk passed to fmap
throws an exception, we're generally going to be much more interested in
that exception than in the fact that there aren't any constructors.

In order to match the semantics for phantoms (see note above), we need to
be a bit careful about 'traverse'. The obvious definition would be

   traverse _ z = case z of

but this is stricter than the one for phantoms. We instead use

   traverse _ z = pure $ case z of

For foldMap, the obvious choices are

   foldMap _ _ = mempty

or

   foldMap _ z = case z of

We choose the first one to be consistent with what foldMapDefault does for
a derived Traversable instance.
