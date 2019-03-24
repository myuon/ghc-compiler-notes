`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/types/Type.hs>`_

Note [coreView vs tcView]
~~~~~~~~~~~~~~~~~~~~~~~~~
So far as the typechecker is concerned, 'Constraint' and 'TYPE
LiftedRep' are distinct kinds.

But in Core these two are treated as identical.

We implement this by making 'coreView' convert 'Constraint' to 'TYPE
LiftedRep' on the fly.  The function tcView (used in the type checker)
does not do this.

See also #11715, which tracks removing this inconsistency.



Note [Efficiency for mapCoercion ForAllCo case]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
As noted in Note [Forall coercions] in TyCoRep, a ForAllCo is a bit redundant.
It stores a TyCoVar and a Coercion, where the kind of the TyCoVar always matches
the left-hand kind of the coercion. This is convenient lots of the time, but
not when mapping a function over a coercion.

The problem is that tcm_tybinder will affect the TyCoVar's kind and
mapCoercion will affect the Coercion, and we hope that the results will be
the same. Even if they are the same (which should generally happen with
correct algorithms), then there is an efficiency issue. In particular,
this problem seems to make what should be a linear algorithm into a potentially
exponential one. But it's only going to be bad in the case where there's
lots of foralls in the kinds of other foralls. Like this:

.. code-block:: haskell

  forall a : (forall b : (forall c : ...). ...). ...

This construction seems unlikely. So we'll do the inefficient, easy way
for now.



Note [Specialising mappers]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
These INLINABLE pragmas are indispensable. mapType/mapCoercion are used
to implement zonking, and it's vital that they get specialised to the TcM
monad. This specialisation happens automatically (that is, without a
SPECIALISE pragma) as long as the definitions are INLINABLE. For example,
this one change made a 20% allocation difference in perf/compiler/T5030.



Note [Decomposing fat arrow c=>t]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Can we unify (a b) with (Eq a => ty)?   If we do so, we end up with
a partial application like ((=>) Eq a) which doesn't make sense in
source Haskell.  In contrast, we *can* unify (a b) with (t1 -> t2).
Here's an example (#9858) of how you might do it:
   i :: (Typeable a, Typeable b) => Proxy (a b) -> TypeRep
   i p = typeRep p

   j = i (Proxy :: Proxy (Eq Int => Int))
The type (Proxy (Eq Int => Int)) is only accepted with -XImpredicativeTypes,
but suppose we want that.  But then in the call to 'i', we end
up decomposing (Eq Int => Int), and we definitely don't want that.

This really only applies to the type checker; in Core, '=>' and '->'
are the same, as are 'Constraint' and '*'.  But for now I've put
the test in repSplitAppTy_maybe, which applies throughout, because
the other calls to splitAppTy are in Unify, which is also used by
the type checker (e.g. when matching type-function equations).



Note [Representation of function types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Functions (e.g. Int -> Char) can be thought of as being applications
of funTyCon (known in Haskell surface syntax as (->)),

.. code-block:: haskell

    (->) :: forall (r1 :: RuntimeRep) (r2 :: RuntimeRep)
                   (a :: TYPE r1) (b :: TYPE r2).
            a -> b -> Type

However, for efficiency's sake we represent saturated applications of (->)
with FunTy. For instance, the type,

.. code-block:: haskell

    (->) r1 r2 a b

is equivalent to,

.. code-block:: haskell

    FunTy (Anon a) b

Note how the RuntimeReps are implied in the FunTy representation. For this
reason we must be careful when recontructing the TyConApp representation (see,
for instance, splitTyConApp_maybe).

In the compiler we maintain the invariant that all saturated applications of
(->) are represented with FunTy.

See #11714.


Note [Care with kind instantiation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have
  T :: forall k. k
and we are finding the kind of
  T (forall b. b -> b) * Int
Then
  T (forall b. b->b) :: k[ k :-> forall b. b->b]
                     :: forall b. b -> b
So
  T (forall b. b->b) * :: (b -> b)[ b :-> *]
                       :: * -> *

In other words we must intantiate the forall!

Similarly (#15428)
   S :: forall k f. k -> f k
and we are finding the kind of
   S * (* ->) Int Bool
We have
   S * (* ->) :: (k -> f k)[ k :-> *, f :-> (* ->)]
              :: * -> * -> *
So again we must instantiate.

The same thing happens in ToIface.toIfaceAppArgsX.


---------------------------------------------------------------------
                                TyConApp
                                ~~~~~~~~


Note [mkLamType: dictionary arguments]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If we have (\ (d :: Ord a). blah), we want to give it type
           (Ord a => blah_ty)
with a fat arrow; that is, using mkInvisFunTy, not mkVisFunTy.

Why? After all, we are in Core, where (=>) and (->) behave the same.
Yes, but the /specialiser/ does treat dictionary arguments specially.
Suppose we do w/w on 'foo' in module A, thus (#11272, #6056)
   foo :: Ord a => Int -> blah
   foo a d x = case x of I# x' -> $wfoo @a d x'

.. code-block:: haskell

   $wfoo :: Ord a => Int# -> blah

Now in module B we see (foo @Int dOrdInt).  The specialiser will
specialise this to $sfoo, where
   $sfoo :: Int -> blah
   $sfoo x = case x of I# x' -> $wfoo @Int dOrdInt x'

Now we /must/ also specialise $wfoo!  But it wasn't user-written,
and has a type built with mkLamTypes.

Conclusion: the easiest thing is to make mkLamType build
            (c => ty)
when the argument is a predicate type.  See TyCoRep
Note [Types for coercions, predicates, and evidence]


Note [Evidence for quantified constraints]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The superclass mechanism in TcCanonical.makeSuperClasses risks
taking a quantified constraint like
   (forall a. C a => a ~ b)
and generate superclass evidence
   (forall a. C a => a ~# b)

This is a funny thing: neither isPredTy nor isCoVarType are true
of it.  So we are careful not to generate it in the first place:
see Note [Equality superclasses in quantified constraints]
in TcCanonical.


Note [Dictionary-like types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Being "dictionary-like" means either a dictionary type or a tuple thereof.
In GHC 6.10 we build implication constraints which construct such tuples,
and if we land up with a binding
    t :: (C [a], Eq [a])
    t = blah
then we want to treat t as cheap under "-fdicts-cheap" for example.
(Implication constraints are normally inlined, but sadly not if the
occurrence is itself inside an INLINE function!  Until we revise the
handling of implication constraints, that is.)  This turned out to
be important in getting good arities in DPH code.  Example:

.. code-block:: haskell

    class C a
    class D a where { foo :: a -> a }
    instance C a => D (Maybe a) where { foo x = x }

.. code-block:: haskell

    bar :: (C a, C b) => a -> b -> (Maybe a, Maybe b)
    {-# INLINE bar #-}
    bar x y = (foo (Just x), foo (Just y))

Then 'bar' should jolly well have arity 4 (two dicts, two args), but
we ended up with something like
   bar = __inline_me__ (\d1,d2. let t :: (D (Maybe a), D (Maybe b)) = ...
                                in \x,y. <blah>)

This is all a bit ad-hoc; eg it relies on knowing that implication
constraints build tuples.

ToDo: it would be far easier just to use isPredTy.


Note [ScopedSort]
~~~~~~~~~~~~~~~~~
Consider

.. code-block:: haskell

  foo :: Proxy a -> Proxy (b :: k) -> Proxy (a :: k2) -> ()

This function type is implicitly generalised over [a, b, k, k2]. These
variables will be Specified; that is, they will be available for visible
type application. This is because they are written in the type signature
by the user.

However, we must ask: what order will they appear in? In cases without
dependency, this is easy: we just use the lexical left-to-right ordering
of first occurrence. With dependency, we cannot get off the hook so
easily.

We thus state:

 * These variables appear in the order as given by ScopedSort, where
   the input to ScopedSort is the left-to-right order of first occurrence.

Note that this applies only to *implicit* quantification, without a
`forall`. If the user writes a `forall`, then we just use the order given.

ScopedSort is defined thusly (as proposed in #15743):
  * Work left-to-right through the input list, with a cursor.
  * If variable v at the cursor is depended on by any earlier variable w,
    move v immediately before the leftmost such w.

INVARIANT: The prefix of variables before the cursor form a valid telescope.

Note that ScopedSort makes sense only after type inference is done and all
types/kinds are fully settled and zonked.



Note [Excess polymorphism and join points]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In principle, if a function would be a join point except that it fails
the polymorphism rule (see Note [The polymorphism rule of join points] in
CoreSyn), it can still be made a join point with some effort. This is because
all tail calls must return the same type (they return to the same context!), and
thus if the return type depends on an argument, that argument must always be the
same.

For instance, consider:

.. code-block:: haskell

  let f :: forall a. a -> Char -> [a]
      f @a x c = ... f @a y 'a' ...
  in ... f @Int 1 'b' ... f @Int 2 'c' ...

(where the calls are tail calls). `f` fails the polymorphism rule because its
return type is [a], where [a] is bound. But since the type argument is always
'Int', we can rewrite it as:

.. code-block:: haskell

  let f' :: Int -> Char -> [Int]
      f' x c = ... f' y 'a' ...
  in ... f' 1 'b' ... f 2 'c' ...

and now we can make f' a join point:

.. code-block:: haskell

  join f' :: Int -> Char -> [Int]
       f' x c = ... jump f' y 'a' ...
  in ... jump f' 1 'b' ... jump f' 2 'c' ...

It's not clear that this comes up often, however. TODO: Measure how often and
add this analysis if necessary.  See #14620.




Note [Equality on AppTys]
~~~~~~~~~~~~~~~~~~~~~~~~~
In our cast-ignoring equality, we want to say that the following two
are equal:

.. code-block:: haskell

  (Maybe |> co) (Int |> co')   ~?       Maybe Int

But the left is an AppTy while the right is a TyConApp. The solution is
to use repSplitAppTy_maybe to break up the TyConApp into its pieces and
then continue. Easy to do, but also easy to forget to do.



Note [nonDetCmpType nondeterminism]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
nonDetCmpType is implemented in terms of nonDetCmpTypeX. nonDetCmpTypeX
uses nonDetCmpTc which compares TyCons by their Unique value. Using Uniques for
ordering leads to nondeterminism. We hit the same problem in the TyVarTy case,
comparing type variables is nondeterministic, note the call to nonDetCmpVar in
nonDetCmpTypeX.
See Note [Unique Determinism] for more details.


Note [typeKind vs tcTypeKind]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We have two functions to get the kind of a type

  * typeKind   ignores  the distinction between Constraint and *
  * tcTypeKind respects the distinction between Constraint and *

tcTypeKind is used by the type inference engine, for which Constraint
and * are different; after that we use typeKind.

See also Note [coreView vs tcView]



Note [Kinding rules for types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In typeKind we consider Constraint and (TYPE LiftedRep) to be identical.
We then have

.. code-block:: haskell

         t1 : TYPE rep1
         t2 : TYPE rep2
   (FUN) ----------------
         t1 -> t2 : Type

         ty : TYPE rep
         `a` is not free in rep
(FORALL) -----------------------
         forall a. ty : TYPE rep

In tcTypeKind we consider Constraint and (TYPE LiftedRep) to be distinct:

.. code-block:: haskell

          t1 : TYPE rep1
          t2 : TYPE rep2
    (FUN) ----------------
          t1 -> t2 : Type

.. code-block:: haskell

          t1 : Constraint
          t2 : TYPE rep
  (PRED1) ----------------
          t1 => t2 : Type

.. code-block:: haskell

          t1 : Constraint
          t2 : Constraint
  (PRED2) ---------------------
          t1 => t2 : Constraint

          ty : TYPE rep
          `a` is not free in rep
(FORALL1) -----------------------
          forall a. ty : TYPE rep

          ty : Constraint
(FORALL2) -------------------------
          forall a. ty : Constraint

Note that:
* The only way we distinguish '->' from '=>' is by the fact
  that the argument is a PredTy.  Both are FunTys



Note [Phantom type variables in kinds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider

.. code-block:: haskell

  type K (r :: RuntimeRep) = Type   -- Note 'r' is unused
  data T r :: K r                   -- T :: forall r -> K r
  foo :: forall r. T r

The body of the forall in foo's type has kind (K r), and
normally it would make no sense to have
   forall r. (ty :: K r)
because the kind of the forall would escape the binding
of 'r'.  But in this case it's fine because (K r) exapands
to Type, so we expliclity /permit/ the type
   forall r. T r

To accommodate such a type, in typeKind (forall a.ty) we use
occCheckExpand to expand any type synonyms in the kind of 'ty'
to eliminate 'a'.  See kinding rule (FORALL) in
Note [Kinding rules for types]

And in TcValidity.checkEscapingKind, we use also use
occCheckExpand, for the same reason.
---------------------------


Note [Occurs check expansion]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(occurCheckExpand tv xi) expands synonyms in xi just enough to get rid
of occurrences of tv outside type function arguments, if that is
possible; otherwise, it returns Nothing.

For example, suppose we have
  type F a b = [a]
Then
  occCheckExpand b (F Int b) = Just [Int]
but
  occCheckExpand a (F a Int) = Nothing

We don't promise to do the absolute minimum amount of expanding
necessary, but we try not to do expansions we don't need to.  We
prefer doing inner expansions first.  For example,
  type F a b = (a, Int, a, [a])
  type G b   = Char
We have
  occCheckExpand b (F (G b)) = Just (F Char)
even though we could also expand F to get rid of b.

