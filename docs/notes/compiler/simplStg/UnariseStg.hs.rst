`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/simplStg/UnariseStg.hs>`_

====================
compiler/simplStg/UnariseStg.hs.rst
====================

Note [Unarisation]
~~~~~~~~~~~~~~~~~~
The idea of this pass is to translate away *all* unboxed-tuple and unboxed-sum
binders. So for example:

.. code-block:: haskell

  f (x :: (# Int, Bool #)) = f x + f (# 1, True #)

.. code-block:: haskell

  ==>

.. code-block:: haskell

  f (x1 :: Int) (x2 :: Bool) = f x1 x2 + f 1 True

It is important that we do this at the STG level and NOT at the Core level
because it would be very hard to make this pass Core-type-preserving. In this
example the type of 'f' changes, for example.

STG fed to the code generators *must* be unarised because the code generators do
not support unboxed tuple and unboxed sum binders natively.

In more detail: (see next note for unboxed sums)

Suppose that a variable x : (# t1, t2 #).

  * At the binding site for x, make up fresh vars  x1:t1, x2:t2

  * Extend the UnariseEnv   x :-> MultiVal [x1,x2]

  * Replace the binding with a curried binding for x1,x2

.. code-block:: haskell

       Lambda:   \x.e                ==>   \x1 x2. e
       Case alt: MkT a b x c d -> e  ==>   MkT a b x1 x2 c d -> e

  * Replace argument occurrences with a sequence of args via a lookup in
    UnariseEnv

.. code-block:: haskell

       f a b x c d   ==>   f a b x1 x2 c d

  * Replace tail-call occurrences with an unboxed tuple via a lookup in
    UnariseEnv

.. code-block:: haskell

       x  ==>  (# x1, x2 #)

.. code-block:: haskell

    So, for example

.. code-block:: haskell

       f x = x    ==>   f x1 x2 = (# x1, x2 #)

  * We /always/ eliminate a case expression when

       - It scrutinises an unboxed tuple or unboxed sum

       - The scrutinee is a variable (or when it is an explicit tuple, but the
         simplifier eliminates those)

.. code-block:: haskell

    The case alternative (there can be only one) can be one of these two
    things:

      - An unboxed tuple pattern. e.g.

.. code-block:: haskell

          case v of x { (# x1, x2, x3 #) -> ... }

.. code-block:: haskell

        Scrutinee has to be in form `(# t1, t2, t3 #)` so we just extend the
        environment with

.. code-block:: haskell

          x :-> MultiVal [t1,t2,t3]
          x1 :-> UnaryVal t1, x2 :-> UnaryVal t2, x3 :-> UnaryVal t3

      - A DEFAULT alternative. Just the same, without the bindings for x1,x2,x3

By the end of this pass, we only have unboxed tuples in return positions.
Unboxed sums are completely eliminated, see next note.



Note [Translating unboxed sums to unboxed tuples]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Unarise also eliminates unboxed sum binders, and translates unboxed sums in
return positions to unboxed tuples. We want to overlap fields of a sum when
translating it to a tuple to have efficient memory layout. When translating a
sum pattern to a tuple pattern, we need to translate it so that binders of sum
alternatives will be mapped to right arguments after the term translation. So
translation of sum DataCon applications to tuple DataCon applications and
translation of sum patterns to tuple patterns need to be in sync.

These translations work like this. Suppose we have

.. code-block:: haskell

  (# x1 | | ... #) :: (# t1 | t2 | ... #)

remember that t1, t2 ... can be sums and tuples too. So we first generate
layouts of those. Then we "merge" layouts of each alternative, which gives us a
sum layout with best overlapping possible.

Layout of a flat type 'ty1' is just [ty1].
Layout of a tuple is just concatenation of layouts of its fields.

For layout of a sum type,

  - We first get layouts of all alternatives.
  - We sort these layouts based on their "slot types".
  - We merge all the alternatives.

For example, say we have (# (# Int#, Char #) | (# Int#, Int# #) | Int# #)

  - Layouts of alternatives: [ [Word, Ptr], [Word, Word], [Word] ]
  - Sorted: [ [Ptr, Word], [Word, Word], [Word] ]
  - Merge all alternatives together: [ Ptr, Word, Word ]

We add a slot for the tag to the first position. So our tuple type is

.. code-block:: haskell

  (# Tag#, Any, Word#, Word# #)
  (we use Any for pointer slots)

Now, any term of this sum type needs to generate a tuple of this type instead.
The translation works by simply putting arguments to first slots that they fit
in. Suppose we had

.. code-block:: haskell

  (# (# 42#, 'c' #) | | #)

42# fits in Word#, 'c' fits in Any, so we generate this application:

.. code-block:: haskell

  (# 1#, 'c', 42#, rubbish #)

Another example using the same type: (# | (# 2#, 3# #) | #). 2# fits in Word#,
3# fits in Word #, so we get:

.. code-block:: haskell

  (# 2#, rubbish, 2#, 3# #).



Note [Types in StgConApp]
~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have this unboxed sum term:

.. code-block:: haskell

  (# 123 | #)

What will be the unboxed tuple representation? We can't tell without knowing the
type of this term. For example, these are all valid tuples for this:

.. code-block:: haskell

  (# 1#, 123 #)          -- when type is (# Int | String #)
  (# 1#, 123, rubbish #) -- when type is (# Int | Float# #)
  (# 1#, 123, rubbish, rubbish #)
                         -- when type is (# Int | (# Int, Int, Int #) #)

So we pass type arguments of the DataCon's TyCon in StgConApp to decide what
layout to use. Note that unlifted values can't be let-bound, so we don't need
types in StgRhsCon.



Note [UnariseEnv can map to literals]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
To avoid redundant case expressions when unarising unboxed sums, UnariseEnv
needs to map variables to literals too. Suppose we have this Core:

.. code-block:: haskell

  f (# x | #)

.. code-block:: haskell

  ==> (CorePrep)

.. code-block:: haskell

  case (# x | #) of y {
    _ -> f y
  }

.. code-block:: haskell

  ==> (MultiVal)

.. code-block:: haskell

  case (# 1#, x #) of [x1, x2] {
    _ -> f x1 x2
  }

To eliminate this case expression we need to map x1 to 1# in UnariseEnv:

.. code-block:: haskell

  x1 :-> UnaryVal 1#, x2 :-> UnaryVal x

so that `f x1 x2` becomes `f 1# x`.



Note [Unarisation and arity]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Because of unarisation, the arity that will be recorded in the generated info
table for an Id may be larger than the idArity. Instead we record what we call
the RepArity, which is the Arity taking into account any expanded arguments, and
corresponds to the number of (possibly-void) *registers* arguments will arrive
in.



Note [Post-unarisation invariants]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
STG programs after unarisation have these invariants:

  * No unboxed sums at all.

  * No unboxed tuple binders. Tuples only appear in return position.

  * DataCon applications (StgRhsCon and StgConApp) don't have void arguments.
    This means that it's safe to wrap `StgArg`s of DataCon applications with
    `StgCmmEnv.NonVoid`, for example.

  * Alt binders (binders in patterns) are always non-void.

