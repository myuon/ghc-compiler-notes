[[src]](https://github.com/ghc/ghc/tree/master/compiler/simplStg/UnariseStg.hs)

(c) The GRASP/AQUA Project, Glasgow University, 1992-2012

### Note: Unarisation

The idea of this pass is to translate away *all* unboxed-tuple and unboxed-sum
binders. So for example:

  ==>

  f (x1 :: Int) (x2 :: Bool) = f x1 x2 + f 1 True

It is important that we do this at the STG level and NOT at the Core level
because it would be very hard to make this pass Core-type-preserving. In this
example the type of 'f' changes, for example.

STG fed to the code generators *must* be unarised because the code generators do
not support unboxed tuple and unboxed sum binders natively.

In more detail: (see next note for unboxed sums)

  * At the binding site for x, make up fresh vars  x1:t1, x2:t2

  * Extend the UnariseEnv   x :-> MultiVal [x1,x2]

  * Replace the binding with a curried binding for x1,x2

       Lambda:   \x.e                ==>   \x1 x2. e
       Case alt: MkT a b x c d -> e  ==>   MkT a b x1 x2 c d -> e

  * Replace argument occurrences with a sequence of args via a lookup in
    UnariseEnv

       f a b x c d   ==>   f a b x1 x2 c d

  * Replace tail-call occurrences with an unboxed tuple via a lookup in
    UnariseEnv

    So, for example

  * We /always/ eliminate a case expression when

       - It scrutinises an unboxed tuple or unboxed sum

       - The scrutinee is a variable (or when it is an explicit tuple, but the
         simplifier eliminates those)

    The case alternative (there can be only one) can be one of these two
    things:

      - An unboxed tuple pattern. e.g.

          x :-> MultiVal [t1,t2,t3]
          x1 :-> UnaryVal t1, x2 :-> UnaryVal t2, x3 :-> UnaryVal t3

      - A DEFAULT alternative. Just the same, without the bindings for x1,x2,x3

By the end of this pass, we only have unboxed tuples in return positions.
Unboxed sums are completely eliminated, see next note.

### Note: Translating unboxed sums to unboxed tuples

Unarise also eliminates unboxed sum binders, and translates unboxed sums in
return positions to unboxed tuples. We want to overlap fields of a sum when
translating it to a tuple to have efficient memory layout. When translating a
sum pattern to a tuple pattern, we need to translate it so that binders of sum
alternatives will be mapped to right arguments after the term translation. So
translation of sum DataCon applications to tuple DataCon applications and
translation of sum patterns to tuple patterns need to be in sync.

These translations work like this. Suppose we have

remember that t1, t2 ... can be sums and tuples too. So we first generate
layouts of those. Then we "merge" layouts of each alternative, which gives us a
sum layout with best overlapping possible.

Layout of a flat type 'ty1' is just [ty1].
Layout of a tuple is just concatenation of layouts of its fields.

For layout of a sum type,

  - We first get layouts of all alternatives.
  - We sort these layouts based on their "slot types".
  - We merge all the alternatives.

  - Layouts of alternatives: [ [Word, Ptr], [Word, Word], [Word] ]
  - Sorted: [ [Ptr, Word], [Word, Word], [Word] ]
  - Merge all alternatives together: [ Ptr, Word, Word ]

We add a slot for the tag to the first position. So our tuple type is

Now, any term of this sum type needs to generate a tuple of this type instead.
The translation works by simply putting arguments to first slots that they fit
in. Suppose we had

42# fits in Word#, 'c' fits in Any, so we generate this application:

### Note: Types in StgConApp

Suppose we have this unboxed sum term:

What will be the unboxed tuple representation? We can't tell without knowing the
type of this term. For example, these are all valid tuples for this:

So we pass type arguments of the DataCon's TyCon in StgConApp to decide what
layout to use. Note that unlifted values can't be let-bound, so we don't need
types in StgRhsCon.

### Note: UnariseEnv can map to literals

To avoid redundant case expressions when unarising unboxed sums, UnariseEnv
needs to map variables to literals too. Suppose we have this Core:

  ==> (CorePrep)

  ==> (MultiVal)

To eliminate this case expression we need to map x1 to 1# in UnariseEnv:

  x1 :-> UnaryVal 1#, x2 :-> UnaryVal x

so that `f x1 x2` becomes `f 1# x`.

### Note: Unarisation and arity

Because of unarisation, the arity that will be recorded in the generated info
table for an Id may be larger than the idArity. Instead we record what we call
the RepArity, which is the Arity taking into account any expanded arguments, and
corresponds to the number of (possibly-void) *registers* arguments will arrive
in.

### Note: Post-unarisation invariants

STG programs after unarisation have these invariants:

  * No unboxed sums at all.

  * No unboxed tuple binders. Tuples only appear in return position.

  * DataCon applications (StgRhsCon and StgConApp) don't have void arguments.
    This means that it's safe to wrap `StgArg`s of DataCon applications with
    `StgCmmEnv.NonVoid`, for example.

  * Alt binders (binders in patterns) are always non-void.



For arguments (StgArg) and binders (Id) we have two kind of unarisation:

  - When unarising function arg binders and arguments, we don't want to remove
    void binders and arguments. For example,

    Here after unarise we should still get a function with arity 3. Similarly
    in the call site we shouldn't remove void arguments:

    When unarising <body>, we extend the environment with these binders:

      x :-> MultiVal [], y :-> MultiVal [], z :-> MultiVal []

    Because their rep types are `MultiRep []` (aka. void). This means that when
    we see `x` in a function argument position, we actually replace it with a
    void argument. When we see it in a DataCon argument position, we just get
    rid of it, because DataCon applications in STG are always saturated.

  - When unarising case alternative binders we remove void binders, but we
    still update the environment the same way, because those binders may be
    used in the RHS. Example:

    We know that y can't be void, because we don't scrutinize voids, so x will
    be unarised to some number of arguments, and those arguments will have at
    least one non-void thing. So in the rho we will have something like:

      x :-> MultiVal [xu1, xu2]

    Now, after we eliminate void binders in the pattern, we get exactly the same
    number of binders, and extend rho again with these:

      x1 :-> UnaryVal xu1
      x2 :-> MultiVal [] -- x2 is void
      x3 :-> UnaryVal xu2

    Now when we see x2 in a function argument position or in return position, we
    generate void#. In constructor argument position, we just remove it.

So in short, when we have a void id,

  - We keep it if it's a lambda argument binder or
                       in argument position of an application.

  - We remove it if it's a DataCon field binder or
                         in argument position of a DataCon application.
