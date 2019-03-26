`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/prelude/TysWiredIn.hs>`_

compiler/prelude/TysWiredIn.hs
==============================


Note [Wiring in RuntimeRep]
~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/prelude/TysWiredIn.hs#L176>`__

The RuntimeRep type (and friends) in GHC.Types has a bunch of constructors,
making it a pain to wire in. To ease the pain somewhat, we use lists of
the different bits, like Uniques, Names, DataCons. These lists must be
kept in sync with each other. The rule is this: use the order as declared
in GHC.Types. All places where such lists exist should contain a reference
to this Note, so a search for this Note's name should find all the lists.



Note [Any types]
~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/prelude/TysWiredIn.hs#L314>`__

The type constructor Any,

::

    type family Any :: k where { }

It has these properties:

  * Note that 'Any' is kind polymorphic since in some program we may
    need to use Any to fill in a type variable of some kind other than *
    (see #959 for examples).  Its kind is thus `forall k. k``.

  * It is defined in module GHC.Types, and exported so that it is
    available to users.  For this reason it's treated like any other
    wired-in type:
      - has a fixed unique, anyTyConKey,
      - lives in the global name cache

  * It is a *closed* type family, with no instances.  This means that
    if   ty :: '(k1, k2)  we add a given coercion
             g :: ty ~ (Fst ty, Snd ty)
    If Any was a *data* type, then we'd get inconsistency because 'ty'
    could be (Any '(k1,k2)) and then we'd have an equality with Any on
    one side and '(,) on the other. See also #9097 and #9636.

  * When instantiated at a lifted type it is inhabited by at least one value,
    namely bottom

  * You can safely coerce any /lifted/ type to Any, and back with unsafeCoerce.

  * It does not claim to be a *data* type, and that's important for
    the code generator, because the code gen may *enter* a data value
    but never enters a function value.

  * It is wired-in so we can easily refer to it where we don't have a name
    environment (e.g. see Rules.matchRule for one example)

  * If (Any k) is the type of a value, it must be a /lifted/ value. So
    if we have (Any @(TYPE rr)) then rr must be 'LiftedRep.  See
    Note [TYPE and RuntimeRep] in TysPrim.  This is a convenient
    invariant, and makes isUnliftedTyCon well-defined; otherwise what
    would (isUnliftedTyCon Any) be?

It's used to instantiate un-constrained type variables after type checking. For
example, 'length' has type

::

  length :: forall a. [a] -> Int

and the list datacon for the empty list has type

::

  [] :: forall a. [a]

In order to compose these two terms as @length []@ a type
application is required, but there is no constraint on the
choice.  In this situation GHC uses 'Any',

> length (Any *) ([] (Any *))

Above, we print kinds explicitly, as if with --fprint-explicit-kinds.

The Any tycon used to be quite magic, but we have since been able to
implement it merely with an empty kind polymorphic type family. See #10886 for a
bit of history.



Note [One-tuples]
~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/prelude/TysWiredIn.hs#L678>`__

GHC supports both boxed and unboxed one-tuples:
 - Unboxed one-tuples are sometimes useful when returning a
   single value after CPR analysis
 - A boxed one-tuple is used by DsUtils.mkSelectorBinds, when
   there is just one binder
Basically it keeps everythig uniform.

However the /naming/ of the type/data constructors for one-tuples is a
bit odd:
  3-tuples:  (,,)   (,,)#
  2-tuples:  (,)    (,)#
  1-tuples:  ??
  0-tuples:  ()     ()#

Zero-tuples have used up the logical name. So we use 'Unit' and 'Unit#'
for one-tuples.  So in ghc-prim:GHC.Tuple we see the declarations:
  data ()     = ()
  data Unit a = Unit a
  data (a,b)  = (a,b)

NB (Feb 16): for /constraint/ one-tuples I have 'Unit%' but no class
decl in GHC.Classes, so I think this part may not work properly. But
it's unused I think.



Note [Boxing primitive types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/prelude/TysWiredIn.hs#L1291>`__

For a handful of primitive types (Int, Char, Word, Flaot, Double),
we can readily box and an unboxed version (Int#, Char# etc) using
the corresponding data constructor.  This is useful in a couple
of places, notably let-floating

