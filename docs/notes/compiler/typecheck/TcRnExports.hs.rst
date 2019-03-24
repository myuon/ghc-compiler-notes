`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcRnExports.hs>`_

Note [Exports of data families]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose you see (#5306)
        module M where
          import X( F )
          data instance F Int = FInt
What does M export?  AvailTC F [FInt]
                  or AvailTC F [F,FInt]?
The former is strictly right because F isn't defined in this module.
But then you can never do an explicit import of M, thus
    import M( F( FInt ) )
because F isn't exported by M.  Nor can you import FInt alone from here
    import M( FInt )
because we don't have syntax to support that.  (It looks like an import of
the type FInt.)

At one point I implemented a compromise:
  * When constructing exports with no export list, or with module M(
    module M ), we add the parent to the exports as well.
  * But not when you see module M( f ), even if f is a
    class method with a parent.
  * Nor when you see module M( module N ), with N /= M.

But the compromise seemed too much of a hack, so we backed it out.
You just have to use an explicit export list:
    module M( F(..) ) where ...



Note [Avails of associated data families]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose you have (#16077)

.. code-block:: haskell

    {-# LANGUAGE TypeFamilies #-}
    module A (module A) where

.. code-block:: haskell

    class    C a  where { data T a }
    instance C () where { data T () = D }

Because @A@ is exported explicitly, GHC tries to produce an export list
from the @GlobalRdrEnv@. In this case, it pulls out the following:

.. code-block:: haskell

    [ C defined at A.hs:4:1
    , T parent:C defined at A.hs:4:23
    , D parent:T defined at A.hs:5:35 ]

If map these directly into avails, (via 'availFromGRE'), we get
@[C{C;}, C{T;}, T{D;}]@, which eventually gets merged into @[C{C, T;}, T{D;}]@.
That's not right, because @T{D;}@ violates the AvailTC invariant: @T@ is
exported, but it isn't the first entry in the avail!

We work around this issue by expanding GREs where the parent and child
are both type constructors into two GRES.

.. code-block:: haskell

    T parent:C defined at A.hs:4:23

.. code-block:: haskell

      =>

.. code-block:: haskell

    [ T parent:C defined at A.hs:4:23
    , T defined at A.hs:4:23 ]

Then, we get  @[C{C;}, C{T;}, T{T;}, T{D;}]@, which eventually gets merged
into @[C{C, T;}, T{T, D;}]@ (which satsifies the AvailTC invariant).


Note [Modules without a module header]
--------------------------------------------------

The Haskell 2010 report says in section 5.1:

>> An abbreviated form of module, consisting only of the module body, is
>> permitted. If this is used, the header is assumed to be
>> ‘module Main(main) where’.

For modules without a module header, this is implemented the
following way:

If the module has a main function:
   Then create a module header and export the main function.
   This has the effect to mark the main function and all top level
   functions called directly or indirectly via main as 'used',
   and later on, unused top-level functions can be reported correctly.
   There is no distinction between GHC and GHCi.
If the module has NO main function:
   Then export all top-level functions. This marks all top level
   functions as 'used'.
   In GHCi this has the effect, that we don't get any 'non-used' warnings.
   In GHC, however, the 'has-main-module' check in the module
   compiler/typecheck/TcRnDriver (functions checkMain / check-main) fires,
   and we get the error:
      The IO action ‘main’ is not defined in module ‘Main’
Renaming exports lists is a minefield. Five different things can appear in
children export lists ( T(A, B, C) ).
1. Record selectors
2. Type constructors
3. Data constructors
4. Pattern Synonyms
5. Pattern Synonym Selectors

However, things get put into weird name spaces.
1. Some type constructors are parsed as variables (-.->) for example.
2. All data constructors are parsed as type constructors
3. When there is ambiguity, we default type constructors to data
constructors and require the explicit `type` keyword for type
constructors.

This function first establishes the possible namespaces that an
identifier might be in (`choosePossibleNameSpaces`).

Then for each namespace in turn, tries to find the correct identifier
there returning the first positive result or the first terminating
error.


