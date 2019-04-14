`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/hieFile/HieTypes.hs>`_

compiler/hieFile/HieTypes.hs
============================


Note [Efficient serialization of redundant type info]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/hieFile/HieTypes.hs#L101>`__

The type information in .hie files is highly repetitive and redundant. For
example, consider the expression

::

    const True 'a'

..

There is a lot of shared structure between the types of subterms:

  * const True 'a' ::                 Bool
  * const True     ::         Char -> Bool
  * const          :: Bool -> Char -> Bool

Since all 3 of these types need to be stored in the .hie file, it is worth
making an effort to deduplicate this shared structure. The trick is to define
a new data type that is a flattened version of 'Type':

::

    data HieType a = HAppTy a a  -- data Type = AppTy Type Type
                   | HFunTy a a  --           | FunTy Type Type
                   | ...

..

::

    type TypeIndex = Int

..

Types in the final AST are stored in an 'A.Array TypeIndex (HieType TypeIndex)',
where the 'TypeIndex's in the 'HieType' are references to other elements of the
array. Types recovered from GHC are deduplicated and stored in this compressed
form with sharing of subtrees.

