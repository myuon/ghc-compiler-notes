`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/libraries/template-haskell/Language/Haskell/TH/Syntax.hs>`_

libraries/template-haskell/Language/Haskell/TH/Syntax.hs
========================================================


Note [Role of TExp]
~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/libraries/template-haskell/Language/Haskell/TH/Syntax.hs#L255>`__

TExp's argument must have a nominal role, not phantom as would
be inferred (#8459).  Consider

::

  e :: TExp Age
  e = MkAge 3

::

  foo = $(coerce e) + 4::Int

The splice will evaluate to (MkAge 3) and you can't add that to
4::Int. So you can't coerce a (TExp Age) to a (TExp Int). --------------------------------------------------
 Packaged versions for the programmer, hiding the Quasi-ness



Note [Name lookup]
~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/libraries/template-haskell/Language/Haskell/TH/Syntax.hs#L342>`__




Note [Data for non-algebraic types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/libraries/template-haskell/Language/Haskell/TH/Syntax.hs#L897>`__

Class Data was originally intended for algebraic data types.  But
it is possible to use it for abstract types too.  For example, in
package `text` we find

::

  instance Data Text where
    ...
    toConstr _ = packConstr

::

  packConstr :: Constr
  packConstr = mkConstr textDataType "pack" [] Prefix

Here `packConstr` isn't a real data constructor, it's an ordinary
function.  Two complications

* In such a case, we must take care to build the Name using
  mkNameG_v (for values), not mkNameG_d (for data constructors).
  See #10796.

* The pseudo-constructor is named only by its string, here "pack".
  But 'dataToQa' needs the TyCon of its defining module, and has
  to assume it's defined in the same module as the TyCon itself.
  But nothing enforces that; #12596 shows what goes wrong if
  "pack" is defined in a different module than the data type "Text".



Note [Unresolved infix]
~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/libraries/template-haskell/Language/Haskell/TH/Syntax.hs#L1543>`__




Note [GADT return type]
~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/libraries/template-haskell/Language/Haskell/TH/Syntax.hs#L2061>`__

The return type of a GADT constructor does not necessarily match the name of
the data type:

type S = T

data T a where
    MkT :: S Int


type S a = T

data T a where
    MkT :: S Char Int


type Id a = a
type S a = T

data T a where
    MkT :: Id (S Char Int)


That is why we allow the return type stored by a constructor to be an
arbitrary type. See also #11341



Note [Representing concrete syntax in types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/libraries/template-haskell/Language/Haskell/TH/Syntax.hs#L2192>`__

Haskell has a rich concrete syntax for types, including
  t1 -> t2, (t1,t2), [t], and so on
In TH we represent all of this using AppT, with a distinguished
type constructor at the head.  So,
  Type              TH representation
  -----------------------------------------------
  t1 -> t2          ArrowT `AppT` t2 `AppT` t2
  [t]               ListT `AppT` t
  (t1,t2)           TupleT 2 `AppT` t1 `AppT` t2
  '(t1,t2)          PromotedTupleT 2 `AppT` t1 `AppT` t2

But if the original HsSyn used prefix application, we won't use
these special TH constructors.  For example
  [] t              ConT "[]" `AppT` t
  (->) t            ConT "->" `AppT` t
In this way we can faithfully represent in TH whether the original
HsType used concrete syntax or not.

The one case that doesn't fit this pattern is that of promoted lists
  '[ Maybe, IO ]    PromotedListT 2 `AppT` t1 `AppT` t2
but it's very smelly because there really is no type constructor
corresponding to PromotedListT. So we encode HsExplicitListTy with
PromotedConsT and PromotedNilT (which *do* have underlying type
constructors):
  '[ Maybe, IO ]    PromotedConsT `AppT` Maybe `AppT`
                    (PromotedConsT  `AppT` IO `AppT` PromotedNilT)
---------------------------------------------------
              Internal helper functions
---------------------------------------------------

