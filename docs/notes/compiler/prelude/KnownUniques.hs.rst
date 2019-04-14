`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/prelude/KnownUniques.hs>`_

compiler/prelude/KnownUniques.hs
================================


Note [Uniques for tuple type and data constructors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/prelude/KnownUniques.hs#L118>`__

Wired-in type constructor keys occupy *two* slots:
   * u: the TyCon itself
   * u+1: the TyConRepName of the TyCon

Wired-in tuple data constructor keys occupy *three* slots:
   * u: the DataCon itself
   * u+1: its worker Id
   * u+2: the TyConRepName of the promoted TyCon

