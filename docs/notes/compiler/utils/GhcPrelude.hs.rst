`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/utils/GhcPrelude.hs>`_

compiler/utils/GhcPrelude.hs
============================


Note [Why do we import Prelude here?]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/utils/GhcPrelude.hs#L23>`__

The files ghc-boot-th.cabal, ghc-boot.cabal, ghci.cabal and
ghc-heap.cabal contain the directive default-extensions:
NoImplicitPrelude. There are two motivations for this:
  - Consistency with the compiler directory, which enables
    NoImplicitPrelude;
  - Allows loading the above dependent packages with ghc-in-ghci,
    giving a smoother development experience when adding new
    extensions.

