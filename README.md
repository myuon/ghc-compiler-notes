# ghc-docs-book

[![Join the chat at https://gitter.im/ghc-compiler-notes/community](https://badges.gitter.im/ghc-compiler-notes/community.svg)](https://gitter.im/ghc-compiler-notes/community?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge) [![CircleCI](https://circleci.com/gh/myuon/ghc-compiler-notes.svg?style=svg)](https://circleci.com/gh/myuon/ghc-compiler-notes) [![Documentation Status](https://readthedocs.org/projects/ghc-compiler-notes/badge/?version=latest)](https://ghc-compiler-notes.readthedocs.io/en/latest/?badge=latest)


## Installation

Requirements:

* GHC (== 8.6.x): for `ghc-paths`
* GCC: for `CPP`
* Sphinx: for `docs` building

```bash
git clone https://github.com/myuon/ghc-compiler-notes.git
cd ghc-compiler-notes
git clone --depth 1 https://gitlab.haskell.org/ghc/ghc.git output/ghc
make generate
make docs
```

See `output/doc`

## Development

Using stack.

```shell
$ stack run
$ stack test --flag ghc-compiler-notes:dev
```

Using cabal.

```shell
$ cabal new-run
$ cabal new-test --enable-tests -fdev
```

## TODO

* Read Cabal config
  - For `MIN_VERSION_Cabal` / etc. macros.
  - e.g. Failed to parse `utils/haddock/haddock-test/src/Test/Haddock/Config.hs`.

* Sources using nested pragmas: https://ghc.haskell.org/trac/ghc/ticket/314
  - GHC 8.6.x has an issue for raw stream token with nested comments.
  - Fixed this issue in GHC 8.8.x.
  - e.g. Failed to parse `libraries/ghc-prim/GHC/Classes.hs`.

* Notes not followed standard style
  - e.g. "A note about the stupid context" at `compiler/basicTypes/DataCon.hs`.
  - e.g. "Note [About the NameSorts]" at `compiler/basicTypes/Name.hs`.
  - e.g. "Note [Continuation BlockId]" at `compiler/cmm/CmmNode.hs`.

* Collect note references
  - Collect `See Note [...]` and relate them to notes
