# ghc-docs-book

[![Join the chat at https://gitter.im/ghc-compiler-notes/community](https://badges.gitter.im/ghc-compiler-notes/community.svg)](https://gitter.im/ghc-compiler-notes/community?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge) [![Build Status](https://travis-ci.org/myuon/ghc-compiler-notes.svg?branch=master)](https://travis-ci.org/myuon/ghc-compiler-notes)

## Installation

Requirements:

* GHC (== 8.6.x): for `ghc-paths`
* GCC: for `CPP`

```bash
git clone https://github.com/myuon/ghc-compiler-notes.git
cd ghc-compiler-notes
git clone https://gitlab.haskell.org/ghc/ghc.git output/ghc
cabal new-build
cabal new-exec -- ghc-compiler-notes conf/ghc-8.6.4.yml
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
