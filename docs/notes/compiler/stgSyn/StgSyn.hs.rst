`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/stgSyn/StgSyn.hs>`_

compiler/stgSyn/StgSyn.hs
=========================


Note [CAF consistency]
~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/stgSyn/StgSyn.hs#L488>`__

`topStgBindHasCafRefs` is only used by an assert (`consistentCafInfo` in
`CoreToStg`) to make sure CAF-ness predicted by `TidyPgm` is consistent with
reality.

Specifically, if the RHS mentions any Id that itself is marked
`MayHaveCafRefs`; or if the binding is a top-level updateable thunk; then the
`Id` for the binding should be marked `MayHaveCafRefs`. The potential trouble
is that `TidyPgm` computed the CAF info on the `Id` but some transformations
have taken place since then.

