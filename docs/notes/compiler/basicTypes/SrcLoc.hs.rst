`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/basicTypes/SrcLoc.hs>`_

Note [HasSrcSpan Typeclass]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

To be able to uniformly set/get source location spans (of `SrcSpan`) in
syntactic entities (`HsSyn`), we use the typeclass `HasSrcSpan`.
More details can be found at the following wiki page
  ImplementingTreesThatGrow/HandlingSourceLocations

For most syntactic entities, the source location spans are stored in
a syntactic entity by a wapper constuctor (introduced by TTG's
new constructor extension), e.g., by `NewPat (WrapperPat sp pat)`
for a source location span `sp` and a pattern `pat`.

