`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/basicTypes/NameEnv.hs>`_

compiler/basicTypes/NameEnv.hs
==============================


Note [depAnal determinism]
~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/basicTypes/NameEnv.hs#L53>`__

depAnal is deterministic provided it gets the nodes in a deterministic order.
The order of lists that get_defs and get_uses return doesn't matter, as these
are only used to construct the edges, and stronglyConnCompFromEdgedVertices is
deterministic even when the edges are not in deterministic order as explained
in Note [Deterministic SCC] in Digraph.

