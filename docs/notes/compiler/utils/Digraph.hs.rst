`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/utils/Digraph.hs>`_

compiler/utils/Digraph.hs
=========================


Note [Nodes, keys, vertices]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/utils/Digraph.hs#L71>`__

 * A 'node' is a big blob of client-stuff

 * Each 'node' has a unique (client) 'key', but the latter
        is in Ord and has fast comparison

 * Digraph then maps each 'key' to a Vertex (Int) which is
        arranged densely in 0.n



Note [reduceNodesIntoVertices implementations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/utils/Digraph.hs#L158>`__

reduceNodesIntoVertices is parameterized by the container type.
This is to accomodate key types that don't have an Ord instance
and hence preclude the use of Data.Map. An example of such type
would be Unique, there's no way to implement Ord Unique
deterministically.

For such types, there's a version with a Uniquable constraint.
This leaves us with two versions of every function that depends on
reduceNodesIntoVertices, one with Ord constraint and the other with
Uniquable constraint.
For example: graphFromEdgedVerticesOrd and graphFromEdgedVerticesUniq.

The Uniq version should be a tiny bit more efficient since it uses
Data.IntMap internally.



Note [Deterministic SCC]
~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/utils/Digraph.hs#L273>`__

stronglyConnCompFromEdgedVerticesUniq,
stronglyConnCompFromEdgedVerticesUniqR,
stronglyConnCompFromEdgedVerticesOrd and
stronglyConnCompFromEdgedVerticesOrdR
provide a following guarantee:
Given a deterministically ordered list of nodes it returns a deterministically
ordered list of strongly connected components, where the list of vertices
in an SCC is also deterministically ordered.
Note that the order of edges doesn't need to be deterministic for this to work.
We use the order of nodes to normalize the order of edges.

