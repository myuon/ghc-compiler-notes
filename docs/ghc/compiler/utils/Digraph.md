[[src]](https://github.com/ghc/ghc/tree/master/compiler/utils/Digraph.hs)
# Graphs and Graph Construction


### Note: Nodes, keys, vertices

 * A 'node' is a big blob of client-stuff

 * Each 'node' has a unique (client) 'key', but the latter
        is in Ord and has fast comparison

 * Digraph then maps each 'key' to a Vertex (Int) which is
        arranged densely in 0.n


### Note: reduceNodesIntoVertices implementations

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


# SCC


# Strongly Connected Component wrappers for Graph


Note: the components are returned topologically sorted: later components
depend on earlier ones, but not vice versa i.e. later components only have
edges going from them to earlier ones.


### Note: Deterministic SCC

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


# Misc wrappers for Graph


# Showing Graphs


# IntGraphs



------------------------------------------------------------
-- Depth first search numbering
------------------------------------------------------------



------------------------------------------------------------
-- Finding reachable vertices
------------------------------------------------------------



------------------------------------------------------------
-- Total ordering on groups of vertices
------------------------------------------------------------

The plan here is to extract a list of groups of elements of the graph
such that each group has no dependence except on nodes in previous
groups (i.e. in particular they may not depend on nodes in their own
group) and is maximal such group.

Clearly we cannot provide a solution for cyclic graphs.

We proceed by iteratively removing elements with no outgoing edges
and their associated edges from the graph.

This probably isn't very efficient and certainly isn't very clever.
