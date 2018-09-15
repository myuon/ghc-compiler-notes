[[src]](https://github.com/ghc/ghc/tree/master/compiler/utils/UnVarGraph.hs)


Copyright (c) 2014 Joachim Breitner

A data structure for undirected graphs of variables
(or in plain terms: Sets of unordered pairs of numbers)


This is very specifically tailored for the use in CallArity. In particular it
stores the graph as a union of complete and complete bipartite graph, which
would be very expensive to store as sets of edges or as adjanceny lists.

It does not normalize the graphs. This means that g `unionUnVarGraph` g is
equal to g, but twice as expensive and large.




Premature optimisation, it seems.
unionUnVarGraph (UnVarGraph [CBPG s1 s2]) (UnVarGraph [CG s3, CG s4])
    | s1 == s3 && s2 == s4
    = pprTrace "unionUnVarGraph fired" empty $
      completeGraph (s1 `unionUnVarSet` s2)
unionUnVarGraph (UnVarGraph [CBPG s1 s2]) (UnVarGraph [CG s3, CG s4])
    | s2 == s3 && s1 == s4
    = pprTrace "unionUnVarGraph fired2" empty $
      completeGraph (s1 `unionUnVarSet` s2)
