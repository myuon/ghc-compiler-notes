`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/nativeGen/RegAlloc/Graph/TrivColorable.hs>`_

====================
compiler/nativeGen/RegAlloc/Graph/TrivColorable.hs.rst
====================

Note [accSqueeze]
~~~~~~~~~~~~~~~~~~~~
BL 2007/09
Doing a nice fold over the UniqSet makes trivColorable use
32% of total compile time and 42% of total alloc when compiling SHA1.hs from darcs.
Therefore the UniqFM is made non-abstract and we use custom fold.

MS 2010/04
When converting UniqFM to use Data.IntMap, the fold cannot use UniqFM internal
representation any more. But it is imperative that the accSqueeze stops
the folding if the count gets greater or equal to maxCount. We thus convert
UniqFM to a (lazy) list, do the fold and stops if necessary, which was
the most efficient variant tried. Benchmark compiling 10-times SHA1.hs follows.
(original = previous implementation, folding = fold of the whole UFM,
 lazyFold = the current implementation,
 hackFold = using internal representation of Data.IntMap)

                                 original  folding   hackFold  lazyFold
 -O -fasm (used everywhere)      31.509s   30.387s   30.791s   30.603s
                                 100.00%   96.44%    97.72%    97.12%
 -fregs-graph                    67.938s   74.875s   62.673s   64.679s
                                 100.00%   110.21%   92.25%    95.20%
 -fregs-iterative                89.761s   143.913s  81.075s   86.912s
                                 100.00%   160.33%   90.32%    96.83%
 -fnew-codegen                   38.225s   37.142s   37.551s   37.119s
                                 100.00%   97.17%    98.24%    97.11%
 -fnew-codegen -fregs-graph      91.786s   91.51s    87.368s   86.88s
                                 100.00%   99.70%    95.19%    94.65%
 -fnew-codegen -fregs-iterative  206.72s   343.632s  194.694s  208.677s
                                 100.00%   166.23%   94.18%    100.95%

