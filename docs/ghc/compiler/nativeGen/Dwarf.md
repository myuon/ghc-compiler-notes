[[src]](https://github.com/ghc/ghc/tree/master/compiler/nativeGen/Dwarf.hs)
### Note: Splitting DebugBlocks

DWARF requires that we break up the the nested DebugBlocks produced from
the C-- AST. For instance, we begin with tick trees containing nested procs.
For example,

    proc A [tick1, tick2]
      block B [tick3]
        proc C [tick4]

when producing DWARF we need to procs (which are represented in DWARF as
TAG_subprogram DIEs) to be top-level DIEs. debugSplitProcs is responsible for
this transform, pulling out the nested procs into top-level procs.

However, in doing this we need to be careful to preserve the parentage of the
nested procs. This is the reason DebugBlocks carry the dblParent field, allowing
us to reorganize the above tree as,

    proc A [tick1, tick2]
      block B [tick3]
    proc C [tick4] parent=B

Here we have annotated the new proc C with an attribute giving its original
parent, B.
