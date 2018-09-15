[[src]](https://github.com/ghc/ghc/tree/master/compiler/cmm/CmmOpt.hs)
### Note: Comparison operators

If we have
   CmmCondBranch ((x>#y) == 1) t f
we really want to convert to
   CmmCondBranch (x>#y) t f

That's what the constant-folding operations on comparison operators do above.
