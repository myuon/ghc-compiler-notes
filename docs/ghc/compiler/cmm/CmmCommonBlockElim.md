[[src]](https://github.com/ghc/ghc/tree/master/compiler/cmm/CmmCommonBlockElim.hs)

  | equal     = pprTrace "equal" (vcat [ppr block, ppr block']) True
  | otherwise = pprTrace "not equal" (vcat [ppr block, ppr block']) False
  