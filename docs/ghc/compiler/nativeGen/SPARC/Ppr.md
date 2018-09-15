[[src]](https://github.com/ghc/ghc/tree/master/compiler/nativeGen/SPARC/Ppr.hs)

pprRIReg :: LitString -> Bool -> RI -> Reg -> SDoc
pprRIReg name b ri reg1
  = hcat [
        char '\t',
        ptext name,
        if b then text "cc\t" else char '\t',
        pprRI ri,
        comma,
        pprReg reg1
    ]



pp_ld_lbracket :: SDoc
pp_ld_lbracket    = text "\tld\t["
