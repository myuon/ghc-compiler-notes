[[src]](https://github.com/ghc/ghc/tree/master/compiler/nativeGen/PPC/Ppr.hs)

pprInstr (COMMENT s) =
     if platformOS platform == OSLinux
     then text "# " <> ftext s
     else text "; " <> ftext s



pprInstr (SPILL reg slot)
   = hcat [
           text "\tSPILL",
        char '\t',
        pprReg reg,
        comma,
        text "SLOT" <> parens (int slot)]

pprInstr (RELOAD slot reg)
   = hcat [
           text "\tRELOAD",
        char '\t',
        text "SLOT" <> parens (int slot),
        comma,
        pprReg reg]
