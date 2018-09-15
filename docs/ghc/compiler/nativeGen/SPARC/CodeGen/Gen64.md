[[src]](https://github.com/ghc/ghc/tree/master/compiler/nativeGen/SPARC/CodeGen/Gen64.hs)
     pprTrace "assignMem_I64Code"
        (vcat   [ text "addrTree:  " <+> ppr addrTree
                , text "valueTree: " <+> ppr valueTree
                , text "vcode:"
                , vcat $ map ppr $ fromOL vcode
                , text ""
                , text "acode:"
                , vcat $ map ppr $ fromOL acode ])
       $ 