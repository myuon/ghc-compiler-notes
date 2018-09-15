[[src]](https://github.com/ghc/ghc/tree/master/compiler/main/Finder.hs)

(c) The University of Glasgow, 2000-2006

# Module Finder


        hPutStrLn stderr (showSDoc $
                vcat [text "Search" <+> ppr mod <+> sep (map (text. fst) exts)
                    , nest 2 (vcat (map text paths))
                    , case result of
                        Succeeded (loc, p) -> text "Found" <+> ppr loc
                        Failed fs          -> text "not found"])
