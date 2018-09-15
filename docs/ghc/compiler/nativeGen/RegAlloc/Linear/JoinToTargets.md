[[src]](https://github.com/ghc/ghc/tree/master/compiler/nativeGen/RegAlloc/Linear/JoinToTargets.hs)
              -- debugging
                pprTrace
                        ("joinToTargets: making fixup code")
                        (vcat   [ text "        in block: "     <> ppr block_id
                                , text " jmp instruction: "     <> ppr instr
                                , text "  src assignment: "     <> ppr src_assig
                                , text " dest assignment: "     <> ppr dest_assig
                                , text "  movement graph: "     <> ppr graph
                                , text "   sccs of graph: "     <> ppr sccs
                                , text ""])
                        (return ())


              pprTrace
                        ("joinToTargets: fixup code is:")
                        (vcat   [ ppr block
                                , text ""])
                        (return ())
