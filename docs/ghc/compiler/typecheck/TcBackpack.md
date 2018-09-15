[[src]](https://github.com/ghc/ghc/tree/master/compiler/typecheck/TcBackpack.hs)
 from local sig 


inheritedSigPvpWarning :: WarningTxt
inheritedSigPvpWarning =
    WarningTxt (noLoc NoSourceText) [noLoc (StringLiteral NoSourceText (fsLit msg))]
  where
    msg = "Inherited requirements from non-signature libraries (libraries " ++
          "with modules) should not be used, as this mode of use is not " ++
          "compatible with PVP-style version bounds.  Instead, copy the " ++
          "declaration to the local hsig file or move the signature to a " ++
          "library of its own and add that library as a dependency."


 empty 


        -- TODO: Warnings are transitive, but this is not what we want here:
        -- if a module reexports an entity from a signature, that should be OK.
        -- Not supported in current warning framework
        warns | null warn_occs = NoWarnings
              | otherwise = WarnSome $ map (\o -> (o, inheritedSigPvpWarning)) warn_occs
        

 -- NB: This is commented out, because warns above is disabled.
    -- If you tried to explicitly export an identifier that has a warning
    -- attached to it, that's probably a mistake.  Warn about it.
    case mb_lies of
      Nothing -> return ()
      Just lies ->
        forM_ (concatMap (\(L loc x) -> map (L loc) (ieNames x)) lies) $ \(L loc n) ->
          setSrcSpan loc $
            unless (nameOccName n `elemOccSet` ok_to_use) $
                addWarn NoReason $ vcat [
                    text "Exported identifier" <+> quotes (ppr n) <+> text "will cause warnings if used.",
                    parens (text "To suppress this warning, remove" <+> quotes (ppr n) <+> text "from the export list of this signature.")
                    ]
    

 safe 

 boot 