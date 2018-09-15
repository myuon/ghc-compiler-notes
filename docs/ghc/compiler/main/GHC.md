[[src]](https://github.com/ghc/ghc/tree/master/compiler/main/GHC.hs)

 ToDo:

  * inline bits of HscMain here to simplify layering: hscTcExpr, hscStmt.


 if isHomeModule (hsc_dflags hsc_env) mdl
        then return Nothing
        else 

 ToDo: Move the primary logic here to compiler/main/Packages.hs
-- | Return all /external/ modules available in the package database.
-- Modules from the current session (i.e., from the 'HomePackageTable') are
-- not included.  This includes module names which are reexported by packages.
packageDbModules :: GhcMonad m =>
                    Bool  -- ^ Only consider exposed packages.
                 -> m [Module]
packageDbModules only_exposed = do
   dflags <- getSessionDynFlags
   let pkgs = eltsUFM (pkgIdMap (pkgState dflags))
   return $
     [ mkModule pid modname
     | p <- pkgs
     , not only_exposed || exposed p
     , let pid = packageConfigId p
     , modname <- exposedModules p
               ++ map exportName (reexportedModules p) ]
               