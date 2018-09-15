[[src]](https://github.com/ghc/ghc/tree/master/compiler/backpack/NameShape.hs)

data NameShape = NameShape {
        ns_mod_name :: ModuleName,
        ns_exports :: [AvailInfo],
        ns_map :: OccEnv Name
    }


# Name substitutions


# AvailInfo merging


# AvailInfo unification


 hole name 