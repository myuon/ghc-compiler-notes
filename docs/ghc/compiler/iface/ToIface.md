[[src]](https://github.com/ghc/ghc/tree/master/compiler/iface/ToIface.hs)
# Conversion from Type to IfaceType


# Conversion of pattern synonyms


# Conversion of other things


# Conversion of expressions


### Note: Inlining and hs-boot files

Consider this example (Trac #10083, #12789):

    ---------- RSR.hs-boot ------------
    module RSR where
      data RSR
      eqRSR :: RSR -> RSR -> Bool