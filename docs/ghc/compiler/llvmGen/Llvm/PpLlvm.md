[[src]](https://github.com/ghc/ghc/tree/master/compiler/llvmGen/Llvm/PpLlvm.hs)

        | otherwise = error ("can't compare different types, left = "
                ++ (show $ getVarType left) ++ ", right = "
                ++ (show $ getVarType right))
        