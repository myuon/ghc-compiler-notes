[[src]](https://github.com/ghc/ghc/tree/master/compiler/nativeGen/RegAlloc/Graph/SpillCost.hs)

spillCost_chaitin
        :: SpillCostInfo
        -> Graph Reg RegClass Reg
        -> Reg
        -> Float

spillCost_chaitin info graph reg
        -- Spilling a live range that only lives for 1 instruction
        -- isn't going to help us at all - and we definitely want to avoid
        -- trying to re-spill previously inserted spill code.
        | lifetime <= 1         = 1/0

        -- It's unlikely that we'll find a reg for a live range this long
        -- better to spill it straight up and not risk trying to keep it around
        -- and have to go through the build/color cycle again.
        | lifetime > allocatableRegsInClass (regClass reg) * 10
        = 0

        -- Otherwise revert to chaitin's regular cost function.
        | otherwise     = fromIntegral (uses + defs)
                        / fromIntegral (nodeDegree graph reg)
        where (_, defs, uses, lifetime)
                = fromMaybe (reg, 0, 0, 0) $ lookupUFM info reg
