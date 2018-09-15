[[src]](https://github.com/ghc/ghc/tree/master/compiler/nativeGen/SPARC/Regs.hs)

        The SPARC has 64 registers of interest; 32 integer registers and 32
        floating point registers.  The mapping of STG registers to SPARC
        machine registers is defined in StgRegs.h.  We are, of course,
        prepared for any eventuality.

        The whole fp-register pairing thing on sparcs is a huge nuisance.  See
        includes/stg/MachRegs.h for a description of what's going on
        here.



fPair :: Reg -> Maybe Reg
fPair (RealReg n)
        | n >= 32 && n `mod` 2 == 0  = Just (RealReg (n+1))

fPair (VirtualRegD u)
        = Just (VirtualRegHi u)

fPair reg
        = trace ("MachInstrs.fPair: can't get high half of supposed double reg " ++ showPpr reg)
                Nothing
