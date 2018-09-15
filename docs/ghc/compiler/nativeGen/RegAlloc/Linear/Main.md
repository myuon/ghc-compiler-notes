[[src]](https://github.com/ghc/ghc/tree/master/compiler/nativeGen/RegAlloc/Linear/Main.hs)

The algorithm is roughly:

  1) Compute strongly connected components of the basic block list.

  2) Compute liveness (mapping from pseudo register to
     point(s) of death?).

  3) Walk instructions in each basic block.  We keep track of
        (a) Free real registers (a bitmap?)
        (b) Current assignment of temporaries to machine registers and/or
            spill slots (call this the "assignment").
        (c) Partial mapping from basic block ids to a virt-to-loc mapping.
            When we first encounter a branch to a basic block,
            we fill in its entry in this table with the current mapping.

     For each instruction:
        (a) For each temporary *read* by the instruction:
            If the temporary does not have a real register allocation:
                - Allocate a real register from the free list.  If
                  the list is empty:
                  - Find a temporary to spill.  Pick one that is
                    not used in this instruction (ToDo: not
                    used for a while...)
                  - generate a spill instruction
                - If the temporary was previously spilled,
                  generate an instruction to read the temp from its spill loc.
            (optimisation: if we can see that a real register is going to
            be used soon, then don't use it for allocation).

        (b) For each real register clobbered by this instruction:
            If a temporary resides in it,
                If the temporary is live after this instruction,
                    Move the temporary to another (non-clobbered & free) reg,
                    or spill it to memory.  Mark the temporary as residing
                    in both memory and a register if it was spilled (it might
                    need to be read by this instruction).

            (ToDo: this is wrong for jump instructions?)

            We do this after step (a), because if we start with
               movq v1, %rsi
            which is an instruction that clobbers %rsi, if v1 currently resides
            in %rsi we want to get
               movq %rsi, %freereg
               movq %rsi, %rsi     -- will disappear
            instead of
               movq %rsi, %freereg
               movq %freereg, %rsi

        (c) Update the current assignment

        (d) If the instruction is a branch:
              if the destination block already has a register assignment,
                Generate a new block with fixup code and redirect the
                jump to the new block.
              else,
                Update the block id->assignment mapping with the current
                assignment.

        (e) Delete all register assignments for temps which are read
            (only) and die here.  Update the free register list.

        (f) Mark all registers clobbered by this instruction as not free,
            and mark temporaries which have been spilled due to clobbering
            as in memory (step (a) marks then as in both mem & reg).

        (g) For each temporary *written* by this instruction:
            Allocate a real register as for (b), spilling something
            else if necessary.
                - except when updating the assignment, drop any memory
                  locations that the temporary was previously in, since
                  they will be no longer valid after this instruction.

        (h) Delete all register assignments for temps which are
            written and die here (there should rarely be any).  Update
            the free register list.

        (i) Rewrite the instruction with the new mapping.

        (j) For each spilled reg known to be now dead, re-add its stack slot
            to the free list.



 from John Dias's patch 2008/10/16:
   The linear-scan allocator sometimes allocates a block
   before allocating one of its predecessors, which could lead to
   inconsistent allocations. Make it so a block is only allocated
   if a predecessor has set the "incoming" assignments for the block, or
   if it's the procedure's entry block.

   BL 2009/02: Careful. If the assignment for a block doesn't get set for
   some reason then this function will loop. We should probably do some
   more sanity checking to guard against this eventuality.


 BUGS: There are so many unreachable blocks in the code the warnings are overwhelming.
             pprTrace "RegAlloc.Linear.Main.process: no progress made, bailing out."
                (  text "Unreachable blocks:"
                $$ vcat (map ppr next_round)) 


          freeregs <- getFreeRegsR
          assig <- getAssigR
          pprTrace "raInsn" (text "ELIMINATED: " <> docToSDoc (pprInstr instr)
                        $$ ppr r_dying <+> ppr w_dying $$ text (show freeregs) $$ ppr assig) $ do
          

    freeregs <- getFreeRegsR
    assig    <- getAssigR
    pprDebugAndThen (defaultDynFlags Settings{ sTargetPlatform=platform } undefined) trace "genRaInsn"
        (ppr instr
                $$ text "r_dying      = " <+> ppr r_dying
                $$ text "w_dying      = " <+> ppr w_dying
                $$ text "virt_read    = " <+> ppr virt_read
                $$ text "virt_written = " <+> ppr virt_written
                $$ text "freeregs     = " <+> text (show freeregs)
                $$ text "assig        = " <+> ppr assig)
        $ do


reading

writing

 COMMENT (fsLit "spill load") : 