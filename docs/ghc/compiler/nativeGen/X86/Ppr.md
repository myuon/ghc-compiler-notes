[[src]](https://github.com/ghc/ghc/tree/master/compiler/nativeGen/X86/Ppr.hs)

pprInstr (SPILL reg slot)
   = hcat [
        text "\tSPILL",
        char ' ',
        pprUserReg reg,
        comma,
        text "SLOT" <> parens (int slot)]

pprInstr (RELOAD slot reg)
   = hcat [
        text "\tRELOAD",
        char ' ',
        text "SLOT" <> parens (int slot),
        comma,
        pprUserReg reg]


 A hack.  The Intel documentation says that "The two and three
   operand forms [of IMUL] may also be used with unsigned operands
   because the lower half of the product is the same regardless if
   (sic) the operands are signed or unsigned.  The CF and OF flags,
   however, cannot be used to determine if the upper half of the
   result is non-zero."  So there.


 Gruesome swamp follows.  If you're unfortunate enough to have ventured
   this far into the jungle AND you give a Rat's Ass (tm) what's going
   on, here's the deal.  Generate code to do a floating point comparison
   of src1 and src2, of kind cond, and set the Zero flag if true.

   The complications are to do with handling NaNs correctly.  We want the
   property that if either argument is NaN, then the result of the
   comparison is False ... except if we're comparing for inequality,
   in which case the answer is True.

   Here's how the general (non-inequality) case works.  As an
   example, consider generating the an equality test:

     pushl %eax         -- we need to mess with this
     <get src1 to top of FPU stack>
     fcomp <src2 location in FPU stack> and pop pushed src1
                -- Result of comparison is in FPU Status Register bits
                -- C3 C2 and C0
     fstsw %ax  -- Move FPU Status Reg to %ax
     sahf       -- move C3 C2 C0 from %ax to integer flag reg
     -- now the serious magic begins
     setpo %ah     -- %ah = if comparable(neither arg was NaN) then 1 else 0
     sete  %al     -- %al = if arg1 == arg2 then 1 else 0
     andb %ah,%al  -- %al &= %ah
                   -- so %al == 1 iff (comparable && same); else it holds 0
     decb %al      -- %al == 0, ZeroFlag=1  iff (comparable && same);
                      else %al == 0xFF, ZeroFlag=0
     -- the zero flag is now set as we desire.
     popl %eax

   The special case of inequality differs thusly:

     setpe %ah     -- %ah = if incomparable(either arg was NaN) then 1 else 0
     setne %al     -- %al = if arg1 /= arg2 then 1 else 0
     orb %ah,%al   -- %al = if (incomparable || different) then 1 else 0
     decb %al      -- if (incomparable || different) then (%al == 0, ZF=1)
                                                     else (%al == 0xFF, ZF=0)


 On the 486, the flags set by FP compare are the unsigned ones!
           (This looks like a HACK to me.  WDP 96/03)
        