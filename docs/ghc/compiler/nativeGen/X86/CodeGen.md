[[src]](https://github.com/ghc/ghc/tree/master/compiler/nativeGen/X86/CodeGen.hs)

Now, given a tree (the argument to a CmmLoad) that references memory,
produce a suitable addressing mode.

A Rule of the Game (tm) for Amodes: use of the addr bit must
immediately follow use of the code part, since the code part puts
values in registers which the addr then refers to.  So you can't put
anything in between, lest it overwrite some of those registers.  If
you need to do some other computation between the code part and use of
the addr bit, first store the effective address from the amode in a
temporary, then do the other computation, and then use the temporary:

    code
    LEA amode, tmp
    ... other computation ...
    ... (tmp) ...


 Shift ops on x86s have constraints on their source, it
           either has to be Imm, CL or 1
            => trivialCode is not restrictive enough (sigh.)
        

False

False

False

 Case1: shift length as immediate 

 Case2: shift length is complex (non-immediate)
      * y must go in %ecx.
      * we cannot do y first *and* put its result in %ecx, because
        %ecx might be clobbered by x.
      * if we do y second, then x cannot be
        in a clobbered reg.  Also, we cannot clobber x's reg
        with the instruction itself.
      * so we can either:
        - do y first, put its result in a fresh tmp, then copy it to %ecx later
        - do y second and put its result into %ecx.  x gets placed in a fresh
          tmp.  This is likely to be better, because the reg alloc can
          eliminate this reg->reg move here (it won't eliminate the other one,
          because the move is into the fixed %ecx).
    

amount

no sse2

no sse2

the branch target


Conditional jumps are always to local labels, so we can use branch
instructions.  We peek at the arguments to decide what kind of
comparison to do.

I386: First, we have to ensure that the condition
codes are set according to the supplied comparison operation.


current argument


The Rules of the Game are:

* You cannot assume anything about the destination register dst;
  it may be anything, including a fixed reg.

* You may compute an operand into a fixed reg, but you may not
  subsequently change the contents of that fixed reg.  If you
  want to do so, first copy the value either to a temporary
  or into dst.  You are free to modify dst even if it happens
  to be a fixed reg -- that's not your problem.

* You cannot assume that a fixed reg will stay live over an
  arbitrary computation.  The same applies to the dst reg.

* Temporary regs obtained from getNewRegNat are distinct from
  each other and from all other regs, and stay live over
  arbitrary computations.

--------------------

SDM's version of The Rules:

* If getRegister returns Any, that means it can generate correct
  code which places the result in any register, period.  Even if that
  register happens to be read during the computation.

  Corollary #1: this means that if you are generating code for an
  operation with two arbitrary operands, you cannot assign the result
  of the first operand into the destination register before computing
  the second operand.  The second operand might require the old value
  of the destination register.

  Corollary #2: A function might be able to generate more efficient
  code if it knows the destination register is a new temporary (and
  therefore not read by any of the sub-computations).

* If getRegister returns Any, then the code it generates may modify only:
        (a) fresh temporaries
        (b) the destination register
        (c) known registers (eg. %ecx is used by shifts)
  In particular, it may *not* modify global registers, unless the global
  register happens to be the destination register.
