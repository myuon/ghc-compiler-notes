[[src]](https://github.com/ghc/ghc/tree/master/compiler/nativeGen/PPC/CodeGen.hs)

Simple support for generating 64-bit code (ie, 64 bit values and 64
bit assignments) on 32-bit platforms.  Unlike the main code generator
we merely shoot for generating working code as simply as possible, and
pay little attention to code quality.  Specifically, there is no
attempt to deal cleverly with the fixed-vs-floating register
distinction; all values are generated into (pairs of) floating
registers, even if this would mean some redundant reg-reg moves as a
result.  Only one of the VRegUniques is returned, since it will be
of the VRegUniqueLo form, and the upper-half VReg can be determined
by applying getHiVRegFromLo to it.



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


### Note: Power instruction format

The Power ISA specification document can be obtained from www.power.org.


the branch target


Conditional jumps are always to local labels, so we can use branch
instructions.  We peek at the arguments to decide what kind of
comparison to do.



    The PowerPC calling convention for Darwin/Mac OS X
    is described in Apple's document
    "Inside Mac OS X - Mach-O Runtime Architecture".

    PowerPC Linux uses the System V Release 4 Calling Convention
    for PowerPC. It is described in the
    "System V Application Binary Interface PowerPC Processor Supplement".

    Both conventions are similar:
    Parameters may be passed in general-purpose registers starting at r3, in
    floating point registers starting at f1, or on the stack.

    But there are substantial differences:
    * The number of registers used for parameter passing and the exact set of
      nonvolatile registers differs (see MachRegs.hs).
    * On Darwin, stack space is always reserved for parameters, even if they are
      passed in registers. The called routine may choose to save parameters from
      registers to the corresponding space on the stack.
    * On Darwin, a corresponding amount of GPRs is skipped when a floating point
      parameter is passed in an FPR.
    * SysV insists on either passing I64 arguments on the stack, or in two GPRs,
      starting with an odd-numbered GPR. It may skip a GPR to achieve this.
      Darwin just treats an I64 like two separate II32s (high word first).
    * I64 and FF64 arguments are 8-byte aligned on the stack for SysV, but only
      4-byte aligned like everything else on Darwin.
    * The SysV spec claims that FF32 is represented as FF64 on the stack. GCC on
      PowerPC Linux does not agree, so neither do we.

    PowerPC 64 Linux uses the System V Release 4 Calling Convention for
    64-bit PowerPC. It is specified in
    "64-bit PowerPC ELF Application Binary Interface Supplement 1.9"
    (PPC64 ELF v1.9).
    PowerPC 64 Linux in little endian mode uses the "Power Architecture 64-Bit
    ELF V2 ABI Specification -- OpenPOWER ABI for Linux Supplement"
    (PPC64 ELF v2).
    AIX follows the "PowerOpen ABI: Application Binary Interface Big-Endian
    32-Bit Hardware Implementation"

    According to all conventions, the parameter area should be part of the
    caller's stack frame, allocated in the caller's prologue code (large enough
    to hold the parameter lists for all called routines). The NCG already
    uses the stack for register spilling, leaving 64 bytes free at the top.
    If we need a larger parameter area than that, we just allocate a new stack
    frame just before ccalling.



Wolfgang's PowerPC version of The Rules:

A slightly modified version of The Rules to take advantage of the fact
that PowerPC instructions work on all registers and don't implicitly
clobber any fixed registers.

* The only expression for which getRegister returns Fixed is (CmmReg reg).

* If getRegister returns Any, then the code it generates may modify only:
        (a) fresh temporaries
        (b) the destination register
  It may *not* modify global registers, unless the global
  register happens to be the destination register.
  It may not clobber any other registers. In fact, only ccalls clobber any
  fixed registers.
  Also, it may not modify the counter register (used by genCCall).

  Corollary: If a getRegister for a subexpression returns Fixed, you need
  not move it to a fresh temporary before evaluating the next subexpression.
  The Fixed register won't be modified.
  Therefore, we don't need a counterpart for the x86's getStableReg on PPC.

* SDM's First Rule is valid for PowerPC, too: subexpressions can depend on
  the value of the destination register.
