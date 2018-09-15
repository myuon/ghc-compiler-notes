[[src]](https://github.com/ghc/ghc/tree/master/compiler/nativeGen/SPARC/CodeGen.hs)

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


the branch target


Conditional jumps are always to local labels, so we can use branch
instructions.  We peek at the arguments to decide what kind of
comparison to do.

SPARC: First, we have to ensure that the condition codes are set
according to the supplied comparison operation.  We generate slightly
different code for floating point comparisons, because a floating
point operation cannot directly precede a @BF@.  We assume the worst
and fill that slot with a @NOP@.

SPARC: Do not fill the delay slots here; you will confuse the register
allocator.



   Now the biggest nightmare---calls.  Most of the nastiness is buried in
   @get_arg@, which moves the arguments to the correct registers/stack
   locations.  Apart from that, the code is easy.

   The SPARC calling convention is an absolute
   nightmare.  The first 6x32 bits of arguments are mapped into
   %o0 through %o5, and the remaining arguments are dumped to the
   stack, beginning at [%sp+92].  (Note that %o6 == %sp.)

   If we have to put args on the stack, move %o6==%sp down by
   the number of words to go on the stack, to ensure there's enough space.

   According to Fraser and Hanson's lcc book, page 478, fig 17.2,
   16 words above the stack pointer is a word for the address of
   a structure return value.  I use this as a temporary location
   for moving values from float to int regs.  Certainly it isn't
   safe to put anything in the 16 words starting at %sp, since
   this area can get trashed at any time due to window overflows
   caused by signal handlers.

   A final complication (if the above isn't enough) is that
   we can't blithely calculate the arguments one by one into
   %o0 .. %o5.  Consider the following nested calls:

       fff a (fff b c)

   Naive code moves a into %o0, and (fff b c) into %o1.  Unfortunately
   the inner call will itself use %o0, which trashes the value put there
   in preparation for the outer call.  Upshot: we need to calculate the
   args into temporary regs, and move those to arg regs or onto the
   stack only immediately prior to the call proper.  Sigh.
