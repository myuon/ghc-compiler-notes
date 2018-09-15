[[src]](https://github.com/ghc/ghc/tree/master/compiler/cmm/CmmPipeline.hs)
### Note: inconsistent-pic-reg

On x86/Darwin, PIC is implemented by inserting a sequence like

    call 1f
 1: popl %reg

at the proc entry point, and then referring to labels as offsets from
%reg.  If we don't split proc points, then we could have many entry
points in a proc that would need this sequence, and each entry point
would then get a different value for %reg.  If there are any join
points, then at the join point we don't have a consistent value for
%reg, so we don't know how to refer to labels.

Hence, on x86/Darwin, we have to split proc points, and then each proc
point will get its own PIC initialisation sequence.

The situation is the same for ppc/Darwin. We use essentially the same
sequence to load the program counter onto reg:

    bcl  20,31,1f
 1: mflr reg

This isn't an issue on x86/ELF, where the sequence is

    call 1f
 1: popl %reg
    addl $_GLOBAL_OFFSET_TABLE_+(.-1b), %reg

so %reg always has a consistent value: the address of
_GLOBAL_OFFSET_TABLE_, regardless of which entry point we arrived via.



### Note: unreachable blocks

The control-flow optimiser sometimes leaves unreachable blocks behind
containing junk code.  These aren't necessarily a problem, but
removing them is good because it might save time in the native code
generator later.

