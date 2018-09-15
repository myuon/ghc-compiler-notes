[[src]](https://github.com/ghc/ghc/tree/master/compiler/nativeGen/X86/Instr.hs)

Intel, in their infinite wisdom, selected a stack model for floating
point registers on x86.  That might have made sense back in 1979 --
nowadays we can see it for the nonsense it really is.  A stack model
fits poorly with the existing nativeGen infrastructure, which assumes
flat integer and FP register sets.  Prior to this commit, nativeGen
could not generate correct x86 FP code -- to do so would have meant
somehow working the register-stack paradigm into the register
allocator and spiller, which sounds very difficult.

We have decided to cheat, and go for a simple fix which requires no
infrastructure modifications, at the expense of generating ropey but
correct FP code.  All notions of the x86 FP stack and its insns have
been removed.  Instead, we pretend (to the instruction selector and
register allocator) that x86 has six floating point registers, %fake0
.. %fake5, which can be used in the usual flat manner.  We further
claim that x86 has floating point instructions very similar to SPARC
and Alpha, that is, a simple 3-operand register-register arrangement.
Code generation and register allocation proceed on this basis.

When we come to print out the final assembly, our convenient fiction
is converted to dismal reality.  Each fake instruction is
independently converted to a series of real x86 instructions.
%fake0 .. %fake5 are mapped to %st(0) .. %st(5).  To do reg-reg
arithmetic operations, the two operands are pushed onto the top of the
FP stack, the operation done, and the result copied back into the
relevant register.  There are only six %fake registers because 2 are
needed for the translation, and x86 has 8 in total.

The translation is inefficient but is simple and it works.  A cleverer
translation would handle a sequence of insns, simulating the FP stack
contents, would not impose a fixed mapping from %fake to %st regs, and
hopefully could avoid most of the redundant reg-reg moves of the
current translation.

We might as well make use of whatever unique FP facilities Intel have
chosen to bless us with (let's not be churlish, after all).
Hence GLDZ and GLD1.  Bwahahahahahahaha!


### Note: x86 Floating point precision

Intel's internal floating point registers are by default 80 bit
extended precision.  This means that all operations done on values in
registers are done at 80 bits, and unless the intermediate values are
truncated to the appropriate size (32 or 64 bits) by storing in
memory, calculations in registers will give different results from
calculations which pass intermediate values in memory (eg. via
function calls).

One solution is to set the FPU into 64 bit precision mode.  Some OSs
do this (eg. FreeBSD) and some don't (eg. Linux).  The problem here is
that this will only affect 64-bit precision arithmetic; 32-bit
calculations will still be done at 64-bit precision in registers.  So
it doesn't solve the whole problem.

There's also the issue of what the C library is expecting in terms of
precision.  It seems to be the case that glibc on Linux expects the
FPU to be set to 80 bit precision, so setting it to 64 bit could have
unexpected effects.  Changing the default could have undesirable
effects on other 3rd-party library code too, so the right thing would
be to save/restore the FPU control word across Haskell code if we were
to do this.

gcc's -ffloat-store gives consistent results by always storing the
results of floating-point calculations in memory, which works for both
32 and 64-bit precision.  However, it only affects the values of
user-declared floating point variables in C, not intermediate results.
GHC in -fvia-C mode uses -ffloat-store (see the -fexcess-precision
flag).

Another problem is how to spill floating point registers in the
register allocator.  Should we spill the whole 80 bits, or just 64?
On an OS which is set to 64 bit precision, spilling 64 is fine.  On
Linux, spilling 64 bits will round the results of some operations.
This is what gcc does.  Spilling at 80 bits requires taking up a full
128 bit slot (so we get alignment).  We spill at 80-bits and ignore
the alignment problems.

In the future [edit: now available in GHC 7.0.1, with the -msse2
flag], we'll use the SSE registers for floating point.  This requires
a CPU that supports SSE2 (ordinary SSE only supports 32 bit precision
float ops), which means P4 or Xeon and above.  Using SSE will solve
all these problems, because the SSE registers use fixed 32 bit or 64
bit precision.

--SDM 1/2003


amount

amount

amount

 RcFloat/RcDouble 

 RcFloat/RcDouble 