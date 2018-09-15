[[src]](https://github.com/ghc/ghc/tree/master/compiler/cmm/CmmNode.hs)
### Note: Foreign calls

A CmmUnsafeForeignCall is used for *unsafe* foreign calls;
a CmmForeignCall call is used for *safe* foreign calls.

Unsafe ones are mostly easy: think of them as a "fat machine
instruction".  In particular, they do *not* kill all live registers,
just the registers they return to (there was a bit of code in GHC that
conservatively assumed otherwise.)  However, see [Register parameter passing].

Safe ones are trickier.  A safe foreign call
     r = f(x)
ultimately expands to
     push "return address"      -- Never used to return to;
                                -- just points an info table
     save registers into TSO
     call suspendThread
     r = f(x)                   -- Make the call
     call resumeThread
     restore registers
     pop "return address"
We cannot "lower" a safe foreign call to this sequence of Cmms, because
after we've saved Sp all the Cmm optimiser's assumptions are broken.

Note that a safe foreign call needs an info table.

So Safe Foreign Calls must remain as last nodes until the stack is
made manifest in CmmLayoutStack, where they are lowered into the above
sequence.


### Note: Unsafe foreign calls clobber caller-save registers


A foreign call is defined to clobber any GlobalRegs that are mapped to
caller-saves machine registers (according to the prevailing C ABI).
StgCmmUtils.callerSaves tells you which GlobalRegs are caller-saves.

This is a design choice that makes it easier to generate code later.
We could instead choose to say that foreign calls do *not* clobber
caller-saves regs, but then we would have to figure out which regs
were live across the call later and insert some saves/restores.

Furthermore when we generate code we never have any GlobalRegs live
across a call, because they are always copied-in to LocalRegs and
copied-out again before making a call/jump.  So all we have to do is
avoid any code motion that would make a caller-saves GlobalReg live
across a foreign call during subsequent optimisations.


### Note: Register parameter passing

On certain architectures, some registers are utilized for parameter
passing in the C calling convention.  For example, in x86-64 Linux
convention, rdi, rsi, rdx and rcx (as well as r8 and r9) may be used for
argument passing.  These are registers R3-R6, which our generated
code may also be using; as a result, it's necessary to save these
values before doing a foreign call.  This is done during initial
code generation in callerSaveVolatileRegs in StgCmmUtils.hs.  However,
one result of doing this is that the contents of these registers
may mysteriously change if referenced inside the arguments.  This
is dangerous, so you'll need to disable inlining much in the same
way is done in cmm/CmmOpt.hs currently.  We should fix this!
