`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/cmm/CmmNode.hs>`_

compiler/cmm/CmmNode.hs
=======================


Note [Foreign calls]
~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/cmm/CmmNode.hs#L166>`__

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



Note [Unsafe foreign calls clobber caller-save registers]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/cmm/CmmNode.hs#L197>`__

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



Note [Register parameter passing]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/cmm/CmmNode.hs#L216>`__

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



Note [Safe foreign calls clobber STG registers]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/cmm/CmmNode.hs#L382>`__

During stack layout phase every safe foreign call is expanded into a block
that contains unsafe foreign call (instead of safe foreign call) and ends
with a normal call (See Note [Foreign calls]). This means that we must
treat safe foreign call as if it was a normal call (because eventually it
will be). This is important if we try to run sinking pass before stack
layout phase. Consider this example of what might go wrong (this is cmm
code from stablename001 test). Here is code after common block elimination
(before stack layout):

 c1q6:
     _s1pf::P64 = R1;
     _c1q8::I64 = performMajorGC;
     I64[(young<c1q9> + 8)] = c1q9;
     foreign call "ccall" arg hints:  []  result hints:  [] (_c1q8::I64)(...)
                  returns to c1q9 args: ([]) ress: ([])ret_args: 8ret_off: 8;
 c1q9:
     I64[(young<c1qb> + 8)] = c1qb;
     R1 = _s1pc::P64;
     call stg_makeStableName#(R1) returns to c1qb, args: 8, res: 8, upd: 8;

If we run sinking pass now (still before stack layout) we will get this:

 c1q6:
     I64[(young<c1q9> + 8)] = c1q9;
     foreign call "ccall" arg hints:  []  result hints:  [] performMajorGC(...)
                  returns to c1q9 args: ([]) ress: ([])ret_args: 8ret_off: 8;
 c1q9:
     I64[(young<c1qb> + 8)] = c1qb;
     _s1pf::P64 = R1;         <------ _s1pf sunk past safe foreign call
     R1 = _s1pc::P64;
     call stg_makeStableName#(R1) returns to c1qb, args: 8, res: 8, upd: 8;

Notice that _s1pf was sunk past a foreign call. When we run stack layout
safe call to performMajorGC will be turned into:

 c1q6:
     _s1pc::P64 = P64[Sp + 8];
     I64[Sp - 8] = c1q9;
     Sp = Sp - 8;
     I64[I64[CurrentTSO + 24] + 16] = Sp;
     P64[CurrentNursery + 8] = Hp + 8;
     (_u1qI::I64) = call "ccall" arg hints:  [PtrHint,]
                          result hints:  [PtrHint] suspendThread(BaseReg, 0);
     call "ccall" arg hints:  []  result hints:  [] performMajorGC();
     (_u1qJ::I64) = call "ccall" arg hints:  [PtrHint]
                          result hints:  [PtrHint] resumeThread(_u1qI::I64);
     BaseReg = _u1qJ::I64;
     _u1qK::P64 = CurrentTSO;
     _u1qL::P64 = I64[_u1qK::P64 + 24];
     Sp = I64[_u1qL::P64 + 16];
     SpLim = _u1qL::P64 + 192;
     HpAlloc = 0;
     Hp = I64[CurrentNursery + 8] - 8;
     HpLim = I64[CurrentNursery] + (%MO_SS_Conv_W32_W64(I32[CurrentNursery + 48]) * 4096 - 1);
     call (I64[Sp])() returns to c1q9, args: 8, res: 8, upd: 8;
 c1q9:
     I64[(young<c1qb> + 8)] = c1qb;
     _s1pf::P64 = R1;         <------ INCORRECT!
     R1 = _s1pc::P64;
     call stg_makeStableName#(R1) returns to c1qb, args: 8, res: 8, upd: 8;

Notice that c1q6 now ends with a call. Sinking _s1pf::P64 = R1 past that
call is clearly incorrect. This is what would happen if we assumed that
safe foreign call has the same semantics as unsafe foreign call. To prevent
this we need to treat safe foreign call as if was normal call.

