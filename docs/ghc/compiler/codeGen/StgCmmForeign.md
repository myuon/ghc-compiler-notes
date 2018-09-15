[[src]](https://github.com/ghc/ghc/tree/master/compiler/codeGen/StgCmmForeign.hs)
### Note: safe foreign call convention

The simple thing to do for a safe foreign call would be the same as an
unsafe one: just

    emitForeignCall ...
    emitReturn ...

but consider what happens in this case

The sequel is AssignTo [r].  The call to newUnboxedTupleRegs picks [r]
as the result reg, and we generate

  r = foo(x,y,z) returns to L1  -- emitForeignCall
 L1:
  r = r  -- emitReturn
  goto L2
L2:
  ...

Now L1 is a proc point (by definition, it is the continuation of the
safe foreign call).  If L2 does a heap check, then L2 will also be a
proc point.

Furthermore, the stack layout algorithm has to arrange to save r
somewhere between the call and the jump to L1, which is annoying: we
would have to treat r differently from the other live variables, which
have to be saved *before* the call.

So we adopt a special convention for safe foreign calls: the results
are copied out according to the NativeReturn convention by the call,
and the continuation of the call should copyIn the results.  (The
copyOut code is actually inserted when the safe foreign call is
lowered later).  The result regs attached to the safe foreign call are
only used temporarily to hold the results before they are copied out.

We will now generate this:

  r = foo(x,y,z) returns to L1
 L1:
  r = R1  -- copyIn, inserted by mkSafeCall
  goto L2
 L2:
  ... r ...

And when the safe foreign call is lowered later (see Note [lower safe
foreign calls]) we get this:

  suspendThread()
  r = foo(x,y,z)
  resumeThread()
  R1 = r  -- copyOut, inserted by lowerSafeForeignCall
  jump L1
 L1:
  r = R1  -- copyIn, inserted by mkSafeCall
  goto L2
 L2:
  ... r ...

Now consider what happens if L2 does a heap check: the Adams
optimisation kicks in and commons up L1 with the heap-check
continuation, resulting in just one proc point instead of two. Yay!


 |
@closeNursery dflags tso@ produces code to close the nursery.
A local register holding the value of @CurrentTSO@ is expected for
efficiency.

Closing the nursery corresponds to the following code:

@
  tso = CurrentTSO;
  cn = CurrentNuresry;

  // Update the allocation limit for the current thread.  We don't
  // check to see whether it has overflowed at this point, that check is
  // made when we run out of space in the current heap block (stg_gc_noregs)
  // and in the scheduler when context switching (schedulePostRunThread).
  tso->alloc_limit -= Hp + WDS(1) - cn->start;

  // Set cn->free to the next unoccupied word in the block
  cn->free = Hp + WDS(1);
@


 |
@openNursery dflags tso@ produces code to open the nursery. A local register
holding the value of @CurrentTSO@ is expected for efficiency.

Opening the nursery corresponds to the following code:

@
   tso = CurrentTSO;
   cn = CurrentNursery;
   bdfree = CurrentNuresry->free;
   bdstart = CurrentNuresry->start;

   // We *add* the currently occupied portion of the nursery block to
   // the allocation limit, because we will subtract it again in
   // closeNursery.
   tso->alloc_limit += bdfree - bdstart;

   // Set Hp to the last occupied word of the heap block.  Why not the
   // next unocupied word?  Doing it this way means that we get to use
   // an offset of zero more often, which might lead to slightly smaller
   // code on some architectures.
   Hp = bdfree - WDS(1);

   // Set HpLim to the end of the current nursery block (note that this block
   // might be a block group, consisting of several adjacent blocks.
   HpLim = bdstart + CurrentNursery->blocks*BLOCK_SIZE_W - 1;
@
