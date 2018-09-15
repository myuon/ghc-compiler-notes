[[src]](https://github.com/ghc/ghc/tree/master/compiler/cmm/CmmLayoutStack.hs)
### Note: Stack Layout

The job of this pass is to

 - replace references to abstract stack Areas with fixed offsets from Sp.

 - replace the CmmHighStackMark constant used in the stack check with
   the maximum stack usage of the proc.

 - save any variables that are live across a call, and reload them as
   necessary.

Before stack allocation, local variables remain live across native
calls (CmmCall{ cmm_cont = Just _ }), and after stack allocation local
variables are clobbered by native calls.

We want to do stack allocation so that as far as possible
 - stack use is minimized, and
 - unnecessary stack saves and loads are avoided.

The algorithm we use is a variant of linear-scan register allocation,
where the stack is our register file.

### Note: Two pass approach

Pass 1:

 - First, we do a liveness analysis, which annotates every block with
   the variables live on entry to the block.

 - We traverse blocks in reverse postorder DFS; that is, we visit at
   least one predecessor of a block before the block itself.  The
   stack layout flowing from the predecessor of the block will
   determine the stack layout on entry to the block.

 - We maintain a data structure

     Map Label StackMap

   which describes the contents of the stack and the stack pointer on
   entry to each block that is a successor of a block that we have
   visited.

 - For each block we visit:

    - Look up the StackMap for this block.

    - If this block is a proc point (or a call continuation, if we aren't
      splitting proc points), we need to reload all the live variables from the
      stack - but this is done in Pass 2, which calculates more precise liveness
      information (see description of Pass 2).

    - Walk forwards through the instructions:
      - At an assignment  x = Sp[loc]
        - Record the fact that Sp[loc] contains x, so that we won't
          need to save x if it ever needs to be spilled.
      - At an assignment  x = E
        - If x was previously on the stack, it isn't any more
      - At the last node, if it is a call or a jump to a proc point
        - Lay out the stack frame for the call (see setupStackFrame)
        - emit instructions to save all the live variables
        - Remember the StackMaps for all the successors
        - emit an instruction to adjust Sp
      - If the last node is a branch, then the current StackMap is the
        StackMap for the successors.

    - Manifest Sp: replace references to stack areas in this block
      with real Sp offsets. We cannot do this until we have laid out
      the stack area for the successors above.

      In this phase we also eliminate redundant stores to the stack;
      see elimStackStores.

  - There is one important gotcha: sometimes we'll encounter a control
    transfer to a block that we've already processed (a join point),
    and in that case we might need to rearrange the stack to match
    what the block is expecting. (exactly the same as in linear-scan
    register allocation, except here we have the luxury of an infinite
    supply of temporary variables).

  - Finally, we update the magic CmmHighStackMark constant with the
    stack usage of the function, and eliminate the whole stack check
    if there was no stack use. (in fact this is done as part of the
    main traversal, by feeding the high-water-mark output back in as
    an input. I hate cyclic programming, but it's just too convenient
    sometimes.)

  There are plenty of tricky details: update frames, proc points, return
  addresses, foreign calls, and some ad-hoc optimisations that are
  convenient to do here and effective in common cases.  Comments in the
  code below explain these.

Pass 2:

- Calculate live registers, but taking into account that nothing is live at the
  entry to a proc point.

- At each proc point and call continuation insert reloads of live registers from
  the stack (they were saved by Pass 1).

### Note: Two pass approach

The main reason for Pass 2 is being able to insert only the reloads that are
needed and the fact that the two passes need different liveness information.
Let's consider an example:

  .....
   \ /
    D   <- proc point
   / \
  E   F
   \ /
    G   <- proc point
    |
    X

Pass 1 needs liveness assuming that local variables are preserved across calls.
This is important because it needs to save any local registers to the stack
(e.g., if register a is used in block X, it must be saved before any native
call).
However, for Pass 2, where we want to reload registers from stack (in a proc
point), this is overly conservative and would lead us to generate reloads in D
for things used in X, even though we're going to generate reloads in G anyway
(since it's also a proc point).
So Pass 2 calculates liveness knowing that nothing is live at the entry to a
proc point. This means that in D we only need to reload things used in E or F.
This can be quite important, for an extreme example see testcase for #3294.

Merging the two passes is not trivial - Pass 2 is a backward rewrite and Pass 1
is a forward one. Furthermore, Pass 1 is creating code that uses local registers
(saving them before a call), which the liveness analysis for Pass 2 must see to
be correct.




 A StackMap describes the stack at any given point.  At a continuation
 it has a particular layout, like this:

         |             | <- base
         |-------------|
         |     ret0    | <- base + 8
         |-------------|
         .  upd frame  . <- base + sm_ret_off
         |-------------|
         |             |
         .    vars     .
         . (live/dead) .
         |             | <- base + sm_sp - sm_args
         |-------------|
         |    ret1     |
         .  ret vals   . <- base + sm_sp    (<--- Sp points here)
         |-------------|

Why do we include the final return address (ret0) in our stack map?  I
have absolutely no idea, but it seems to be done that way consistently
in the rest of the code generator, so I played along here. --SDM

Note that we will be constructing an info table for the continuation
(ret1), which needs to describe the stack down to, but not including,
the update frame (or ret0, if there is no update frame).


### Note: SP old/young offsets

Sp(L) is the Sp offset on entry to block L relative to the base of the
OLD area.

SpArgs(L) is the size of the young area for L, i.e. the number of
arguments.

 - in block L, each reference to [old + N] turns into
   [Sp + Sp(L) - N]

 - in block L, each reference to [young(L') + N] turns into
   [Sp + Sp(L) - Sp(L') + SpArgs(L') - N]

 - be careful with the last node of each block: Sp has already been adjusted
   to be Sp + Sp(L) - Sp(L')


### Note: Lower safe foreign calls

We start with

   Sp[young(L1)] = L1
 ,-----------------------
 | r1 = foo(x,y,z) returns to L1
 '-----------------------
 L1:
   R1 = r1 -- copyIn, inserted by mkSafeCall
   ...

the stack layout algorithm will arrange to save and reload everything
live across the call.  Our job now is to expand the call so we get

   Sp[young(L1)] = L1
 ,-----------------------
 | SAVE_THREAD_STATE()
 | token = suspendThread(BaseReg, interruptible)
 | r = foo(x,y,z)
 | BaseReg = resumeThread(token)
 | LOAD_THREAD_STATE()
 | R1 = r  -- copyOut
 | jump Sp[0]
 '-----------------------
 L1:
   r = R1 -- copyIn, inserted by mkSafeCall
   ...

### Note: safe foreign call convention