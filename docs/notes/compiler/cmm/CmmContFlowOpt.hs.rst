`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/cmm/CmmContFlowOpt.hs>`_

compiler/cmm/CmmContFlowOpt.hs
==============================


Note [What is shortcutting]
~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/cmm/CmmContFlowOpt.hs#L29>`__

Consider this Cmm code:

L1: ...
    goto L2;
L2: goto L3;
L3: ...

Here L2 is an empty block and contains only an unconditional branch
to L3. In this situation any block that jumps to L2 can jump
directly to L3:

L1: ...
    goto L3;
L2: goto L3;
L3: ...

In this situation we say that we shortcut L2 to L3. One of
consequences of shortcutting is that some blocks of code may become
unreachable (in the example above this is true for L2).



Note [Control-flow optimisations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/cmm/CmmContFlowOpt.hs#L53>`__

This optimisation does three things:

  - If a block finishes in an unconditional branch to another block
    and that is the only jump to that block we concatenate the
    destination block at the end of the current one.

  - If a block finishes in a call whose continuation block is a
    goto, then we can shortcut the destination, making the
    continuation block the destination of the goto - but see Note
    [Shortcut call returns].

  - For any block that is not a call we try to shortcut the
    destination(s). Additionally, if a block ends with a
    conditional branch we try to invert the condition.

Blocks are processed using postorder DFS traversal. A side effect
of determining traversal order with a graph search is elimination
of any blocks that are unreachable.

Transformations are improved by working from the end of the graph
towards the beginning, because we may be able to perform many
shortcuts in one go.



Note [Shortcut call returns]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/cmm/CmmContFlowOpt.hs#L80>`__

We are going to maintain the "current" graph (LabelMap CmmBlock) as
we go, and also a mapping from BlockId to BlockId, representing
continuation labels that we have renamed.  This latter mapping is
important because we might shortcut a CmmCall continuation.  For
example:

   Sp[0] = L
   call g returns to L
   L: goto M
   M: ...

So when we shortcut the L block, we need to replace not only
the continuation of the call, but also references to L in the
code (e.g. the assignment Sp[0] = L):

   Sp[0] = M
   call g returns to M
   M: ...

So we keep track of which labels we have renamed and apply the mapping
at the end with replaceLabels.



Note [Shortcut call returns and proc-points]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/cmm/CmmContFlowOpt.hs#L106>`__

Consider this code that you might get from a recursive
let-no-escape:

      goto L1
     L1:
      if (Hp > HpLim) then L2 else L3
     L2:
      call stg_gc_noregs returns to L4
     L4:
      goto L1
     L3:
      ...
      goto L1

Then the control-flow optimiser shortcuts L4.  But that turns L1
into the call-return proc point, and every iteration of the loop
has to shuffle variables to and from the stack.  So we must *not*
shortcut L4.

Moreover not shortcutting call returns is probably fine.  If L4 can
concat with its branch target then it will still do so.  And we
save some compile time because we don't have to traverse all the
code in replaceLabels.

However, we probably do want to do this if we are splitting proc
points, because L1 will be a proc-point anyway, so merging it with
L4 reduces the number of proc points.  Unfortunately recursive
let-no-escapes won't generate very good code with proc-point
splitting on - we should probably compile them to explicitly use
the native calling convention instead.

