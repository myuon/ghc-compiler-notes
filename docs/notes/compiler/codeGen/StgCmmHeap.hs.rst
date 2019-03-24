`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/codeGen/StgCmmHeap.hs>`_

====================
compiler/codeGen/StgCmmHeap.hs.rst
====================

Note [Single stack check]
~~~~~~~~~~~~~~~~~~~~~~~~~
When compiling a function we can determine how much stack space it
will use. We therefore need to perform only a single stack check at
the beginning of a function to see if we have enough stack space.

The check boils down to comparing Sp-N with SpLim, where N is the
amount of stack space needed (see Note [Stack usage] below).  *BUT*
at this stage of the pipeline we are not supposed to refer to Sp
itself, because the stack is not yet manifest, so we don't quite
know where Sp pointing.
So instead of referring directly to Sp - as we used to do in the
past - the code generator uses (old + 0) in the stack check. That
is the address of the first word of the old area, so if we add N
we'll get the address of highest used word.

This makes the check robust.  For example, while we need to perform
only one stack check for each function, we could in theory place
more stack checks later in the function. They would be redundant,
but not incorrect (in a sense that they should not change program
behaviour). We need to make sure however that a stack check
inserted after incrementing the stack pointer checks for a
respectively smaller stack space. This would not be the case if the
code generator produced direct references to Sp. By referencing
(old + 0) we make sure that we always check for a correct amount of
stack: when converting (old + 0) to Sp the stack layout phase takes
into account changes already made to stack pointer. The idea for
this change came from observations made while debugging #8275.


Note [Stack usage]
~~~~~~~~~~~~~~~~~~
At the moment we convert from STG to Cmm we don't know N, the
number of bytes of stack that the function will use, so we use a
special late-bound CmmLit, namely
      CmmHighStackMark
to stand for the number of bytes needed. When the stack is made
manifest, the number of bytes needed is calculated, and used to
replace occurrences of CmmHighStackMark

The (Maybe CmmExpr) passed to do_checks is usually
    Just (CmmLit CmmHighStackMark)
but can also (in certain hand-written RTS functions)
    Just (CmmLit 8)  or some other fixed valuet
If it is Nothing, we don't generate a stack check at all.


Note [Self-recursive loop header]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Self-recursive loop header is required by loopification optimization (See
Note [Self-recursive tail calls] in StgCmmExpr). We emit it if:

 1. There is information about self-loop in the FCode environment. We don't
    check the binder (first component of the self_loop_info) because we are
    certain that if the self-loop info is present then we are compiling the
    binder body. Reason: the only possible way to get here with the
    self_loop_info present is from closureCodeBody.

 2. checkYield && isJust mb_stk_hwm. checkYield tells us that it is possible
    to preempt the heap check (see #367 for motivation behind this check). It
    is True for heap checks placed at the entry to a function and
    let-no-escape heap checks but false for other heap checks (eg. in case
    alternatives or created from hand-written high-level Cmm). The second
    check (isJust mb_stk_hwm) is true for heap checks at the entry to a
    function and some heap checks created in hand-written Cmm. Otherwise it
    is Nothing. In other words the only situation when both conditions are
    true is when compiling stack and heap checks at the entry to a
    function. This is the only situation when we want to emit a self-loop
    label.

