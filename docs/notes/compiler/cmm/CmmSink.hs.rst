Note [dependent assignments]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If our assignment list looks like

   [ y = e,  x = ... y ... ]

We cannot inline x.  Remember this list is really in reverse order,
so it means  x = ... y ...; y = e

Hence if we inline x, the outer assignment to y will capture the
reference in x's right hand side.

In this case we should rename the y in x's right-hand side,
i.e. change the list to [ y = e, x = ... y1 ..., y1 = y ]
Now we can go ahead and inline x.

For now we do nothing, because this would require putting
everything inside UniqSM.

One more variant of this (#7366):

  [ y = e, y = z ]

If we don't want to inline y = e, because y is used many times, we
might still be tempted to inline y = z (because we always inline
trivial rhs's).  But of course we can't, because y is equal to e,
not z.


Note [discard during inlining]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Opportunities to discard assignments sometimes appear after we've
done some inlining.  Here's an example:

     x = R1;
     y = P64[x + 7];
     z = P64[x + 15];
     /* z is dead */
     R1 = y & (-8);

The x assignment is trivial, so we inline it in the RHS of y, and
keep both x and y.  z gets dropped because it is dead, then we
inline y, and we have a dead assignment to x.  If we don't notice
that x is dead in tryToInline, we end up retaining it.


Note [Sinking and calls]
~~~~~~~~~~~~~~~~~~~~~~~~

We have three kinds of calls: normal (CmmCall), safe foreign (CmmForeignCall)
and unsafe foreign (CmmUnsafeForeignCall). We perform sinking pass after
stack layout (see Note [Sinking after stack layout]) which leads to two
invariants related to calls:

  a) during stack layout phase all safe foreign calls are turned into
     unsafe foreign calls (see Note [Lower safe foreign calls]). This
     means that we will never encounter CmmForeignCall node when running
     sinking after stack layout

  b) stack layout saves all variables live across a call on the stack
     just before making a call (remember we are not sinking assignments to
     stack):

      L1:
         x = R1
         P64[Sp - 16] = L2
         P64[Sp - 8]  = x
         Sp = Sp - 16
         call f() returns L2
      L2:

     We will attempt to sink { x = R1 } but we will detect conflict with
     { P64[Sp - 8]  = x } and hence we will drop { x = R1 } without even
     checking whether it conflicts with { call f() }. In this way we will
     never need to check any assignment conflicts with CmmCall. Remember
     that we still need to check for potential memory conflicts.

So the result is that we only need to worry about CmmUnsafeForeignCall nodes
when checking conflicts (see Note [Unsafe foreign calls clobber caller-save registers]).
This assumption holds only when we do sinking after stack layout. If we run
it before stack layout we need to check for possible conflicts with all three
kinds of calls. Our `conflicts` function does that by using a generic
foldRegsDefd and foldRegsUsed functions defined in DefinerOfRegs and
UserOfRegs typeclasses.

An abstraction of memory read or written.


Note [Foreign calls clobber heap]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

It is tempting to say that foreign calls clobber only
non-heap/stack memory, but unfortunately we break this invariant in
the RTS.  For example, in stg_catch_retry_frame we call
stmCommitNestedTransaction() which modifies the contents of the
TRec it is passed (this actually caused incorrect code to be
generated).

Since the invariant is true for the majority of foreign calls,
perhaps we ought to have a special annotation for calls that can
modify heap/stack memory.  For now we just use the conservative
definition here.

Some CallishMachOp imply a memory barrier e.g. AtomicRMW and
therefore we should never float any memory operations across one of
these calls.
