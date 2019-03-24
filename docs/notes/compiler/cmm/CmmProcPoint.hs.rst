`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/cmm/CmmProcPoint.hs>`_

Note [Proc-point analysis]
~~~~~~~~~~~~~~~~~~~~~~~~~~

Given a specified set of proc-points (a set of block-ids), "proc-point
analysis" figures out, for every block, which proc-point it belongs to.
All the blocks belonging to proc-point P will constitute a single
top-level C procedure.

A non-proc-point block B "belongs to" a proc-point P iff B is
reachable from P without going through another proc-point.

Invariant: a block B should belong to at most one proc-point; if it
belongs to two, that's a bug.



Note [Non-existing proc-points]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

On some architectures it might happen that the list of proc-points
computed before stack layout pass will be invalidated by the stack
layout. This will happen if stack layout removes from the graph
blocks that were determined to be proc-points. Later on in the pipeline
we use list of proc-points to perform [Proc-point analysis], but
if a proc-point does not exist anymore then we will get compiler panic.
See #8205.


Note [Separate Adams optimization]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It may be worthwhile to attempt the Adams optimization by rewriting
the graph before the assignment of proc-point protocols.  Here are a
couple of rules:

.. code-block:: haskell

  g() returns to k;                    g() returns to L;
  k: CopyIn c ress; goto L:
   ...                        ==>        ...
  L: // no CopyIn node here            L: CopyIn c ress;


And when c == c' and ress == ress', this also:

.. code-block:: haskell

  g() returns to k;                    g() returns to L;
  k: CopyIn c ress; goto L:
   ...                        ==>        ...
  L: CopyIn c' ress'                   L: CopyIn c' ress' ;

In both cases the goal is to eliminate k.

