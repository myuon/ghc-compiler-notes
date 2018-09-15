[[src]](https://github.com/ghc/ghc/tree/master/compiler/cmm/CmmProcPoint.hs)

A proc point is a basic block that, after CPS transformation, will
start a new function.  The entry block of the original function is a
proc point, as is the continuation of each function call.
A third kind of proc point arises if we want to avoid copying code.
Suppose we have code like the following:

  f() {
    if (...) { ..1..; call foo(); ..2..}
    else     { ..3..; call bar(); ..4..}
    x = y + z;
    return x;
  }

The statement 'x = y + z' can be reached from two different proc
points: the continuations of foo() and bar().  We would prefer not to
put a copy in each continuation; instead we would like 'x = y + z' to
be the start of a new procedure to which the continuations can jump:

  f_cps () {
    if (...) { ..1..; push k_foo; jump foo_cps(); }
    else     { ..3..; push k_bar; jump bar_cps(); }
  }
  k_foo() { ..2..; jump k_join(y, z); }
  k_bar() { ..4..; jump k_join(y, z); }
  k_join(y, z) { x = y + z; return x; }

You might think then that a criterion to make a node a proc point is
that it is directly reached by two distinct proc points.  (Note
[Direct reachability].)  But this criterion is a bit too simple; for
example, 'return x' is also reached by two proc points, yet there is
no point in pulling it out of k_join.  A good criterion would be to
say that a node should be made a proc point if it is reached by a set
of proc points that is different than its immediate dominator.  NR
believes this criterion can be shown to produce a minimum set of proc
points, and given a dominator tree, the proc points can be chosen in
time linear in the number of blocks.  Lacking a dominator analysis,
however, we turn instead to an iterative solution, starting with no
proc points and adding them according to these rules:

  1. The entry block is a proc point.
  2. The continuation of a call is a proc point.
  3. A node is a proc point if it is directly reached by more proc
     points than one of its predecessors.

### Note: No simple dataflow



### Note: Proc-point analysis


Given a specified set of proc-points (a set of block-ids), "proc-point
analysis" figures out, for every block, which proc-point it belongs to.
All the blocks belonging to proc-point P will constitute a single
top-level C procedure.

A non-proc-point block B "belongs to" a proc-point P iff B is
reachable from P without going through another proc-point.

Invariant: a block B should belong to at most one proc-point; if it
belongs to two, that's a bug.

### Note: Non-existing proc-points


On some architectures it might happen that the list of proc-points
computed before stack layout pass will be invalidated by the stack
layout. This will happen if stack layout removes from the graph
blocks that were determined to be proc-points. Later on in the pipeline
we use list of proc-points to perform [Proc-point analysis], but
if a proc-point does not exist anymore then we will get compiler panic.
See #8205.


### Note: Direct reachability

Block B is directly reachable from proc point P iff control can flow
from P to B without passing through an intervening proc point.


### Note: No simple dataflow

Sadly, it seems impossible to compute the proc points using a single
dataflow pass.  One might attempt to use this simple lattice:

  data Location = Unknown
                | InProc BlockId -- node is in procedure headed by the named proc point
                | ProcPoint      -- node is itself a proc point

At a join, a node in two different blocks becomes a proc point.
The difficulty is that the change of information during iterative
computation may promote a node prematurely.  Here's a program that
illustrates the difficulty:

  f () {
  entry:
    ....
  L1:
    if (...) { ... }
    else { ... }

  L2: if (...) { g(); goto L1; }
      return x + y;
  }

The only proc-point needed (besides the entry) is L1.  But in an
iterative analysis, consider what happens to L2.  On the first pass
through, it rises from Unknown to 'InProc entry', but when L1 is
promoted to a proc point (because it's the successor of g()), L1's
successors will be promoted to 'InProc L1'.  The problem hits when the
new fact 'InProc L1' flows into L2 which is already bound to 'InProc entry'.
The join operation makes it a proc point when in fact it needn't be,
because its immediate dominator L1 is already a proc point and there
are no other proc points that directly reach L2.


### Note: Separate Adams optimization

It may be worthwhile to attempt the Adams optimization by rewriting
the graph before the assignment of proc-point protocols.  Here are a
couple of rules:

  g() returns to k;                    g() returns to L;
  k: CopyIn c ress; goto L:
   ...                        ==>        ...
  L: // no CopyIn node here            L: CopyIn c ress;


And when c == c' and ress == ress', this also:

  g() returns to k;                    g() returns to L;
  k: CopyIn c ress; goto L:
   ...                        ==>        ...
  L: CopyIn c' ress'                   L: CopyIn c' ress' ;

In both cases the goal is to eliminate k.
