`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/codeGen/StgCmmClosure.hs>`_

Note [Data constructor dynamic tags]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The family size of a data type (the number of constructors
or the arity of a function) can be either:
   * small, if the family size < 2**tag_bits
   * big, otherwise.

Small families can have the constructor tag in the tag bits.
Big families only use the tag value 1 to represent evaluatedness.
We don't have very many tag bits: for example, we have 2 bits on
x86-32 and 3 bits on x86-64.


Note [GC recovery]
~~~~~~~~~~~~~~~~~~~~~
If we a have a local let-binding (function or thunk)
   let f = <body> in ...
AND <body> allocates, then the heap-overflow check needs to know how
to re-start the evaluation.  It uses the "self" pointer to do this.
So even if there are no free variables in <body>, we still make
nodeMustPointToIt be True for non-top-level bindings.

Why do any such bindings exist?  After all, let-floating should have
floated them out.  Well, a clever optimiser might leave one there to
avoid a space leak, deliberately recomputing a thunk.  Also (and this
really does happen occasionally) let-floating may make a function f smaller
so it can be inlined, so now (f True) may generate a local no-fv closure.
This actually happened during bootstrapping GHC itself, with f=mkRdrFunBind
in TcGenDeriv.) ---------------------------------------------------------------------------
                getCallMethod
---------------------------------------------------------------------------
The entry conventions depend on the type of closure being entered,
whether or not it has free variables, and whether we're running
sequentially or in parallel.

Closure                           Node   Argument   Enter
Characteristics              Par   Req'd  Passing    Via
---------------------------------------------------------------------------
Unknown                     & no  & yes & stack     & node
Known fun (>1 arg), no fvs  & no  & no  & registers & fast entry (enough args)
                                                    & slow entry (otherwise)
Known fun (>1 arg), fvs     & no  & yes & registers & fast entry (enough args)
0 arg, no fvs \r,\s         & no  & no  & n/a       & direct entry
0 arg, no fvs \u            & no  & yes & n/a       & node
0 arg, fvs \r,\s,selector   & no  & yes & n/a       & node
0 arg, fvs \r,\s            & no  & yes & n/a       & direct entry
0 arg, fvs \u               & no  & yes & n/a       & node
Unknown                     & yes & yes & stack     & node
Known fun (>1 arg), no fvs  & yes & no  & registers & fast entry (enough args)
                                                    & slow entry (otherwise)
Known fun (>1 arg), fvs     & yes & yes & registers & node
0 arg, fvs \r,\s,selector   & yes & yes & n/a       & node
0 arg, no fvs \r,\s         & yes & no  & n/a       & direct entry
0 arg, no fvs \u            & yes & yes & n/a       & node
0 arg, fvs \r,\s            & yes & yes & n/a       & node
0 arg, fvs \u               & yes & yes & n/a       & node

When black-holing, single-entry closures could also be entered via node
(rather than directly) to catch double-entry. 


Note [Black-holing non-updatable thunks]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We must not black-hole non-updatable (single-entry) thunks otherwise
we run into issues like #10414. Specifically:

  * There is no reason to black-hole a non-updatable thunk: it should
    not be competed for by multiple threads

  * It could, conceivably, cause a space leak if we don't black-hole
    it, if there was a live but never-followed pointer pointing to it.
    Let's hope that doesn't happen.

  * It is dangerous to black-hole a non-updatable thunk because
     - is not updated (of course)
     - hence, if it is black-holed and another thread tries to evaluate
       it, that thread will block forever
    This actually happened in #10414.  So we do not black-hole
    non-updatable thunks.

  * How could two threads evaluate the same non-updatable (single-entry)
    thunk?  See Reid Barton's example below.

  * Only eager blackholing could possibly black-hole a non-updatable
    thunk, because lazy black-holing only affects thunks with an
    update frame on the stack.

Here is and example due to Reid Barton (#10414):
    x = \u []  concat [[1], []]
with the following definitions,

.. code-block:: haskell

    concat x = case x of
        []       -> []
        (:) x xs -> (++) x (concat xs)

.. code-block:: haskell

    (++) xs ys = case xs of
        []         -> ys
        (:) x rest -> (:) x ((++) rest ys)

Where we use the syntax @\u []@ to denote an updatable thunk and @\s []@ to
denote a single-entry (i.e. non-updatable) thunk. After a thread evaluates @x@
to WHNF and calls @(++)@ the heap will contain the following thunks,

.. code-block:: haskell

    x = 1 : y
    y = \u []  (++) [] z
    z = \s []  concat []

Now that the stage is set, consider the follow evaluations by two racing threads
A and B,

  1. Both threads enter @y@ before either is able to replace it with an
     indirection

  2. Thread A does the case analysis in @(++)@ and consequently enters @z@,
     replacing it with a black-hole

  3. At some later point thread B does the same case analysis and also attempts
     to enter @z@. However, it finds that it has been replaced with a black-hole
     so it blocks.

  4. Thread A eventually finishes evaluating @z@ (to @[]@) and updates @y@
     accordingly. It does *not* update @z@, however, as it is single-entry. This
     leaves Thread B blocked forever on a black-hole which will never be
     updated.

To avoid this sort of condition we never black-hole non-updatable thunks.

