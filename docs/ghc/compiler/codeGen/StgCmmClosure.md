[[src]](https://github.com/ghc/ghc/tree/master/compiler/codeGen/StgCmmClosure.hs)
### Note: GC recovery

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
in TcGenDeriv.) 

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

 OLD: || opt_SMP
         I decided to remove this, because in SMP mode it doesn't matter
         if we enter the same thunk multiple times, so the optimisation
         of jumping directly to the entry code is still valid.  --SDM
        

  staticClosureRequired is never called (hence commented out)

    SimonMar writes (Sept 07) It's an optimisation we used to apply at
    one time, I believe, but it got lost probably in the rewrite of
    the RTS/code generator.  I left that code there to remind me to
    look into whether it was worth doing sometime

{- Avoiding generating entries and info tables
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

At present, for every function we generate all of the following,
just in case.  But they aren't always all needed, as noted below:

[NB1: all of this applies only to *functions*.  Thunks always
have closure, info table, and entry code.]

[NB2: All are needed if the function is *exported*, just to play safe.]

* Fast-entry code  ALWAYS NEEDED

* Slow-entry code
        Needed iff (a) we have any un-saturated calls to the function
        OR         (b) the function is passed as an arg
        OR         (c) we're in the parallel world and the function has free vars
                       [Reason: in parallel world, we always enter functions
                       with free vars via the closure.]

* The function closure
        Needed iff (a) we have any un-saturated calls to the function
        OR         (b) the function is passed as an arg
        OR         (c) if the function has free vars (ie not top level)

  Why case (a) here?  Because if the arg-satis check fails,
  UpdatePAP stuffs a pointer to the function closure in the PAP.
  [Could be changed; UpdatePAP could stuff in a code ptr instead,
   but doesn't seem worth it.]

  [NB: these conditions imply that we might need the closure
  without the slow-entry code.  Here's how.

        f x y = let g w = ...x..y..w...
                in
                ...(g t)...

  Here we need a closure for g which contains x and y,
  but since the calls are all saturated we just jump to the
  fast entry point for g, with R1 pointing to the closure for g.]


* Standard info table
        Needed iff (a) we have any un-saturated calls to the function
        OR         (b) the function is passed as an arg
        OR         (c) the function has free vars (ie not top level)

        NB.  In the sequential world, (c) is only required so that the function closure has
        an info table to point to, to keep the storage manager happy.
        If (c) alone is true we could fake up an info table by choosing
        one of a standard family of info tables, whose entry code just
        bombs out.

        [NB In the parallel world (c) is needed regardless because
        we enter functions with free vars via the closure.]

        If (c) is retained, then we'll sometimes generate an info table
        (for storage mgr purposes) without slow-entry code.  Then we need
        to use an error label in the info table to substitute for the absent
        slow entry code.


 ClosureInfo: information about a binding

   We make a ClosureInfo for each let binding (both top level and not),
   but not bindings for data constructors: for those we build a CmmInfoTable
   directly (see mkDataConInfoTable).

   To a first approximation:
       ClosureInfo = (LambdaFormInfo, CmmInfoTable)

   A ClosureInfo has enough information
     a) to construct the info table itself, and build other things
        related to the binding (e.g. slow entry points for a function)
     b) to allocate a closure containing that info pointer (i.e.
           it knows the info table label)


### Note: Black-holing non-updatable thunks

We must not black-hole non-updatable (single-entry) thunks otherwise
we run into issues like Trac #10414. Specifically:

  * There is no reason to black-hole a non-updatable thunk: it should
    not be competed for by multiple threads

  * It could, conceivably, cause a space leak if we don't black-hole
    it, if there was a live but never-followed pointer pointing to it.
    Let's hope that doesn't happen.

  * It is dangerous to black-hole a non-updatable thunk because
     - is not updated (of course)
     - hence, if it is black-holed and another thread tries to evaluate
       it, that thread will block forever
    This actually happened in Trac #10414.  So we do not black-hole
    non-updatable thunks.

  * How could two threads evaluate the same non-updatable (single-entry)
    thunk?  See Reid Barton's example below.

  * Only eager blackholing could possibly black-hole a non-updatable
    thunk, because lazy black-holing only affects thunks with an
    update frame on the stack.

Here is and example due to Reid Barton (Trac #10414):
    x = \u []  concat [[1], []]
with the following definitions,

    concat x = case x of
        []       -> []
        (:) x xs -> (++) x (concat xs)

    (++) xs ys = case xs of
        []         -> ys
        (:) x rest -> (:) x ((++) rest ys)

Where we use the syntax @\u []@ to denote an updatable thunk and @\s []@ to
denote a single-entry (i.e. non-updatable) thunk. After a thread evaluates @x@
to WHNF and calls @(++)@ the heap will contain the following thunks,

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
