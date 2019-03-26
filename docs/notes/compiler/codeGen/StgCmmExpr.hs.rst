`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/codeGen/StgCmmExpr.hs>`_

compiler/codeGen/StgCmmExpr.hs
==============================


Note [Compiling case expressions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/codeGen/StgCmmExpr.hs#L190>`__

It is quite interesting to decide whether to put a heap-check at the
start of each alternative.  Of course we certainly have to do so if
the case forces an evaluation, or if there is a primitive op which can
trigger GC.

A more interesting situation is this (a Plan-B situation)

::

        !P!;
        ...P...
        case x# of
          0#      -> !Q!; ...Q...
          default -> !R!; ...R...

where !x! indicates a possible heap-check point. The heap checks
in the alternatives *can* be omitted, in which case the topmost
heapcheck will take their worst case into account.

In favour of omitting !Q!, !R!:

 - *May* save a heap overflow test,
   if ...P... allocates anything.

 - We can use relative addressing from a single Hp to
   get at all the closures so allocated.

 - No need to save volatile vars etc across heap checks
   in !Q!, !R!

Against omitting !Q!, !R!

  - May put a heap-check into the inner loop.  Suppose
        the main loop is P -> R -> P -> R...
        Q is the loop exit, and only it does allocation.
    This only hurts us if P does no allocation.  If P allocates,
    then there is a heap check in the inner loop anyway.

  - May do more allocation than reqd.  This sometimes bites us
    badly.  For example, nfib (ha!) allocates about 30\% more space if the
    worst-casing is done, because many many calls to nfib are leaf calls
    which don't need to allocate anything.

::

    We can un-allocate, but that costs an instruction

Neither problem hurts us if there is only one alternative.

Suppose the inner loop is P->R->P->R etc.  Then here is
how many heap checks we get in the *inner loop* under various
conditions

  Alloc   Heap check in branches (!Q!, !R!)?
  P Q R      yes     no (absorb to !P!)
--------------------------------------
  n n n      0          0
  n y n      0          1
  n . y      1          1
  y . y      2          1
  y . n      1          1

Best choices: absorb heap checks from Q and R into !P! iff
  a) P itself does some allocation
or
  b) P does allocation, or there is exactly one alternative

We adopt (b) because that is more likely to put the heap check at the
entry to a function, when not many things are live.  After a bunch of
single-branch cases, we may have lots of things live

Hence: two basic plans for

::

        case e of r { alts }

------ Plan A: the general case ---------

::

        ...save current cost centre...

::

        ...code for e,
           with sequel (SetLocals r)

::

        ...restore current cost centre...
        ...code for alts...
        ...alts do their own heap checks

------ Plan B: special case when ---------
  (i)  e does not allocate or call GC
  (ii) either upstream code performs allocation
       or there is just one alternative

::

  Then heap allocation in the (single) case branch
  is absorbed by the upstream check.
  Very common example: primops on unboxed values

::

        ...code for e,
           with sequel (SetLocals r)...

        ...code for alts...
        ...no heap check...
-----------------------------------



Note [case on bool]
~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/codeGen/StgCmmExpr.hs#L332>`__

This special case handles code like

::

  case a <# b of
    True ->
    False ->

-->  case tagToEnum# (a <$# b) of
        True -> .. ; False -> ...

--> case (a <$# b) of r ->
    case tagToEnum# r of
        True -> .. ; False -> ...

If we let the ordinary case code handle it, we'll get something like

::

 tmp1 = a < b
 tmp2 = Bool_closure_tbl[tmp1]
 if (tmp2 & 7 != 0) then ... // normal tagged case

but this junk won't optimise away.  What we really want is just an
inline comparison:

::

 if (a < b) then ...

So we add a special case to generate

::

 tmp1 = a < b
 if (tmp1 == 0) then ...

and later optimisations will further improve this.

Now that #6135 has been resolved it should be possible to remove that
special case. The idea behind this special case and pre-6135 implementation
of Bool-returning primops was that tagToEnum# was added implicitly in the
codegen and then optimized away. Now the call to tagToEnum# is explicit
in the source code, which allows to optimize it away at the earlier stages
of compilation (i.e. at the Core level).



Note [Scrutinising VoidRep]
~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/codeGen/StgCmmExpr.hs#L372>`__

Suppose we have this STG code:
   f = \[s : State# RealWorld] ->
       case s of _ -> blah
This is very odd.  Why are we scrutinising a state token?  But it
can arise with bizarre NOINLINE pragmas (#9964)
    crash :: IO ()
    crash = IO (\s -> let {-# NOINLINE s' #-}
                          s' = s
                      in (# s', () #))

Now the trouble is that 's' has VoidRep, and we do not bind void
arguments in the environment; they don't live anywhere.  See the
calls to nonVoidIds in various places.  So we must not look up
's' in the environment.  Instead, just evaluate the RHS!  Simple.



Note [Dead-binder optimisation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/codeGen/StgCmmExpr.hs#L389>`__

A case-binder, or data-constructor argument, may be marked as dead,
because we preserve occurrence-info on binders in CoreTidy (see
CoreTidy.tidyIdBndr).

If the binder is dead, we can sometimes eliminate a load.  While
CmmSink will eliminate that load, it's very easy to kill it at source
(giving CmmSink less work to do), and in any case CmmSink only runs
with -O. Since the majority of case binders are dead, this
optimisation probably still has a great benefit-cost ratio and we want
to keep it for -O0. See also Phab:D5358.

This probably also was the reason for occurrence hack in Phab:D5339 to
exist, perhaps because the occurrence information preserved by
'CoreTidy.tidyIdBndr' was insufficient.  But now that CmmSink does the
job we deleted the hacks.



Note [Dodgy unsafeCoerce 1]
~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/codeGen/StgCmmExpr.hs#L413>`__

Consider
    case (x :: HValue) |> co of (y :: MutVar# Int)
        DEFAULT -> ...
We want to gnerate an assignment
     y := x
We want to allow this assignment to be generated in the case when the
types are compatible, because this allows some slightly-dodgy but
occasionally-useful casts to be used, such as in RtClosureInspect
where we cast an HValue to a MutVar# so we can print out the contents
of the MutVar#.  If instead we generate code that enters the HValue,
then we'll get a runtime panic, because the HValue really is a
MutVar#.  The types are compatible though, so we can just generate an
assignment.



Note [Dodgy unsafeCoerce 2, #3132]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/codeGen/StgCmmExpr.hs#L454>`__

In all other cases of a lifted Id being cast to an unlifted type, the
Id should be bound to bottom, otherwise this is an unsafe use of
unsafeCoerce.  We can generate code to enter the Id and assume that
it will never return.  Hence, we emit the usual enter/return code, and
because bottom must be untagged, it will be entered.  The Sequel is a
type-correct assignment, albeit bogus.  The (dead) continuation loops;
it would be better to invoke some kind of panic function here.



Note [Handle seq#]
~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/codeGen/StgCmmExpr.hs#L477>`__

See Note [seq# magic] in PrelRules.
The special case for seq# in cgCase does this:

  case seq# a s of v
    (# s', a' #) -> e
==>
  case a of v
    (# s', a' #) -> e

(taking advantage of the fact that the return convention for (# State#, a #)
is the same as the return convention for just 'a')



Note [GC for conditionals]
~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/codeGen/StgCmmExpr.hs#L525>`__

For boolean conditionals it seems that we have always done NoGcInAlts.
That is, we have always done the GC check before the conditional.
This is enshrined in the special case for
   case tagToEnum# (a>b) of ...
See Note [case on bool]

It's odd, and it's flagrantly inconsistent with the rules described
Note [Compiling case expressions].  However, after eliminating the
tagToEnum# (#13397) we will have:
   case (a>b) of ...
Rather than make it behave quite differently, I am testing for a
comparison operator here in in the general case as well.

ToDo: figure out what the Right Rule should be.



Note [scrut sequel]
~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/codeGen/StgCmmExpr.hs#L542>`__

The job of the scrutinee is to assign its value(s) to alt_regs.
Additionally, if we plan to do a heap-check in the alternatives (see
Note [Compiling case expressions]), then we *must* retreat Hp to
recover any unused heap before passing control to the sequel.  If we
don't do this, then any unused heap will become slop because the heap
check will reset the heap usage. Slop in the heap breaks LDV profiling
(+RTS -hb) which needs to do a linear sweep through the nursery.



Note [Inlining out-of-line primops and heap checks]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/codeGen/StgCmmExpr.hs#L553>`__

If shouldInlinePrimOp returns True when called from StgCmmExpr for the
purpose of heap check placement, we *must* inline the primop later in
StgCmmPrim. If we don't things will go wrong.
---------------



Note [Self-recursive tail calls]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/codeGen/StgCmmExpr.hs#L798>`__

Self-recursive tail calls can be optimized into a local jump in the same
way as let-no-escape bindings (see Note [What is a non-escaping let] in
stgSyn/CoreToStg.hs). Consider this:

foo.info:
    a = R1  // calling convention
    b = R2
    goto L1
L1: ...
    ...
...
L2: R1 = x
    R2 = y
    call foo(R1,R2)

Instead of putting x and y into registers (or other locations required by the
calling convention) and performing a call we can put them into local
variables a and b and perform jump to L1:

foo.info:
    a = R1
    b = R2
    goto L1
L1: ...
    ...
...
L2: a = x
    b = y
    goto L1

This can be done only when function is calling itself in a tail position
and only if the call passes number of parameters equal to function's arity.
Note that this cannot be performed if a function calls itself with a
continuation.

This in fact implements optimization known as "loopification". It was
described in "Low-level code optimizations in the Glasgow Haskell Compiler"
by Krzysztof Woś, though we use different approach. Krzysztof performed his
optimization at the Cmm level, whereas we perform ours during code generation
(Stg-to-Cmm pass) essentially making sure that optimized Cmm code is
generated in the first place.

Implementation is spread across a couple of places in the code:

  * FCode monad stores additional information in its reader environment
    (cgd_self_loop field). This information tells us which function can
    tail call itself in an optimized way (it is the function currently
    being compiled), what is the label of a loop header (L1 in example above)
    and information about local registers in which we should arguments
    before making a call (this would be a and b in example above).

  * Whenever we are compiling a function, we set that information to reflect
    the fact that function currently being compiled can be jumped to, instead
    of called. This is done in closureCodyBody in StgCmmBind.

  * We also have to emit a label to which we will be jumping. We make sure
    that the label is placed after a stack check but before the heap
    check. The reason is that making a recursive tail-call does not increase
    the stack so we only need to check once. But it may grow the heap, so we
    have to repeat the heap check in every self-call. This is done in
    do_checks in StgCmmHeap.

  * When we begin compilation of another closure we remove the additional
    information from the environment. This is done by forkClosureBody
    in StgCmmMonad. Other functions that duplicate the environment -
    forkLneBody, forkAlts, codeOnly - duplicate that information. In other
    words, we only need to clean the environment of the self-loop information
    when compiling right hand side of a closure (binding).

  * When compiling a call (cgIdApp) we use getCallMethod to decide what kind
    of call will be generated. getCallMethod decides to generate a self
    recursive tail call when (a) environment stores information about
    possible self tail-call; (b) that tail call is to a function currently
    being compiled; (c) number of passed non-void arguments is equal to
    function's arity. (d) loopification is turned on via -floopification
    command-line option.

  * Command line option to turn loopification on and off is implemented in
    DynFlags.



Note [Void arguments in self-recursive tail calls]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/codeGen/StgCmmExpr.hs#L882>`__

State# tokens can get in the way of the loopification optimization as seen in
#11372. Consider this:

foo :: [a]
    -> (a -> State# s -> (# State s, Bool #))
    -> State# s
    -> (# State# s, Maybe a #)
foo [] f s = (# s, Nothing #)
foo (x:xs) f s = case f x s of
     (# s', b #) -> case b of
         True -> (# s', Just x #)
         False -> foo xs f s'

We would like to compile the call to foo as a local jump instead of a call
(see Note [Self-recursive tail calls]). However, the generated function has
an arity of 2 while we apply it to 3 arguments, one of them being of void
type. Thus, we mustn't count arguments of void type when checking whether
we can turn a call into a self-recursive jump.

