[[src]](https://github.com/ghc/ghc/tree/master/compiler/codeGen/StgCmmExpr.hs)
 Generating code for a let-no-escape binding, aka join point is very
very similar to what we do for a case expression.  The duality is
between
        let-no-escape x = b
        in e
and
        case e of ... -> b

That is, the RHS of 'x' (ie 'b') will execute *later*, just like
the alternative of the case; it needs to be compiled in an environment
in which all volatile bindings are forgotten, and the free vars are
bound only to stable things like stack locations..  The 'e' part will
execute *next*, just like the scrutinee of a case. 

### Note: Compiling case expressions

It is quite interesting to decide whether to put a heap-check at the
start of each alternative.  Of course we certainly have to do so if
the case forces an evaluation, or if there is a primitive op which can
trigger GC.

A more interesting situation is this (a Plan-B situation)

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

        case e of r { alts }

------ Plan A: the general case ---------

        ...save current cost centre...

        ...code for e,
           with sequel (SetLocals r)

        ...restore current cost centre...
        ...code for alts...
        ...alts do their own heap checks

------ Plan B: special case when ---------
  (i)  e does not allocate or call GC
  (ii) either upstream code performs allocation
       or there is just one alternative

  Then heap allocation in the (single) case branch
  is absorbed by the upstream check.
  Very common example: primops on unboxed values

        ...code for e,
           with sequel (SetLocals r)...

        ...code for alts...
        ...no heap check...


### Note: case on bool

This special case handles code like

  case a <# b of
    True ->
    False ->

-->  case tagToEnum# (a <$# b) of
        True -> .. ; False -> ...

--> case (a <$# b) of r ->
    case tagToEnum# r of
        True -> .. ; False -> ...

If we let the ordinary case code handle it, we'll get something like

 tmp1 = a < b
 tmp2 = Bool_closure_tbl[tmp1]
 if (tmp2 & 7 != 0) then ... // normal tagged case

but this junk won't optimise away.  What we really want is just an
inline comparison:

 if (a < b) then ...

So we add a special case to generate

 tmp1 = a < b
 if (tmp1 == 0) then ...

and later optimisations will further improve this.

Now that #6135 has been resolved it should be possible to remove that
special case. The idea behind this special case and pre-6135 implementation
of Bool-returning primops was that tagToEnum# was added implicitly in the
codegen and then optimized away. Now the call to tagToEnum# is explicit
in the source code, which allows to optimize it away at the earlier stages
of compilation (i.e. at the Core level).

### Note: Scrutinising VoidRep

### Note: Dodgy unsafeCoerce 1

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


### Note: Dodgy unsafeCoerce 2, #3132

In all other cases of a lifted Id being cast to an unlifted type, the
Id should be bound to bottom, otherwise this is an unsafe use of
unsafeCoerce.  We can generate code to enter the Id and assume that
it will never return.  Hence, we emit the usual enter/return code, and
because bottom must be untagged, it will be entered.  The Sequel is a
type-correct assignment, albeit bogus.  The (dead) continuation loops;
it would be better to invoke some kind of panic function here.


### Note: Handle seq#

### Note: seq# magic

### Note: scrut sequel

### Note: GC for conditionals

### Note: case on bool

### Note: Compiling case expressions

ToDo: figure out what the Right Rule should be.

### Note: scrut sequel

### Note: Compiling case expressions

### Note: Inlining out-of-line primops and heap checks

If shouldInlinePrimOp returns True when called from StgCmmExpr for the
purpose of heap check placement, we *must* inline the primop later in
StgCmmPrim. If we don't things will go wrong.
