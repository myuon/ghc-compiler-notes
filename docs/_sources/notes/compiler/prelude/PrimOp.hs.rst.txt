Note [PrimOp can_fail and has_side_effects]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Both can_fail and has_side_effects mean that the primop has
some effect that is not captured entirely by its result value.

----------  has_side_effects ---------------------
A primop "has_side_effects" if it has some *write* effect, visible
elsewhere
    - writing to the world (I/O)
    - writing to a mutable data structure (writeIORef)
    - throwing a synchronous Haskell exception

Often such primops have a type like
   State -> input -> (State, output)
so the state token guarantees ordering.  In general we rely *only* on
data dependencies of the state token to enforce write-effect ordering

 * NB1: if you inline unsafePerformIO, you may end up with
   side-effecting ops whose 'state' output is discarded.
   And programmers may do that by hand; see #9390.
   That is why we (conservatively) do not discard write-effecting
   primops even if both their state and result is discarded.

 * NB2: We consider primops, such as raiseIO#, that can raise a
   (Haskell) synchronous exception to "have_side_effects" but not
   "can_fail".  We must be careful about not discarding such things;
   see the paper "A semantics for imprecise exceptions".

 * NB3: *Read* effects (like reading an IORef) don't count here,
   because it doesn't matter if we don't do them, or do them more than
   once.  *Sequencing* is maintained by the data dependency of the state
   token.

----------  can_fail ----------------------------
A primop "can_fail" if it can fail with an *unchecked* exception on
some elements of its input domain. Main examples:
   division (fails on zero demoninator)
   array indexing (fails if the index is out of bounds)

An "unchecked exception" is one that is an outright error, (not
turned into a Haskell exception,) such as seg-fault or
divide-by-zero error.  Such can_fail primops are ALWAYS surrounded
with a test that checks for the bad cases, but we need to be
very careful about code motion that might move it out of
the scope of the test.



Note [Transformations affected by can_fail and has_side_effects]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The can_fail and has_side_effects properties have the following effect
on program transformations.  Summary table is followed by details.

            can_fail     has_side_effects
Discard        YES           NO
Float in       YES           YES
Float out      NO            NO
Duplicate      YES           NO

* Discarding.   case (a `op` b) of _ -> rhs  ===>   rhs
  You should not discard a has_side_effects primop; e.g.
     case (writeIntArray# a i v s of (# _, _ #) -> True
  Arguably you should be able to discard this, since the
  returned stat token is not used, but that relies on NEVER
  inlining unsafePerformIO, and programmers sometimes write
  this kind of stuff by hand (#9390).  So we (conservatively)
  never discard a has_side_effects primop.

  However, it's fine to discard a can_fail primop.  For example
     case (indexIntArray# a i) of _ -> True
  We can discard indexIntArray#; it has can_fail, but not
  has_side_effects; see #5658 which was all about this.
  Notice that indexIntArray# is (in a more general handling of
  effects) read effect, but we don't care about that here, and
  treat read effects as *not* has_side_effects.

  Similarly (a `/#` b) can be discarded.  It can seg-fault or
  cause a hardware exception, but not a synchronous Haskell
  exception.



  Synchronous Haskell exceptions, e.g. from raiseIO#, are treated
  as has_side_effects and hence are not discarded.

* Float in.  You can float a can_fail or has_side_effects primop
  *inwards*, but not inside a lambda (see Duplication below).

* Float out.  You must not float a can_fail primop *outwards* lest
  you escape the dynamic scope of the test.  Example:
      case d ># 0# of
        True  -> case x /# d of r -> r +# 1
        False -> 0
  Here we must not float the case outwards to give
      case x/# d of r ->
      case d ># 0# of
        True  -> r +# 1
        False -> 0

  Nor can you float out a has_side_effects primop.  For example:
       if blah then case writeMutVar# v True s0 of (# s1 #) -> s1
               else s0
  Notice that s0 is mentioned in both branches of the 'if', but
  only one of these two will actually be consumed.  But if we
  float out to
      case writeMutVar# v True s0 of (# s1 #) ->
      if blah then s1 else s0
  the writeMutVar will be performed in both branches, which is
  utterly wrong.

* Duplication.  You cannot duplicate a has_side_effect primop.  You
  might wonder how this can occur given the state token threading, but
  just look at Control.Monad.ST.Lazy.Imp.strictToLazy!  We get
  something like this
        p = case readMutVar# s v of
              (# s', r #) -> (S# s', r)
        s' = case p of (s', r) -> s'
        r  = case p of (s', r) -> r

  (All these bindings are boxed.)  If we inline p at its two call
  sites, we get a catastrophe: because the read is performed once when
  s' is demanded, and once when 'r' is demanded, which may be much
  later.  Utterly wrong.  #3207 is real example of this happening.

  However, it's fine to duplicate a can_fail primop.  That is really
  the only difference between can_fail and has_side_effects.



Note [Implementation: how can_fail/has_side_effects affect transformations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
How do we ensure that that floating/duplication/discarding are done right
in the simplifier?

Two main predicates on primpops test these flags:
  primOpOkForSideEffects <=> not has_side_effects
  primOpOkForSpeculation <=> not (has_side_effects || can_fail)

  * The "no-float-out" thing is achieved by ensuring that we never
    let-bind a can_fail or has_side_effects primop.  The RHS of a
    let-binding (which can float in and out freely) satisfies
    exprOkForSpeculation; this is the let/app invariant.  And
    exprOkForSpeculation is false of can_fail and has_side_effects.

  * So can_fail and has_side_effects primops will appear only as the
    scrutinees of cases, and that's why the FloatIn pass is capable
    of floating case bindings inwards.

  * The no-duplicate thing is done via primOpIsCheap, by making
    has_side_effects things (very very very) not-cheap!


Note [primOpIsCheap]
~~~~~~~~~~~~~~~~~~~~
@primOpIsCheap@, as used in \tr{SimplUtils.hs}.  For now (HACK
WARNING), we just borrow some other predicates for a
what-should-be-good-enough test.  "Cheap" means willing to call it more
than once, and/or push it inside a lambda.  The latter could change the
behaviour of 'seq' for primops that can fail, so we don't treat them as cheap.
