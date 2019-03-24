`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/simplCore/FloatIn.hs>`_

====================
compiler/simplCore/FloatIn.hs.rst
====================

Note [Dead bindings]
~~~~~~~~~~~~~~~~~~~~~~~
At a literal we won't usually have any floated bindings; the
only way that can happen is if the binding wrapped the literal
/in the original input program/.  e.g.
   case x of { DEFAULT -> 1# }
But, while this may be unusual it is not actually wrong, and it did
once happen (#15696).



Note [Do not destroy the let/app invariant]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Watch out for
   f (x +# y)
We don't want to float bindings into here
   f (case ... of { x -> x +# y })
because that might destroy the let/app invariant, which requires
unlifted function arguments to be ok-for-speculation.



Note [Join points]
~~~~~~~~~~~~~~~~~~
Generally, we don't need to worry about join points - there are places we're
not allowed to float them, but since they can't have occurrences in those
places, we're not tempted.

We do need to be careful about jumps, however:

.. code-block:: haskell

  joinrec j x y z = ... in
  jump j a b c

Previous versions often floated the definition of a recursive function into its
only non-recursive occurrence. But for a join point, this is a disaster:

.. code-block:: haskell

  (joinrec j x y z = ... in
  jump j) a b c -- wrong!

Every jump must be exact, so the jump to j must have three arguments. Hence
we're careful not to float into the target of a jump (though we can float into
the arguments just fine).



Note [Floating in past a lambda group]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
* We must be careful about floating inside a value lambda.
  That risks losing laziness.
  The float-out pass might rescue us, but then again it might not.

* We must be careful about type lambdas too.  At one time we did, and
  there is no risk of duplicating work thereby, but we do need to be
  careful.  In particular, here is a bad case (it happened in the
  cichelli benchmark:
        let v = ...
        in let f = /\t -> \a -> ...
           ==>
        let f = /\t -> let v = ... in \a -> ...
  This is bad as now f is an updatable closure (update PAP)
  and has arity 0.

* Hack alert!  We only float in through one-shot lambdas,
  not (as you might guess) through lone big lambdas.
  Reason: we float *out* past big lambdas (see the test in the Lam
  case of FloatOut.floatExpr) and we don't want to float straight
  back in again.

.. code-block:: haskell

  It *is* important to float into one-shot lambdas, however;
  see the remarks with noFloatIntoRhs.

So we treat lambda in groups, using the following rule:

.. code-block:: haskell

 Float in if (a) there is at least one Id,
         and (b) there are no non-one-shot Ids

.. code-block:: haskell

 Otherwise drop all the bindings outside the group.

This is what the 'go' function in the AnnLam case is doing.

(Join points are handled similarly: a join point is considered one-shot iff
it's non-recursive, so we float only into non-recursive join points.)

Urk! if all are tyvars, and we don't float in, we may miss an
      opportunity to float inside a nested case branch




Note [Floating coercions]
~~~~~~~~~~~~~~~~~~~~~~~~~
We could, in principle, have a coercion binding like
   case f x of co { DEFAULT -> e1 e2 }
It's not common to have a function that returns a coercion, but nothing
in Core prohibits it.  If so, 'co' might be mentioned in e1 or e2
/only in a type/.  E.g. suppose e1 was
  let (x :: Int |> co) = blah in blah2


But, with coercions appearing in types, there is a complication: we
might be floating in a "strict let" -- that is, a case. Case expressions
mention their return type. We absolutely can't float a coercion binding
inward to the point that the type of the expression it's about to wrap
mentions the coercion. So we include the union of the sets of free variables
of the types of all the drop points involved. If any of the floaters
bind a coercion variable mentioned in any of the types, that binder must
be dropped right away.



Note [extra_fvs (1): avoid floating into RHS]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider let x=\y....t... in body.  We do not necessarily want to float
a binding for t into the RHS, because it'll immediately be floated out
again.  (It won't go inside the lambda else we risk losing work.)
In letrec, we need to be more careful still. We don't want to transform
        let x# = y# +# 1#
        in
        letrec f = \z. ...x#...f...
        in ...
into
        letrec f = let x# = y# +# 1# in \z. ...x#...f... in ...
because now we can't float the let out again, because a letrec
can't have unboxed bindings.

So we make "extra_fvs" which is the rhs_fvs of such bindings, and
arrange to dump bindings that bind extra_fvs before the entire let.



Note [extra_fvs (2): free variables of rules]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
  let x{rule mentioning y} = rhs in body
Here y is not free in rhs or body; but we still want to dump bindings
that bind y outside the let.  So we augment extra_fvs with the
idRuleAndUnfoldingVars of x.  No need for type variables, hence not using
idFreeVars.


Note [Floating primops]
~~~~~~~~~~~~~~~~~~~~~~~~~~
We try to float-in a case expression over an unlifted type.  The
motivating example was #5658: in particular, this change allows
array indexing operations, which have a single DEFAULT alternative
without any binders, to be floated inward.

SIMD primops for unpacking SIMD vectors into an unboxed tuple of unboxed
scalars also need to be floated inward, but unpacks have a single non-DEFAULT
alternative that binds the elements of the tuple. We now therefore also support
floating in cases with a single alternative that may bind values.

But there are wrinkles

* Which unlifted cases do we float? See PrimOp.hs
  Note [PrimOp can_fail and has_side_effects] which explains:
   - We can float-in can_fail primops, but we can't float them out.
   - But we can float a has_side_effects primop, but NOT inside a lambda,
     so for now we don't float them at all.
  Hence exprOkForSideEffects

* Because we can float can-fail primops (array indexing, division) inwards
  but not outwards, we must be careful not to transform
     case a /# b of r -> f (F# r)
  ===>
    f (case a /# b of r -> F# r)
  because that creates a new thunk that wasn't there before.  And
  because it can't be floated out (can_fail), the thunk will stay
  there.  Disaster!  (This happened in nofib 'simple' and 'scs'.)

.. code-block:: haskell

  Solution: only float cases into the branches of other cases, and
  not into the arguments of an application, or the RHS of a let. This
  is somewhat conservative, but it's simple.  And it still hits the
  cases like #5658.   This is implemented in sepBindsByJoinPoint;
  if is_case is False we dump all floating cases right here.

* #14511 is another example of why we want to restrict float-in
  of case-expressions.  Consider
     case indexArray# a n of (# r #) -> writeArray# ma i (f r)
  Now, floating that indexing operation into the (f r) thunk will
  not create any new thunks, but it will keep the array 'a' alive
  for much longer than the programmer expected.

.. code-block:: haskell

  So again, not floating a case into a let or argument seems like
  the Right Thing

For @Case@, the possible drop points for the 'to_drop'
bindings are:
  (a) inside the scrutinee
  (b) inside one of the alternatives/default (default FVs always /first/!).



Note [noFloatInto considerations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When do we want to float bindings into
   - noFloatIntoRHs: the RHS of a let-binding
   - noFloatIntoArg: the argument of a function application

Definitely don't float in if it has unlifted type; that
would destroy the let/app invariant.

* Wrinkle 1: do not float in if
     (a) any non-one-shot value lambdas
  or (b) all type lambdas
  In both cases we'll float straight back out again
  NB: Must line up with fiExpr (AnnLam...); see #7088

.. code-block:: haskell

  (a) is important: we /must/ float into a one-shot lambda group
  (which includes join points). This makes a big difference
  for things like
     f x# = let x = I# x#
            in let j = \() -> ...x...
               in if <condition> then normal-path else j ()
  If x is used only in the error case join point, j, we must float the
  boxing constructor into it, else we box it every time which is very
  bad news indeed.

* Wrinkle 2: for RHSs, do not float into a HNF; we'll just float right
  back out again... not tragic, but a waste of time.

.. code-block:: haskell

  For function arguments we will still end up with this
  in-then-out stuff; consider
    letrec x = e in f x
  Here x is not a HNF, so we'll produce
    f (letrec x = e in x)
  which is OK... it's not that common, and we'll end up
  floating out again, in CorePrep if not earlier.
  Still, we use exprIsTrivial to catch this case (sigh)




Note [Duplicating floats]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

For case expressions we duplicate the binding if it is reasonably
small, and if it is not used in all the RHSs This is good for
situations like
     let x = I# y in
     case e of
       C -> error x
       D -> error x
       E -> ...not mentioning x...

If the thing is used in all RHSs there is nothing gained,
so we don't duplicate then.

