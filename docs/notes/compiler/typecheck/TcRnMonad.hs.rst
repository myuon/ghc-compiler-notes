`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcRnMonad.hs>`_

Note [Default types]
~~~~~~~~~~~~~~~~~~~~~~~
The Integer type is simply not available in package ghc-prim (it is
declared in integer-gmp).  So we set the defaulting types to (Just
[]), meaning there are no default types, rather then Nothing, which
means "use the default default types of Integer, Double".

If you don't do this, attempted defaulting in package ghc-prim causes
an actual crash (attempting to look up the Integer type).




Note [Constraints and errors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this (#12124):

.. code-block:: haskell

  foo :: Maybe Int
  foo = return (case Left 3 of
                  Left -> 1  -- Hard error here!
                  _    -> 0)

The call to 'return' will generate a (Monad m) wanted constraint; but
then there'll be "hard error" (i.e. an exception in the TcM monad), from
the unsaturated Left constructor pattern.

We'll recover in tcPolyBinds, using recoverM.  But then the final
tcSimplifyTop will see that (Monad m) constraint, with 'm' utterly
un-filled-in, and will emit a misleading error message.

The underlying problem is that an exception interrupts the constraint
gathering process. Bottom line: if we have an exception, it's best
simply to discard any gathered constraints.  Hence in 'attemptM' we
capture the constraints in a fresh variable, and only emit them into
the surrounding context if we exit normally.  If an exception is
raised, simply discard the collected constraints... we have a hard
error to report.  So this capture-the-emit dance isn't as stupid as it
looks :-).

However suppose we throw an exception inside an invocation of
captureConstraints, and discard all the constraints. Some of those
constraints might be "variable out of scope" Hole constraints, and that
might have been the actual original cause of the exception!  For
example (#12529):
   f = p @ Int
Here 'p' is out of scope, so we get an insolube Hole constraint. But
the visible type application fails in the monad (thows an exception).
We must not discard the out-of-scope error.

So we /retain the insoluble constraints/ if there is an exception.
Hence:
  - insolublesOnly in tryCaptureConstraints
  - emitConstraints in the Left case of captureConstraints

However note that freshly-generated constraints like (Int ~ Bool), or
((a -> b) ~ Int) are all CNonCanonical, and hence won't be flagged as
insoluble.  The constraint solver does that.  So they'll be discarded.
That's probably ok; but see th/5358 as a not-so-good example:
   t1 :: Int
   t1 x = x   -- Manifestly wrong

   foo = $(...raises exception...)
We report the exception, but not the bug in t1.  Oh well.  Possible
solution: make TcUnify.uType spot manifestly-insoluble constraints.




Note [Masking exceptions in forkM_maybe]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When using GHC-as-API it must be possible to interrupt snippets of code
executed using runStmt (#1381). Since commit 02c4ab04 this is almost possible
by throwing an asynchronous interrupt to the GHC thread. However, there is a
subtle problem: runStmt first typechecks the code before running it, and the
exception might interrupt the type checker rather than the code. Moreover, the
typechecker might be inside an unsafeInterleaveIO (through forkM_maybe), and
more importantly might be inside an exception handler inside that
unsafeInterleaveIO. If that is the case, the exception handler will rethrow the
asynchronous exception as a synchronous exception, and the exception will end
up as the value of the unsafeInterleaveIO thunk (see #8006 for a detailed
discussion).  We don't currently know a general solution to this problem, but
we can use uninterruptibleMask_ to avoid the situation.

