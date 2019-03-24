`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/libraries/base/GHC/IO/Unsafe.hs>`_

====================
libraries/base/GHC/IO/Unsafe.hs.rst
====================

Note [unsafeDupableInterleaveIO should not be inlined]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We used to believe that INLINE on unsafeInterleaveIO was safe,
because the state from this IO thread is passed explicitly to the
interleaved IO, so it cannot be floated out and shared.

HOWEVER, if the compiler figures out that r is used strictly here,
then it will eliminate the thunk and the side effects in m will no
longer be shared in the way the programmer was probably expecting,
but can be performed many times.  In #5943, this broke our
definition of fixIO, which contains

.. code-block:: haskell

   ans <- unsafeInterleaveIO (takeMVar m)

after inlining, we lose the sharing of the takeMVar, so the second
time 'ans' was demanded we got a deadlock.  We could fix this with
a readMVar, but it seems wrong for unsafeInterleaveIO to sometimes
share and sometimes not (plus it probably breaks the noDuplicate).
So now, we do not inline unsafeDupableInterleaveIO.

