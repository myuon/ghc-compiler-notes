`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/libraries/base/GHC/IORef.hs>`_

====================
libraries/base/GHC/IORef.hs.rst
====================

Note [atomicModifyIORef' definition]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

atomicModifyIORef' was historically defined

.. code-block:: haskell

   atomicModifyIORef' ref f = do
       b <- atomicModifyIORef ref $ \a ->
               case f a of
                   v@(a',_) -> a' `seq` v
       b `seq` return b

The most obvious definition, now that we have atomicModifyMutVar2#,
would be

.. code-block:: haskell

   atomicModifyIORef' ref f = do
     (_old, (!_new, !res)) <- atomicModifyIORef2 ref f
     pure res

Why do we force the new value on the "inside" instead of afterwards?
I initially thought the latter would be okay, but then I realized
that if we write

.. code-block:: haskell

  atomicModifyIORef' ref $ \x -> (x + 5, x - 5)

then we'll end up building a pair of thunks to calculate x + 5
and x - 5. That's no good! With the more complicated definition,
we avoid this problem; the result pair is strict in the new IORef
contents. Of course, if the function passed to atomicModifyIORef'
doesn't inline, we'll build a closure for it. But that was already
true for the historical definition of atomicModifyIORef' (in terms
of atomicModifyIORef), so we shouldn't lose anything. Note that
in keeping with the historical behavior, we *don't* propagate the
strict demand on the result inwards. In particular,

.. code-block:: haskell

  atomicModifyIORef' ref (\x -> (x + 1, undefined))

will increment the IORef and throw an exception; it will not
install an undefined value in the IORef.

A clearer version, in my opinion (but one quite incompatible with
the traditional one) would only force the new IORef value and not
the result. This version would have been relatively inefficient
to implement using atomicModifyMutVar#, but is just fine now.

