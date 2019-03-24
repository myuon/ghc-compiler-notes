`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/libraries/base/GHC/Float.hs>`_

Note [realToFrac int-to-float]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Don found that the RULES for realToFrac/Int->Double and simliarly
Float made a huge difference to some stream-fusion programs.  Here's
an example

.. code-block:: haskell

      import Data.Array.Vector

.. code-block:: haskell

      n = 40000000

.. code-block:: haskell

      main = do
            let c = replicateU n (2::Double)
                a = mapU realToFrac (enumFromToU 0 (n-1) ) :: UArr Double
            print (sumU (zipWithU (*) c a))

Without the RULE we get this loop body:

.. code-block:: haskell

      case $wtoRational sc_sY4 of ww_aM7 { (# ww1_aM9, ww2_aMa #) ->
      case $wfromRat ww1_aM9 ww2_aMa of tpl_X1P { D# ipv_sW3 ->
      Main.$s$wfold
        (+# sc_sY4 1)
        (+# wild_X1i 1)
        (+## sc2_sY6 (*## 2.0 ipv_sW3))

And with the rule:

.. code-block:: haskell

     Main.$s$wfold
        (+# sc_sXT 1)
        (+# wild_X1h 1)
        (+## sc2_sXV (*## 2.0 (int2Double# sc_sXT)))

The running time of the program goes from 120 seconds to 0.198 seconds
with the native backend, and 0.143 seconds with the C backend.

A few more details in #2251, and the patch message
"Add RULES for realToFrac from Int".
Utils


Note [Casting from integral to floating point types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
To implement something like `reinterpret_cast` from C++ to go from a
floating-point type to an integral type one might niavely think that the
following should work:

.. code-block:: haskell

      cast :: Float -> Word32
      cast (F# f#) = W32# (unsafeCoerce# f#)

Unfortunately that is not the case, because all the `unsafeCoerce#` does is tell
the compiler that the types have changed. When one does the above cast and
tries to operate on the resulting `Word32` the code generator will generate code
that performs an integer/word operation on a floating-point register, which
results in a compile error.

The correct way of implementing `reinterpret_cast` to implement a primpop, but
that requires a unique implementation for all supported archetectures. The next
best solution is to write the value from the source register to memory and then
read it from memory into the destination register and the best way to do that
is using CMM.

