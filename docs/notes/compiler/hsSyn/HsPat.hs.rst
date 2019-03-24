`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/hsSyn/HsPat.hs>`_

Note [DotDot fields]
~~~~~~~~~~~~~~~~~~~~
The rec_dotdot field means this:
  Nothing => the normal case
  Just n  => the group uses ".." notation,

In the latter case:

.. code-block:: haskell

  *before* renamer: rec_flds are exactly the n user-written fields

.. code-block:: haskell

  *after* renamer:  rec_flds includes *all* fields, with
                    the first 'n' being the user-written ones
                    and the remainder being 'filled in' implicitly


Note [Punning]
~~~~~~~~~~~~~~
If you write T { x, y = v+1 }, the HsRecFields will be
     HsRecField x x True ...
     HsRecField y (v+1) False ...
That is, for "punned" field x is expanded (in the renamer)
to x=x; but with a punning flag so we can detect it later
(e.g. when pretty printing)

If the original field was qualified, we un-qualify it, thus
   T { A.x } means T { A.x = x }


Note [HsRecField and HsRecUpdField]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A HsRecField (used for record construction and pattern matching)
contains an unambiguous occurrence of a field (i.e. a FieldOcc).
We can't just store the Name, because thanks to
DuplicateRecordFields this may not correspond to the label the user
wrote.

A HsRecUpdField (used for record update) contains a potentially
ambiguous occurrence of a field (an AmbiguousFieldOcc).  The
renamer will fill in the selector function if it can, but if the
selector is ambiguous the renamer will defer to the typechecker.
After the typechecker, a unique selector will have been determined.

The renamer produces an Unambiguous result if it can, rather than
just doing the lookup in the typechecker, so that completely
unambiguous updates can be represented by 'DsMeta.repUpdFields'.

For example, suppose we have:

.. code-block:: haskell

    data S = MkS { x :: Int }
    data T = MkT { x :: Int }

.. code-block:: haskell

    f z = (z { x = 3 }) :: S

The parsed HsRecUpdField corresponding to the record update will have:

.. code-block:: haskell

    hsRecFieldLbl = Unambiguous "x" NoExt :: AmbiguousFieldOcc RdrName

After the renamer, this will become:

.. code-block:: haskell

    hsRecFieldLbl = Ambiguous   "x" NoExt :: AmbiguousFieldOcc Name

(note that the Unambiguous constructor is not type-correct here).
The typechecker will determine the particular selector:

.. code-block:: haskell

    hsRecFieldLbl = Unambiguous "x" $sel:x:MkS  :: AmbiguousFieldOcc Id

See also Note [Disambiguating record fields] in TcExpr.


Note [Unboxed sum patterns aren't irrefutable]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Unlike unboxed tuples, unboxed sums are *not* irrefutable when used as
patterns. A simple example that demonstrates this is from #14228:

.. code-block:: haskell

  pattern Just' x = (# x | #)
  pattern Nothing' = (# | () #)

.. code-block:: haskell

  foo x = case x of
    Nothing' -> putStrLn "nothing"
    Just'    -> putStrLn "just"

In foo, the pattern Nothing' (that is, (# x | #)) is certainly not irrefutable,
as does not match an unboxed sum value of the same arity—namely, (# | y #)
(covered by Just'). In fact, no unboxed sum pattern is irrefutable, since the
minimum unboxed sum arity is 2.

Failing to mark unboxed sum patterns as non-irrefutable would cause the Just'
case in foo to be unreachable, as GHC would mistakenly believe that Nothing'
is the only thing that could possibly be matched!

