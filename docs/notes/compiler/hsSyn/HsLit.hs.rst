`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/hsSyn/HsLit.hs>`_

====================
compiler/hsSyn/HsLit.hs.rst
====================

Note [ol_rebindable]
~~~~~~~~~~~~~~~~~~~~
The ol_rebindable field is True if this literal is actually
using rebindable syntax.  Specifically:

.. code-block:: haskell

  False iff ol_witness is the standard one
  True  iff ol_witness is non-standard

Equivalently it's True if
  a) RebindableSyntax is on
  b) the witness for fromInteger/fromRational/fromString
     that happens to be in scope isn't the standard one



Note [Overloaded literal witnesses]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*Before* type checking, the HsExpr in an HsOverLit is the
name of the coercion function, 'fromInteger' or 'fromRational'.
*After* type checking, it is a witness for the literal, such as
        (fromInteger 3) or lit_78
This witness should replace the literal.

This dual role is unusual, because we're replacing 'fromInteger' with
a call to fromInteger.  Reason: it allows commoning up of the fromInteger
calls, which wouldn't be possible if the desugarer made the application.

The PostTcType in each branch records the type the overload literal is
found to have.
Comparison operations are needed when grouping literals
for compiling pattern-matching (module MatchLit)

