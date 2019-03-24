`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/basicTypes/Lexeme.hs>`_

====================
compiler/basicTypes/Lexeme.hs.rst
====================

Note [Classification of generated names]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Some names generated for internal use can show up in debugging output,
e.g.  when using -ddump-simpl. These generated names start with a $
but should still be pretty-printed using prefix notation. We make sure
this is the case in isLexVarSym by only classifying a name as a symbol
if all its characters are symbols, not just its first one.

