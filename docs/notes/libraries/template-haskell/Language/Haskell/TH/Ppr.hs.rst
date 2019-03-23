`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/libraries/template-haskell/Language/Haskell/TH/Ppr.hs>`_

Note [Pretty-printing kind signatures]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GHC's parser only recognises a kind signature in a type when there are
parens around it.  E.g. the parens are required here:
   f :: (Int :: *)
   type instance F Int = (Bool :: *)
So we always print a SigT with parens (see #10050). 
