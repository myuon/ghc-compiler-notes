`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/stgSyn/StgFVs.hs>`_

compiler/stgSyn/StgFVs.hs
=========================


Note [Tracking local binders]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/stgSyn/StgFVs.hs#L46>`__

'locals' contains non-toplevel, non-imported binders.
We maintain the set in 'expr', 'alt' and 'rhs', which are the only
places where new local binders are introduced.
Why do it there rather than in 'binding'? Two reasons:

  1. We call 'binding' from 'annTopBindingsFreeVars', which would
     add top-level bindings to the 'locals' set.
  2. In the let(-no-escape) case, we need to extend the environment
     prior to analysing the body, but we also need the fvs from the
     body to analyse the RHSs. No way to do this without some
     knot-tying.

