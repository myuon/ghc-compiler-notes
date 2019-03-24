`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/utils/FV.hs>`_

====================
compiler/utils/FV.hs.rst
====================

Note [Deterministic FV]
~~~~~~~~~~~~~~~~~~~~~~~
When computing free variables, the order in which you get them affects
the results of floating and specialization. If you use UniqFM to collect
them and then turn that into a list, you get them in nondeterministic
order as described in Note [Deterministic UniqFM] in UniqDFM.
A naive algorithm for free variables relies on merging sets of variables.
Merging costs O(n+m) for UniqFM and for UniqDFM there's an additional log
factor. It's cheaper to incrementally add to a list and use a set to check
for duplicates.


Note [FV naming conventions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
To get the performance and determinism that FV provides, FV computations
need to built up from smaller FV computations and then evaluated with
one of `fvVarList`, `fvDVarSet`, `fvVarListVarSet`. That means the functions
returning FV need to be exported.

The conventions are:

a) non-deterministic functions:
  * a function that returns VarSet
      e.g. `tyVarsOfType`
b) deterministic functions:
  * a worker that returns FV
      e.g. `tyFVsOfType`
  * a function that returns [Var]
      e.g. `tyVarsOfTypeList`
  * a function that returns DVarSet
      e.g. `tyVarsOfTypeDSet`

Where tyVarsOfType, tyVarsOfTypeList, tyVarsOfTypeDSet are implemented
in terms of the worker evaluated with fvVarSet, fvVarList, fvDVarSet
respectively.


Note [FV eta expansion]
~~~~~~~~~~~~~~~~~~~~~~~
Let's consider an eta-reduced implementation of freeVarsOf using FV:

freeVarsOf (App a b) = freeVarsOf a `unionFV` freeVarsOf b

If GHC doesn't eta-expand it, after inlining unionFV we end up with

freeVarsOf = \x ->
  case x of
    App a b -> \fv_cand in_scope acc ->
      freeVarsOf a fv_cand in_scope $! freeVarsOf b fv_cand in_scope $! acc

which has to create a thunk, resulting in more allocations.

On the other hand if it is eta-expanded:

freeVarsOf (App a b) fv_cand in_scope acc =
  (freeVarsOf a `unionFV` freeVarsOf b) fv_cand in_scope acc

after inlining unionFV we have:

freeVarsOf = \x fv_cand in_scope acc ->
  case x of
    App a b ->
      freeVarsOf a fv_cand in_scope $! freeVarsOf b fv_cand in_scope $! acc

which saves allocations.

GHC when presented with knowledge about all the call sites, correctly
eta-expands in this case. Unfortunately due to the fact that freeVarsOf gets
exported to be composed with other functions, GHC doesn't have that
information and has to be more conservative here.

Hence functions that get exported and return FV need to be manually
eta-expanded. See also #11146.

