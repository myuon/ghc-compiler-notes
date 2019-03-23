`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/coreSyn/CoreTidy.hs>`_

Note [Tidy IdInfo]
~~~~~~~~~~~~~~~~~~
All nested Ids now have the same IdInfo, namely vanillaIdInfo, which
should save some space; except that we preserve occurrence info for
two reasons:

  (a) To make printing tidy core nicer

  (b) Because we tidy RULES and InlineRules, which may then propagate
      via --make into the compilation of the next module, and we want
      the benefit of that occurrence analysis when we use the rule or
      or inline the function.  In particular, it's vital not to lose
      loop-breaker info, else we get an infinite inlining loop

Note that tidyLetBndr puts more IdInfo back.



Note [Preserve evaluatedness]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
  data T = MkT !Bool
  ....(case v of MkT y ->
       let z# = case y of
                  True -> 1#
                  False -> 2#
       in ...)

The z# binding is ok because the RHS is ok-for-speculation,
but Lint will complain unless it can *see* that.  So we
preserve the evaluated-ness on 'y' in tidyBndr.

(Another alternative would be to tidy unboxed lets into cases,
but that seems more indirect and surprising.)



Note [Preserve OneShotInfo]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
We keep the OneShotInfo because we want it to propagate into the interface.
Not all OneShotInfo is determined by a compiler analysis; some is added by a
call of GHC.Exts.oneShot, which is then discarded before the end of the
optimisation pipeline, leaving only the OneShotInfo on the lambda. Hence we
must preserve this info in inlinings. See Note [The oneShot function] in MkId.

This applies to lambda binders only, hence it is stored in IfaceLamBndr.

