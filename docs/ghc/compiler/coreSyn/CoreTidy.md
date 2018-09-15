[[src]](https://github.com/ghc/ghc/tree/master/compiler/coreSyn/CoreTidy.hs)

(c) The University of Glasgow 2006
(c) The AQUA Project, Glasgow University, 1996-1998


This module contains "tidying" code for *nested* expressions, bindings, rules.
The code for *top-level* bindings is in TidyPgm.


# \subsection{Tidying expressions, rules}


# \subsection{Tidying non-top-level binders}


### Note: Tidy IdInfo

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

### Note: Preserve evaluatedness

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

### Note: Preserve OneShotInfo

### Note: The oneShot function

This applies to lambda binders only, hence it is stored in IfaceLamBndr.
