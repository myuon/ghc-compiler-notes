`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/codeGen/StgCmmTicky.hs>`_

Note [Ticky for slow calls]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
Terminology is unfortunately a bit mixed up for these calls. codeGen
uses "slow call" to refer to unknown calls and under-saturated known
calls.

Nowadays, though (ie as of the eval/apply paper), the significantly
slower calls are actually just a subset of these: the ones with no
built-in argument pattern (cf StgCmmArgRep.slowCallPattern)

So for ticky profiling, we split slow calls into
"SLOW_CALL_fast_<pattern>_ctr" (those matching a built-in pattern) and
VERY_SLOW_CALL_ctr (those without a built-in pattern; these are very
bad for both space and time).

-----------------------------------------------------------------------------
Ticky allocation

