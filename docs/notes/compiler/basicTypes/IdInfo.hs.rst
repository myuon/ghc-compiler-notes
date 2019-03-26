`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/basicTypes/IdInfo.hs>`_

compiler/basicTypes/IdInfo.hs
=============================


Note [Specialisations and RULES in IdInfo]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/basicTypes/IdInfo.hs#L385>`__

Generally speaking, a GlobalId has an *empty* RuleInfo.  All their
RULES are contained in the globally-built rule-base.  In principle,
one could attach the to M.f the RULES for M.f that are defined in M.
But we don't do that for instance declarations and so we just treat
them all uniformly.

The EXCEPTION is PrimOpIds, which do have rules in their IdInfo. That is
jsut for convenience really.

However, LocalIds may have non-empty RuleInfo.  We treat them
differently because:
  a) they might be nested, in which case a global table won't work
  b) the RULE might mention free variables, which we use to keep things alive

In TidyPgm, when the LocalId becomes a GlobalId, its RULES are stripped off
and put in the global list.



Note [Levity info]
~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/basicTypes/IdInfo.hs#L590>`__

Ids store whether or not they can be levity-polymorphic at any amount
of saturation. This is helpful in optimizing the levity-polymorphism check
done in the desugarer, where we can usually learn that something is not
levity-polymorphic without actually figuring out its type. See
isExprLevPoly in CoreUtils for where this info is used. Storing
this is required to prevent perf/compiler/T5631 from blowing up.

See Note [Levity info]

