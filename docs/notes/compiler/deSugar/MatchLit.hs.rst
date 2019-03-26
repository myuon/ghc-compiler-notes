`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/deSugar/MatchLit.hs>`_

compiler/deSugar/MatchLit.hs
============================


Note [Literal short cut]
~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/deSugar/MatchLit.hs#L121>`__

The type checker tries to do this short-cutting as early as possible, but
because of unification etc, more information is available to the desugarer.
And where it's possible to generate the correct literal right away, it's
much better to do so.



Note [Suggest NegativeLiterals]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/deSugar/MatchLit.hs#L239>`__

If you write
  x :: Int8
  x = -128
it'll parse as (negate 128), and overflow.  In this case, suggest NegativeLiterals.
We get an erroneous suggestion for
  x = 128
but perhaps that does not matter too much.

