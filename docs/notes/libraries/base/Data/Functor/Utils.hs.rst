`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/libraries/base/Data/Functor/Utils.hs>`_

====================
libraries/base/Data/Functor/Utils.hs.rst
====================

Note [Function coercion]
~~~~~~~~~~~~~~~~~~~~~~~

Several functions here use (#.) instead of (.) to avoid potential efficiency
problems relating to #7542. The problem, in a nutshell:

If N is a newtype constructor, then N x will always have the same
representation as x (something similar applies for a newtype deconstructor).
However, if f is a function,

N . f = \x -> N (f x)

This looks almost the same as f, but the eta expansion lifts it--the lhs could
be _|_, but the rhs never is. This can lead to very inefficient code.  Thus we
steal a technique from Shachaf and Edward Kmett and adapt it to the current
(rather clean) setting. Instead of using  N . f,  we use  N #. f, which is
just

coerce f `asTypeOf` (N . f)

That is, we just *pretend* that f has the right type, and thanks to the safety
of coerce, the type checker guarantees that nothing really goes wrong. We still
have to be a bit careful, though: remember that #. completely ignores the
*value* of its left operand.

