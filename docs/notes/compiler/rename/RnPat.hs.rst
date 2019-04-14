`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/rename/RnPat.hs>`_

compiler/rename/RnPat.hs
========================


Note [CpsRn monad]
~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/rename/RnPat.hs#L86>`__

The CpsRn monad uses continuation-passing style to support this
style of programming:

::

        do { ...
           ; ns <- bindNames rs
           ; ...blah... }

..

::

   where rs::[RdrName], ns::[Name]

..

The idea is that '...blah...'
  a) sees the bindings of ns
  b) returns the free variables it mentions
     so that bindNames can report unused ones

In particular,
    mapM rnPatAndThen [p1, p2, p3]
has a *left-to-right* scoping: it makes the binders in
p1 scope over p2,p3.



Note [Patterns are uses]
~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/rename/RnPat.hs#L150>`__

Consider
  module Foo( f, g ) where
  data T = T1 | T2

::

  f T1 = True
  f T2 = False

..

::

  g _ = T1

..

Arguably we should report T2 as unused, even though it appears in a
pattern, because it never occurs in a constructed position.  See
#7336.
However, implementing this in the face of pattern synonyms would be
less straightforward, since given two pattern synonyms

::

  pattern P1 <- P2
  pattern P2 <- ()

..

we need to observe the dependency between P1 and P2 so that type
checking can be done in the correct order (just like for value
bindings). Dependencies between bindings is analyzed in the renamer,
where we don't know yet whether P2 is a constructor or a pattern
synonym. So for now, we do report conid occurrences in patterns as
uses.



Note [View pattern usage]
~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/rename/RnPat.hs#L254>`__

Consider
  let (r, (r -> x)) = x in ...
Here the pattern binds 'r', and then uses it *only* in the view pattern.
We want to "see" this use, and in let-bindings we collect all uses and
report unused variables at the binding level. So we must use bindLocalNames
here, *not* bindLocalNameFV.  #3943.



Note [Don't report shadowing for pattern synonyms]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/rename/RnPat.hs#L264>`__

There is one special context where a pattern doesn't introduce any new binders -
pattern synonym declarations. Therefore we don't check to see if pattern
variables shadow existing identifiers as they are never bound to anything
and have no scope.

Without this check, there would be quite a cryptic warning that the `x`
in the RHS of the pattern synonym declaration shadowed the top level `x`.

```
x :: ()
x = ()

pattern P x = Just x
```

See #12615 for some more examples.



Note [Negative zero]
~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/rename/RnPat.hs#L848>`__

There were problems with negative zero in conjunction with Negative Literals
extension. Numeric literal value is contained in Integer and Rational types
inside IntegralLit and FractionalLit. These types cannot represent negative
zero value. So we had to add explicit field 'neg' which would hold information
about literal sign. Here in rnOverLit we use it to detect negative zeroes and
in this case return not only literal itself but also negateName so that users
can apply it explicitly. In this case it stays negative zero.  #13211

