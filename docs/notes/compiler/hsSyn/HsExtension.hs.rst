`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/hsSyn/HsExtension.hs>`_

compiler/hsSyn/HsExtension.hs
=============================


Note [Trees that grow]
~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/hsSyn/HsExtension.hs#L32>`__

See https://ghc.haskell.org/trac/ghc/wiki/ImplementingTreesThatGrow

The hsSyn AST is reused across multiple compiler passes. We also have the
Template Haskell AST, and the haskell-src-exts one (outside of GHC)

Supporting multiple passes means the AST has various warts on it to cope with
the specifics for the phases, such as the 'ValBindsOut', 'ConPatOut',
'SigPatOut' etc.

The growable AST will allow each of these variants to be captured explicitly,
such that they only exist in the given compiler pass AST, as selected by the
type parameter to the AST.

In addition it will allow tool writers to define their own extensions to capture
additional information for the tool, in a natural way.

A further goal is to provide a means to harmonise the Template Haskell and
haskell-src-exts ASTs as well.



Note [OutputableX]
~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/hsSyn/HsExtension.hs#L1070>`__

is required because the type family resolution
process cannot determine that all cases are handled for a `GhcPass p`
case where the cases are listed separately.

So

::

  type instance XXHsIPBinds    (GhcPass p) = NoExt

will correctly deduce Outputable for (GhcPass p), but

::

  type instance XIPBinds       GhcPs = NoExt
  type instance XIPBinds       GhcRn = NoExt
  type instance XIPBinds       GhcTc = TcEvBinds

will not.

