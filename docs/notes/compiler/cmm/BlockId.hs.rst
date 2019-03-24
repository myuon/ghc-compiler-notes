`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/cmm/BlockId.hs>`_

====================
compiler/cmm/BlockId.hs.rst
====================

Note [Unique BlockId]
~~~~~~~~~~~~~~~~~~~~~~~~
Although a 'BlockId' is a local label, for reasons of implementation,
'BlockId's must be unique within an entire compilation unit.  The reason
is that each local label is mapped to an assembly-language label, and in
most assembly languages allow, a label is visible throughout the entire
compilation unit in which it appears.

