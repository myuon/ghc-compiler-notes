`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/basicTypes/NameCache.hs>`_

compiler/basicTypes/NameCache.hs
================================


Note [The Name Cache]
~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/basicTypes/NameCache.hs#L27>`__

The Name Cache makes sure that, during any invocation of GHC, each
External Name "M.x" has one, and only one globally-agreed Unique.

* The first time we come across M.x we make up a Unique and record that
  association in the Name Cache.

* When we come across "M.x" again, we look it up in the Name Cache,
  and get a hit.

The functions newGlobalBinder, allocateGlobalBinder do the main work.
When you make an External name, you should probably be calling one
of them.



Note [Built-in syntax and the OrigNameCache]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/basicTypes/NameCache.hs#L43>`__

Built-in syntax like tuples and unboxed sums are quite ubiquitous. To lower
their cost we use two tricks,

::

  a. We specially encode tuple and sum Names in interface files' symbol tables
     to avoid having to look up their names while loading interface files.
     Namely these names are encoded as by their Uniques. We know how to get from
     a Unique back to the Name which it represents via the mapping defined in
     the SumTupleUniques module. See Note [Symbol table representation of names]
     in BinIface and for details.

::

  b. We don't include them in the Orig name cache but instead parse their
     OccNames (in isBuiltInOcc_maybe) to avoid bloating the name cache with
     them.

Why is the second measure necessary? Good question; afterall, 1) the parser
emits built-in syntax directly as Exact RdrNames, and 2) built-in syntax never
needs to looked-up during interface loading due to (a). It turns out that there
are two reasons why we might look up an Orig RdrName for built-in syntax,

  * If you use setRdrNameSpace on an Exact RdrName it may be
    turned into an Orig RdrName.

  * Template Haskell turns a BuiltInSyntax Name into a TH.NameG
    (DsMeta.globalVar), and parses a NameG into an Orig RdrName
    (Convert.thRdrName).  So, e.g. $(do { reify '(,); ... }) will
    go this route (#8954).

