`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/main/PprTyThing.hs>`_

compiler/main/PprTyThing.hs
===========================


Note [Pretty printing via IfaceSyn]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/main/PprTyThing.hs#L40>`__

Our general plan for prett-printing
  - Types
  - TyCons
  - Classes
  - Pattern synonyms
  ...etc...

is to convert them to IfaceSyn, and pretty-print that. For example
  - pprType converts a Type to an IfaceType, and pretty prints that.
  - pprTyThing converts the TyThing to an IfaceDecl,
    and pretty prints that.

So IfaceSyn play a dual role:
  - it's the internal version of an interface files
  - it's used for pretty-printing

Why do this?

* A significant reason is that we need to be able
  to pretty-print IfaceSyn (to display Foo.hi), and it was a
  pain to duplicate masses of pretty-printing goop, esp for
  Type and IfaceType.

* When pretty-printing (a type, say), we want to tidy (with
  tidyType) to avoids having (forall a a. blah) where the two
  a's have different uniques.

  Alas, for type constructors, TyCon, tidying does not work well,
  because a TyCon includes DataCons which include Types, which mention
  TyCons. And tidying can't tidy a mutually recursive data structure
  graph, only trees.

* Interface files contains fast-strings, not uniques, so the very same
  tidying must take place when we convert to IfaceDecl. E.g.
  MkIface.tyThingToIfaceDecl which converts a TyThing (i.e. TyCon,
  Class etc) to an IfaceDecl.

  Bottom line: IfaceDecls are already 'tidy', so it's straightforward
  to print them.

* An alternative I once explored was to ensure that TyCons get type
  variables with distinct print-names. That's ok for type variables
  but less easy for kind variables. Processing data type declarations
  is already so complicated that I don't think it's sensible to add
  the extra requirement that it generates only "pretty" types and
  kinds.

Consequences:

- IfaceSyn (and IfaceType) must contain enough information to
  print nicely.  Hence, for example, the IfaceAppArgs type, which
  allows us to suppress invisible kind arguments in types
  (see Note [Suppressing invisible arguments] in IfaceType)

- In a few places we have info that is used only for pretty-printing,
  and is totally ignored when turning IfaceSyn back into TyCons
  etc (in TcIface). For example, IfaceClosedSynFamilyTyCon
  stores a [IfaceAxBranch] that is used only for pretty-printing.

- See Note [Free tyvars in IfaceType] in IfaceType

See #7730, #8776 for details   

