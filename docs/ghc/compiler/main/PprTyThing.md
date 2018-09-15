[[src]](https://github.com/ghc/ghc/tree/master/compiler/main/PprTyThing.hs)
### Note: Pretty-printing TyThings

We pretty-print a TyThing by converting it to an IfaceDecl,
and pretty-printing that (see ppr_ty_thing below).
Here is why:

* When pretty-printing (a type, say), the idiomatic solution is not to
  "rename type variables on the fly", but rather to "tidy" the type
  (which gives each variable a distinct print-name), and then
  pretty-print it (without renaming). Separate the two
  concerns. Functions like tidyType do this.

* Alas, for type constructors, TyCon, tidying does not work well,
  because a TyCon includes DataCons which include Types, which mention
  TyCons. And tidying can't tidy a mutually recursive data structure
  graph, only trees.

* One alternative would be to ensure that TyCons get type variables
  with distinct print-names. That's ok for type variables but less
  easy for kind variables. Processing data type declarations is
  already so complicated that I don't think it's sensible to add the
  extra requirement that it generates only "pretty" types and kinds.

*  One place the non-pretty names can show up is in GHCi. But another
   is in interface files. Look at MkIface.tyThingToIfaceDecl which
   converts a TyThing (i.e. TyCon, Class etc) to an IfaceDecl. And it
   already does tidying as part of that conversion!  Why? Because
   interface files contains fast-strings, not uniques, so the names
   must at least be distinct.

So if we convert to IfaceDecl, we get a nice tidy IfaceDecl, and can
print that.  Of course, that means that pretty-printing IfaceDecls
must be careful to display nice user-friendly results, but that's ok.

See #7730, #8776 for details   