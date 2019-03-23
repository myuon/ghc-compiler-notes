`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/prelude/PrelInfo.hs>`_

Note [About wired-in things]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
* Wired-in things are Ids\/TyCons that are completely known to the compiler.
  They are global values in GHC, (e.g.  listTyCon :: TyCon).

* A wired in Name contains the thing itself inside the Name:
        see Name.wiredInNameTyThing_maybe
  (E.g. listTyConName contains listTyCon.

* The name cache is initialised with (the names of) all wired-in things
  (except tuples and sums; see Note [Known-])

* The type environment itself contains no wired in things. The type
  checker sees if the Name is wired in before looking up the name in
  the type environment.

* MkIface prunes out wired-in things before putting them in an interface file.
  So interface files never contain wired-in things.

