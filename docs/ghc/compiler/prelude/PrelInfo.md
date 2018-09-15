[[src]](https://github.com/ghc/ghc/tree/master/compiler/prelude/PrelInfo.hs)

(c) The GRASP/AQUA Project, Glasgow University, 1992-1998



# \subsection[builtinNameInfo]{Lookup built-in names}


### Note: About wired-in things

* Wired-in things are Ids\/TyCons that are completely known to the compiler.
  They are global values in GHC, (e.g.  listTyCon :: TyCon).

* A wired in Name contains the thing itself inside the Name:
        see Name.wiredInNameTyThing_maybe
  (E.g. listTyConName contains listTyCon.

### Note: Known-

* The type environment itself contains no wired in things. The type
  checker sees if the Name is wired in before looking up the name in
  the type environment.

* MkIface prunes out wired-in things before putting them in an interface file.
  So interface files never contain wired-in things.


", doc, text "


We let a lot of "non-standard" values be visible, so that we can make
sense of them in interface pragmas. It's cool, though they all have
"non-standard" names, so they won't get past the parser in user code.

# PrimOpIds


# Export lists for pseudo-modules (GHC.Prim)


GHC.Prim "exports" all the primops and primitive types, some
wired-in Ids.


# Built-in keys


ToDo: make it do the ``like'' part properly (as in 0.26 and before).


# Class predicates
