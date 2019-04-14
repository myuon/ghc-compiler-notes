`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/prelude/PrelNames.hs>`_

compiler/prelude/PrelNames.hs
=============================


Note [Known-key names]
~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/prelude/PrelNames.hs#L38>`__

It is *very* important that the compiler gives wired-in things and
things with "known-key" names the correct Uniques wherever they
occur. We have to be careful about this in exactly two places:

::

  1. When we parse some source code, renaming the AST better yield an
     AST whose Names have the correct uniques

..

  2. When we read an interface file, the read-in gubbins better have
     the right uniques

This is accomplished through a combination of mechanisms:

::

  1. When parsing source code, the RdrName-decorated AST has some
     RdrNames which are Exact. These are wired-in RdrNames where the
     we could directly tell from the parsed syntax what Name to
     use. For example, when we parse a [] in a type we can just insert
     an Exact RdrName Name with the listTyConKey.

..

::

     Currently, I believe this is just an optimisation: it would be
     equally valid to just output Orig RdrNames that correctly record
     the module etc we expect the final Name to come from. However,
     were we to eliminate isBuiltInOcc_maybe it would become essential
     (see point 3).

..

  2. The knownKeyNames (which consist of the basicKnownKeyNames from
     the module, and those names reachable via the wired-in stuff from
     TysWiredIn) are used to initialise the "OrigNameCache" in
     IfaceEnv.  This initialization ensures that when the type checker
     or renamer (both of which use IfaceEnv) look up an original name
     (i.e. a pair of a Module and an OccName) for a known-key name
     they get the correct Unique.

::

     This is the most important mechanism for ensuring that known-key
     stuff gets the right Unique, and is why it is so important to
     place your known-key names in the appropriate lists.

..

::

  3. For "infinite families" of known-key names (i.e. tuples and sums), we
     have to be extra careful. Because there are an infinite number of
     these things, we cannot add them to the list of known-key names
     used to initialise the OrigNameCache. Instead, we have to
     rely on never having to look them up in that cache. See
     Note [Infinite families of known-key names] for details.

..



Note [Infinite families of known-key names]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/prelude/PrelNames.hs#L84>`__

Infinite families of known-key things (e.g. tuples and sums) pose a tricky
problem: we can't add them to the knownKeyNames finite map which we use to
ensure that, e.g., a reference to (,) gets assigned the right unique (if this
doesn't sound familiar see Note [Known-key names] above).

We instead handle tuples and sums separately from the "vanilla" known-key
things,

::

  a) The parser recognises them specially and generates an Exact Name (hence not
     looked up in the orig-name cache)

..

  b) The known infinite families of names are specially serialised by
     BinIface.putName, with that special treatment detected when we read back to
     ensure that we get back to the correct uniques. See Note [Symbol table
     representation of names] in BinIface and Note [How tuples work] in
     TysWiredIn.

Most of the infinite families cannot occur in source code, so mechanisms (a) and (b)
suffice to ensure that they always have the right Unique. In particular,
implicit param TyCon names, constraint tuples and Any TyCons cannot be mentioned
by the user. For those things that *can* appear in source programs,

  c) IfaceEnv.lookupOrigNameCache uses isBuiltInOcc_maybe to map built-in syntax
     directly onto the corresponding name, rather than trying to find it in the
     original-name cache.

::

     See also Note [Built-in syntax and the OrigNameCache]

..



Note [The integer library]
~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/prelude/PrelNames.hs#L115>`__

Clearly, we need to know the names of various definitions of the integer
library, e.g. the type itself, `mkInteger` etc. But there are two possible
implementations of the integer library:

 * integer-gmp (fast, but uses libgmp, which may not be available on all
   targets and is GPL licensed)
 * integer-simple (slow, but pure Haskell and BSD-licensed)

We want the compiler to work with either one. The way we achieve this is:

 * When compiling the integer-{gmp,simple} library, we pass
     -this-unit-id  integer-wired-in
   to GHC (see the cabal file libraries/integer-{gmp,simple}.
 * This way, GHC can use just this UnitID (see Module.integerUnitId) when
   generating code, and the linker will succeed.

Unfortuately, the abstraction is not complete: When using integer-gmp, we
really want to use the S# constructor directly. This is controlled by
the `integerLibrary` field of `DynFlags`: If it is IntegerGMP, we use
this constructor directly (see  CorePrep.lookupIntegerSDataConName)

When GHC reads the package data base, it (internally only) pretends it has UnitId
`integer-wired-in` instead of the actual UnitId (which includes the version
number); just like for `base` and other packages, as described in
Note [Wired-in packages] in Module. This is done in Packages.findWiredInPackages.

