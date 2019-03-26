`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/basicTypes/Module.hs>`_

compiler/basicTypes/Module.hs
=============================


Note [The identifier lexicon]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/basicTypes/Module.hs#L175>`__

Unit IDs, installed package IDs, ABI hashes, package names,
versions, there are a *lot* of different identifiers for closely
related things.  What do they all mean? Here's what.  (See also
https://ghc.haskell.org/trac/ghc/wiki/Commentary/Packages/Concepts )

THE IMPORTANT ONES

ComponentId: An opaque identifier provided by Cabal, which should
uniquely identify such things as the package name, the package
version, the name of the component, the hash of the source code
tarball, the selected Cabal flags, GHC flags, direct dependencies of
the component.  These are very similar to InstalledPackageId, but
an 'InstalledPackageId' implies that it identifies a package, while
a package may install multiple components with different
'ComponentId's.
     - Same as Distribution.Package.ComponentId

UnitId/InstalledUnitId: A ComponentId + a mapping from hole names
(ModuleName) to Modules.  This is how the compiler identifies instantiated
components, and also is the main identifier by which GHC identifies things.
     - When Backpack is not being used, UnitId = ComponentId.
       this means a useful fiction for end-users is that there are
       only ever ComponentIds, and some ComponentIds happen to have
       more information (UnitIds).
     - Same as Language.Haskell.TH.Syntax:PkgName, see
         https://gitlab.haskell.org/ghc/ghc/issues/10279
     - The same as PackageKey in GHC 7.10 (we renamed it because
       they don't necessarily identify packages anymore.)
     - Same as -this-package-key/-package-name flags
     - An InstalledUnitId corresponds to an actual package which
       we have installed on disk.  It could be definite or indefinite,
       but if it's indefinite, it has nothing instantiated (we
       never install partially instantiated units.)

Module/InstalledModule: A UnitId/InstalledUnitId + ModuleName. This is how
the compiler identifies modules (e.g. a Name is a Module + OccName)
     - Same as Language.Haskell.TH.Syntax:Module

THE LESS IMPORTANT ONES

PackageName: The "name" field in a Cabal file, something like "lens".
     - Same as Distribution.Package.PackageName
     - DIFFERENT FROM Language.Haskell.TH.Syntax:PkgName, see
         https://gitlab.haskell.org/ghc/ghc/issues/10279
     - DIFFERENT FROM -package-name flag
     - DIFFERENT FROM the 'name' field in an installed package
       information.  This field could more accurately be described
       as a munged package name: when it's for the main library
       it is the same as the package name, but if it's an internal
       library it's a munged combination of the package name and
       the component name.

LEGACY ONES

InstalledPackageId: This is what we used to call ComponentId.
It's a still pretty useful concept for packages that have only
one library; in that case the logical InstalledPackageId =
ComponentId.  Also, the Cabal nix-local-build continues to
compute an InstalledPackageId which is then forcibly used
for all components in a package.  This means that if a dependency
from one component in a package changes, the InstalledPackageId
changes: you don't get as fine-grained dependency tracking,
but it means your builds are hermetic.  Eventually, Cabal will
deal completely in components and we can get rid of this.

PackageKey: This is what we used to call UnitId.  We ditched
"Package" from the name when we realized that you might want to
assign different "PackageKeys" to components from the same package.
(For a brief, non-released period of time, we also called these
UnitKeys).



Note [UnitId to InstalledUnitId improvement]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/basicTypes/Module.hs#L797>`__

Just because a UnitId is definite (has no holes) doesn't
mean it's necessarily a InstalledUnitId; it could just be
that over the course of renaming UnitIds on the fly
while typechecking an indefinite library, we
ended up with a fully instantiated unit id with no hash,
since we haven't built it yet.  This is fine.

However, if there is a hashed unit id for this instantiation
in the package database, we *better use it*, because
that hashed unit id may be lurking in another interface,
and chaos will ensue if we attempt to compare the two
(the unitIdFS for a UnitId never corresponds to a Cabal-provided
hash of a compiled instantiated library).

There is one last niggle: improvement based on the package database means
that we might end up developing on a package that is not transitively
depended upon by the packages the user specified directly via command line
flags.  This could lead to strange and difficult to understand bugs if those
instantiations are out of date.  The solution is to only improve a
unit id if the new unit id is part of the 'preloadClosure'; i.e., the
closure of all the packages which were explicitly specified.



Note [Wired-in packages]
~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/basicTypes/Module.hs#L1056>`__

Certain packages are known to the compiler, in that we know about certain
entities that reside in these packages, and the compiler needs to
declare static Modules and Names that refer to these packages.  Hence
the wired-in packages can't include version numbers in their package UnitId,
since we don't want to bake the version numbers of these packages into GHC.

So here's the plan.  Wired-in packages are still versioned as
normal in the packages database, and you can still have multiple
versions of them installed. To the user, everything looks normal.

However, for each invocation of GHC, only a single instance of each wired-in
package will be recognised (the desired one is selected via
@-package@\/@-hide-package@), and GHC will internall pretend that it has the
*unversioned* 'UnitId', including in .hi files and object file symbols.

Unselected versions of wired-in packages will be ignored, as will any other
package that depends directly or indirectly on it (much as if you
had used @-ignore-package@).

The affected packages are compiled with, e.g., @-this-unit-id base@, so that
the symbols in the object files have the unversioned unit id in their name.

Make sure you change 'Packages.findWiredInPackages' if you add an entry here.

For `integer-gmp`/`integer-simple` we also change the base name to
`integer-wired-in`, but this is fundamentally no different.
See Note [The integer library] in PrelNames.



Note [Representation of module/name variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/basicTypes/Module.hs#L1115>`__

In our ICFP'16, we use <A> to represent module holes, and {A.T} to represent
name holes.  This could have been represented by adding some new cases
to the core data types, but this would have made the existing 'nameModule'
and 'moduleUnitId' partial, which would have required a lot of modifications
to existing code.

Instead, we adopted the following encoding scheme:

::

     <A>   ===> hole:A
     {A.T} ===> hole:A.T

This encoding is quite convenient, but it is also a bit dangerous too,
because if you have a 'hole:A' you need to know if it's actually a
'Module' or just a module stored in a 'Name'; these two cases must be
treated differently when doing substitutions.  'renameHoleModule'
and 'renameHoleUnitId' assume they are NOT operating on a
'Name'; 'NameShape' handles name substitutions exclusively.



Note [ModuleEnv performance and determinism]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/basicTypes/Module.hs#L1158>`__

To prevent accidental reintroduction of nondeterminism the Ord instance
for Module was changed to not depend on Unique ordering and to use the
lexicographic order. This is potentially expensive, but when measured
there was no difference in performance.

To be on the safe side and not pessimize ModuleEnv uses nondeterministic
ordering on Module and normalizes by doing the lexicographic sort when
turning the env to a list.
See Note [Unique Determinism] for more information about the source of
nondeterminismand and Note [Deterministic UniqFM] for explanation of why
it matters for maps.

