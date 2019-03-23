`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/utils/ghc-pkg/Main.hs>`_

Note [Recompute abi-depends]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Like most fields, `ghc-pkg` relies on who-ever is performing package
registration to fill in fields; this includes the `abi-depends` field present
for the package.

However, this was likely a mistake, and is not very robust; in certain cases,
versions of Cabal may use bogus abi-depends fields for a package when doing
builds. Why? Because package database information is aggressively cached; it is
possible to work Cabal into a situation where it uses a cached version of
`abi-depends`, rather than the one in the actual database after it has been
recomputed.

However, there is an easy fix: ghc-pkg /already/ knows the `abi-depends` of a
package, because they are the ABIs of the packages pointed at by the `depends`
field. So it can simply look up the abi from the dependencies in the original
database, and ignore whatever the system registering gave it.

So, instead, we do two things here:

  - We throw away the information for a registered package's `abi-depends` field.

  - We recompute it: we simply look up the unit ID of the package in the original
    database, and use *its* abi-depends.

See #14381, and Cabal issue #4728.

Additionally, because we are throwing away the original (declared) ABI deps, we
return a boolean that indicates whether any abi-depends were actually
overridden.


