Note [Unique Determinism]
~~~~~~~~~~~~~~~~~~~~~~~~~
The order of allocated @Uniques@ is not stable across rebuilds.
The main reason for that is that typechecking interface files pulls
@Uniques@ from @UniqSupply@ and the interface file for the module being
currently compiled can, but doesn't have to exist.

It gets more complicated if you take into account that the interface
files are loaded lazily and that building multiple files at once has to
work for any subset of interface files present. When you add parallelism
this makes @Uniques@ hopelessly random.

As such, to get deterministic builds, the order of the allocated
@Uniques@ should not affect the final result.
see also wiki/DeterministicBuilds



Note [Unique Determinism and code generation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The goal of the deterministic builds (wiki/DeterministicBuilds, #4012)
is to get ABI compatible binaries given the same inputs and environment.
The motivation behind that is that if the ABI doesn't change the
binaries can be safely reused.
Note that this is weaker than bit-for-bit identical binaries and getting
bit-for-bit identical binaries is not a goal for now.
This means that we don't care about nondeterminism that happens after
the interface files are created, in particular we don't care about
register allocation and code generation.
To track progress on bit-for-bit determinism see #12262.


Note [No Ord for Unique]
~~~~~~~~~~~~~~~~~~~~~~~~~~
As explained in Note [Unique Determinism] the relative order of Uniques
is nondeterministic. To prevent from accidental use the Ord Unique
instance has been removed.
This makes it easier to maintain deterministic builds, but comes with some
drawbacks.
The biggest drawback is that Maps keyed by Uniques can't directly be used.
The alternatives are:

  1) Use UniqFM or UniqDFM, see Note [Deterministic UniqFM] to decide which
  2) Create a newtype wrapper based on Unique ordering where nondeterminism
     is controlled. See Module.ModuleEnv
  3) Change the algorithm to use nonDetCmpUnique and document why it's still
     deterministic
  4) Use TrieMap as done in CmmCommonBlockElim.groupByLabel
