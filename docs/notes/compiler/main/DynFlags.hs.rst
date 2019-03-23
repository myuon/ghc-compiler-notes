`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/main/DynFlags.hs>`_

Note [Updating flag description in the User's Guide]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If you modify anything in this file please make sure that your changes are
described in the User's Guide. Please update the flag description in the
users guide (docs/users_guide) whenever you add or change a flag.


Note [Supporting CLI completion]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The command line interface completion (in for example bash) is an easy way
for the developer to learn what flags are available from GHC.
GHC helps by separating which flags are available when compiling with GHC,
and which flags are available when using GHCi.
A flag is assumed to either work in both these modes, or only in one of them.
When adding or changing a flag, please consider for which mode the flag will
have effect, and annotate it accordingly. For Flags use defFlag, defGhcFlag,
defGhciFlag, and for FlagSpec use flagSpec or flagGhciSpec.


Note [Adding a language extension]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

There are a few steps to adding (or removing) a language extension,

 * Adding the extension to GHC.LanguageExtensions

   The Extension type in libraries/ghc-boot-th/GHC/LanguageExtensions/Type.hs
   is the canonical list of language extensions known by GHC.

 * Adding a flag to DynFlags.xFlags

   This is fairly self-explanatory. The name should be concise, memorable,
   and consistent with any previous implementations of the similar idea in
   other Haskell compilers.

 * Adding the flag to the documentation

   This is the same as any other flag. See
   Note [Updating flag description in the User's Guide]

 * Adding the flag to Cabal

   The Cabal library has its own list of all language extensions supported
   by all major compilers. This is the list that user code being uploaded
   to Hackage is checked against to ensure language extension validity.
   Consequently, it is very important that this list remains up-to-date.

   To this end, there is a testsuite test (testsuite/tests/driver/T4437.hs)
   whose job it is to ensure these GHC's extensions are consistent with
   Cabal.

   The recommended workflow is,

    1. Temporarily add your new language extension to the
       expectedGhcOnlyExtensions list in T4437 to ensure the test doesn't
       break while Cabal is updated.

    2. After your GHC change is accepted, submit a Cabal pull request adding
       your new extension to Cabal's list (found in
       Cabal/Language/Haskell/Extension.hs).

    3. After your Cabal change is accepted, let the GHC developers know so
       they can update the Cabal submodule and remove the extensions from
       expectedGhcOnlyExtensions.

 * Adding the flag to the GHC Wiki

   There is a change log tracking language extension additions and removals
   on the GHC wiki:  https://ghc.haskell.org/trac/ghc/wiki/LanguagePragmaHistory

 See #4437 and #8176.
-----------------------------------------------------------------------------
DynFlags


Note [Verbosity levels]
~~~~~~~~~~~~~~~~~~~~~~~
    0   |   print errors & warnings only
    1   |   minimal verbosity: print "compiling M ... done." for each module.
    2   |   equivalent to -dshow-passes
    3   |   equivalent to existing "ghc -v"
    4   |   "ghc -v -ddump-most"
    5   |   "ghc -v -ddump-all"


Note [When is StarIsType enabled]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The StarIsType extension determines whether to treat '*' as a regular type
operator or as a synonym for 'Data.Kind.Type'. Many existing pre-TypeInType
programs expect '*' to be synonymous with 'Type', so by default StarIsType is
enabled.

Programs that use TypeOperators might expect to repurpose '*' for
multiplication or another binary operation, but making TypeOperators imply
NoStarIsType caused too much breakage on Hackage.



Note [Documenting optimisation flags]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If you change the list of flags enabled for particular optimisation levels
please remember to update the User's Guide. The relevant file is:

  docs/users_guide/using-optimisation.rst

Make sure to note whether a flag is implied by -O0, -O or -O2.


Note [Eta-reduction in -O0]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#11562 showed an example which tripped an ASSERT in CoreToStg; a
function was marked as MayHaveCafRefs when in fact it obviously
didn't.  Reason was:
 * Eta reduction wasn't happening in the simplifier, but it was
   happening in CorePrep, on
        $fBla = MkDict (/\a. K a)
 * Result: rhsIsStatic told TidyPgm that $fBla might have CAF refs
   but the eta-reduced version (MkDict K) obviously doesn't
Simple solution: just let the simplifier do eta-reduction even in -O0.
After all, CorePrep does it unconditionally!  Not a big deal, but
removes an assertion failure. -----------------------------------------------------------------------------
Standard sets of warning options


Note [Documenting warning flags]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If you change the list of warning enabled by default
please remember to update the User's Guide. The relevant file is:

 docs/users_guide/using-warnings.rst


Note [No PIE while linking]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
As of 2016 some Linux distributions (e.g. Debian) have started enabling -pie by
default in their gcc builds. This is incompatible with -r as it implies that we
are producing an executable. Consequently, we must manually pass -no-pie to gcc
when joining object files or linking dynamic libraries. Unless, of course, the
user has explicitly requested a PIE executable with -pie. See #12759.


Note [DynFlags consistency]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

There are a number of number of DynFlags configurations which either
do not make sense or lead to unimplemented or buggy codepaths in the
compiler. makeDynFlagsConsistent is responsible for verifying the validity
of a set of DynFlags, fixing any issues, and reporting them back to the
caller.

GHCi and -O
---------------

When using optimization, the compiler can introduce several things
(such as unboxed tuples) into the intermediate code, which GHCi later
chokes on since the bytecode interpreter can't handle this (and while
this is arguably a bug these aren't handled, there are no plans to fix
it.)

While the driver pipeline always checks for this particular erroneous
combination when parsing flags, we also need to check when we update
the flags; this is because API clients may parse flags but update the
DynFlags afterwords, before finally running code inside a session (see
T10052 and #10052).

