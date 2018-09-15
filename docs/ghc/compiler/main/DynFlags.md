[[src]](https://github.com/ghc/ghc/tree/master/compiler/main/DynFlags.hs)
 It would be desirable to have the more generalised

  instance (MonadTrans t, Monad m, HasDynFlags m) => HasDynFlags (t m) where
      getDynFlags = lift getDynFlags

instance definition. However, that definition would overlap with the
`HasDynFlags (GhcT m)` instance. Instead we define instances for a
couple of common Monad transformers explicitly. 

### Note: Verbosity levels

    0   |   print errors & warnings only
    1   |   minimal verbosity: print "compiling M ... done." for each module.
    2   |   equivalent to -dshow-passes
    3   |   equivalent to existing "ghc -v"
    4   |   "ghc -v -ddump-most"
    5   |   "ghc -v -ddump-all"


# 

# 


 - Below we export user facing symbols for GHC dynamic flags for use with the
 - GHC API.
 

### Note: Eta-reduction in -O0

Trac #11562 showed an example which tripped an ASSERT in CoreToStg; a
function was marked as MayHaveCafRefs when in fact it obviously
didn't.  Reason was:
 * Eta reduction wasn't happening in the simplifier, but it was
   happening in CorePrep, on
        $fBla = MkDict (/\a. K a)
 * Result: rhsIsStatic told TidyPgm that $fBla might have CAF refs
   but the eta-reduced version (MkDict K) obviously doesn't
Simple solution: just let the simplifier do eta-reduction even in -O0.
After all, CorePrep does it unconditionally!  Not a big deal, but
removes an assertion failure. 

# 

### Note: No PIE while linking

As of 2016 some Linux distributions (e.g. Debian) have started enabling -pie by
default in their gcc builds. This is incompatible with -r as it implies that we
are producing an executable. Consequently, we must manually pass -no-pie to gcc
when joining object files or linking dynamic libraries. Unless, of course, the
user has explicitly requested a PIE executable with -pie. See #12759.


### Note: DynFlags consistency


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
