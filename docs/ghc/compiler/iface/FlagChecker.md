[[src]](https://github.com/ghc/ghc/tree/master/compiler/iface/FlagChecker.hs)
### Note: path flags and recompilation

There are several flags that we deliberately omit from the
recompilation check; here we explain why.

-osuf, -odir, -hisuf, -hidir
  If GHC decides that it does not need to recompile, then
  it must have found an up-to-date .hi file and .o file.
  There is no point recording these flags - the user must
  have passed the correct ones.  Indeed, the user may
  have compiled the source file in one-shot mode using
  -o to specify the .o file, and then loaded it in GHCi
  using -odir.

-stubdir
  We omit this one because it is automatically set by -outputdir, and
  we don't want changes in -outputdir to automatically trigger
  recompilation.  This could be wrong, but only in very rare cases.

-i (importPaths)
  For the same reason as -osuf etc. above: if GHC decides not to
  recompile, then it must have already checked all the .hi files on
  which the current module depends, so it must have found them
  successfully.  It is occasionally useful to be able to cd to a
  different directory and use -i flags to enable GHC to find the .hi
  files; we don't want this to force recompilation.

The only path-related flag left is -hcsuf.


### Note: Ignoring some flag changes


Normally, --make tries to reuse only compilation products that are
the same as those that would have been produced compiling from
scratch. Sometimes, however, users would like to be more aggressive
about recompilation avoidance. This is particularly likely when
developing using GHCi (see #13604). Currently, we allow users to
ignore optimisation changes using -fignore-optim-changes, and to
ignore HPC option changes using -fignore-hpc-changes. If there's a
demand for it, we could also allow changes to -fprof-auto-* flags
(although we can't allow -prof flags to differ). The key thing about
these options is that we can still successfully link a library or
executable when some of its components differ in these ways.

The way we accomplish this is to leave the optimization and HPC
options out of the flag hash, hashing them separately.
