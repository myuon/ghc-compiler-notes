[[src]](https://github.com/ghc/ghc/tree/master/compiler/iface/MkIface.hs)

(c) The University of Glasgow 2006-2008
(c) The GRASP/AQUA Project, Glasgow University, 1993-1998



  -----------------------------------------------
          Recompilation checking
  -----------------------------------------------

A complete description of how recompilation checking works can be
found in the wiki commentary:

 http://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/RecompilationAvoidance

Please read the above page for a top-down description of how this all
works.  Notes below cover specific issues related to the implementation.

Basic idea:

  * In the mi_usages information in an interface, we record the
    fingerprint of each free variable of the module

  * In mkIface, we compute the fingerprint of each exported thing A.f.
    For each external thing that A.f refers to, we include the fingerprint
    of the external reference when computing the fingerprint of A.f.  So
    if anything that A.f depends on changes, then A.f's fingerprint will
    change.
    Also record any dependent files added with
      * addDependentFile
      * #include
      * -optP-include

  * In checkOldIface we compare the mi_usages for the module with
    the actual fingerprint for all each thing recorded in mi_usages


# \subsection{Completing an interface}


### Note: Fingerprinting IfaceDecls


The general idea here is that we first examine the 'IfaceDecl's and determine
the recursive groups of them. We then walk these groups in dependency order,
serializing each contained 'IfaceDecl' to a "Binary" buffer which we then
hash using MD5 to produce a fingerprint for the group.

However, the serialization that we use is a bit funny: we override the @putName@
operation with our own which serializes the hash of a 'Name' instead of the
'Name' itself. This ensures that the fingerprint of a decl changes if anything
in its transitive closure changes. This trick is why we must be careful about
traversing in dependency order: we need to ensure that we have hashes for
everything referenced by the decl which we are fingerprinting.

Moreover, we need to be careful to distinguish between serialization of binding
Names (e.g. the ifName field of a IfaceDecl) and non-binding (e.g. the ifInstCls
field of a IfaceClsInst): only in the non-binding case should we include the
fingerprint; in the binding case we shouldn't since it is merely the name of the
thing that we are currently fingerprinting.


# The ABI of an IfaceDecl


### Note: The ABI of an IfaceDecl

The ABI of a declaration consists of:

   (a) the full name of the identifier (inc. module and package,
       because these are used to construct the symbol name by which
       the identifier is known externally).

   (b) the declaration itself, as exposed to clients.  That is, the
       definition of an Id is included in the fingerprint only if
       it is made available as an unfolding in the interface.

   (c) the fixity of the identifier (if it exists)
   (d) for Ids: rules
   (e) for classes: instances, fixity & rules for methods
   (f) for datatypes: instances, fixity & rules for constrs

Items (c)-(f) are not stored in the IfaceDecl, but instead appear
elsewhere in the interface file.  But they are *fingerprinted* with
the declaration itself. This is done by grouping (c)-(f) in IfaceDeclExtras,
and fingerprinting that as part of the declaration.



-- for testing: use the md5sum command to generate fingerprints and
-- compare the results against our built-in version.
  fp' <- oldMD5 dflags bh
  if fp /= fp' then pprPanic "computeFingerprint" (ppr fp <+> ppr fp')
               else return fp

oldMD5 dflags bh = do
  tmp <- newTempName dflags CurrentModule "bin"
  writeBinMem bh tmp
  tmp2 <- newTempName dflags CurrentModule "md5"
  let cmd = "md5sum " ++ tmp ++ " >" ++ tmp2
  r <- system cmd
  case r of
    ExitFailure _ -> throwGhcExceptionIO (PhaseFailed cmd r)
    ExitSuccess -> do
        hash_str <- readFile tmp2
        return $! readHexFingerprint hash_str


# COMPLETE Pragmas


# Keeping track of what we've slurped, and fingerprints


### Note: Original module

Consider this:
        module X where { data family T }
        module Y( T(..) ) where { import X; data instance T Int = MkT Int }
The exported Avail from Y will look like
        X.T{X.T, Y.MkT}
That is, in Y,
  - only MkT is brought into scope by the data instance;
  - but the parent (used for grouping and naming in T(..) exports) is X.T
  - and in this case we export X.T too

In the result of MkIfaceExports, the names are grouped by defining module,
so we may need to split up a single Avail into multiple ones.

### Note: Internal used_names

### Note: Binders in Template Haskell

# Load the old interface file for this module (unless
        we have it already), and check whether it is up to date


# Converting things to their Iface equivalents
