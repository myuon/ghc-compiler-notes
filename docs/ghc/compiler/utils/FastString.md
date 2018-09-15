[[src]](https://github.com/ghc/ghc/tree/master/compiler/utils/FastString.hs)
# LANGUAGE BangPatterns, CPP, MagicHash, UnboxedTuples,
    GeneralizedNewtypeDeriving #

|
A 'FastString' is an array of bytes, hashed to support fast O(1)
comparison.  It is also associated with a character encoding, so that
we know how to convert a 'FastString' to the local encoding, or to the
Z-encoding used by the compiler internally.

'FastString's support a memoized conversion to the Z-encoding via zEncodeFS.



Internally, the compiler will maintain a fast string symbol table, providing
sharing and fast comparison. Creation of new @FastString@s then covertly does a
lookup, re-using the @FastString@ if there was a hit.

The design of the FastString hash table allows for lockless concurrent reads
and updates to multiple buckets with low synchronization overhead.

### Note: Updating the FastString table



We include the FastString table in the `sharedCAF` mechanism because we'd like
FastStrings created by a Core plugin to have the same uniques as corresponding
strings created by the host compiler itself.  For example, this allows plugins
to lookup known names (eg `mkTcOcc "MySpecialType"`) in the GlobalRdrEnv or
even re-invoke the parser.

In particular, the following little sanity test was failing in a plugin
prototyping safe newtype-coercions: GHC.NT.Type.NT was imported, but could not
be looked up /by the plugin/.

   let rdrName = mkModuleName "GHC.NT.Type" `mkRdrQual` mkTcOcc "NT"
   putMsgS $ showSDoc dflags $ ppr $ lookupGRE_RdrName rdrName $ mg_rdr_env guts

`mkTcOcc` involves the lookup (or creation) of a FastString.  Since the
plugin's FastString.string_table is empty, constructing the RdrName also
allocates new uniques for the FastStrings "GHC.NT.Type" and "NT".  These
uniques are almost certainly unequal to the ones that the host compiler
originally assigned to those FastStrings.  Thus the lookup fails since the
domain of the GlobalRdrEnv is affected by the RdrName's OccName's FastString's
unique.

Maintaining synchronization of the two instances of this global is rather
difficult because of the uses of `unsafePerformIO` in this module.  Not
synchronizing them risks breaking the rather major invariant that two
FastStrings with the same unique have the same string. Thus we use the
lower-level `sharedCAF` mechanism that relies on Globals.c.



### Note: Updating the FastString table

The procedure goes like this:

1. Read the relevant bucket and perform a look up of the string.
2. If it exists, return it.
3. Otherwise grab a unique ID, create a new FastString and atomically attempt
   to update the relevant bucket with this FastString:

   * Double check that the string is not in the bucket. Another thread may have
     inserted it while we were creating our string.
   * Return the existing FastString if it exists. The one we preemptively
     created will get GCed.
   * Otherwise, insert and return the string we created.


### Note: Double-checking the bucket

It is not necessary to check the entire bucket the second time. We only have to
check the strings that are new to the bucket since the last time we read it.


#UNPACK#

#UNPACK#