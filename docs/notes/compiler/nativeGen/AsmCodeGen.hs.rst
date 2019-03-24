`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/nativeGen/AsmCodeGen.hs>`_

====================
compiler/nativeGen/AsmCodeGen.hs.rst
====================

Note [Unwinding information in the NCG]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Unwind information is a type of metadata which allows a debugging tool
to reconstruct the values of machine registers at the time a procedure was
entered. For the most part, the production of unwind information is handled by
the Cmm stage, where it is represented by CmmUnwind nodes.

Unfortunately, the Cmm stage doesn't know everything necessary to produce
accurate unwinding information. For instance, the x86-64 calling convention
requires that the stack pointer be aligned to 16 bytes, which in turn means that
GHC must sometimes add padding to $sp prior to performing a foreign call. When
this happens unwind information must be updated accordingly.
For this reason, we make the NCG backends responsible for producing
unwinding tables (with the extractUnwindPoints function in NcgImpl).

We accumulate the produced unwind tables over CmmGroups in the ngs_unwinds
field of NativeGenAcc. This is a label map which contains an entry for each
procedure, containing a list of unwinding points (e.g. a label and an associated
unwinding table).

See also Note [What is this unwinding business?] in Debug.

