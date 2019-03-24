`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/cmm/SMRep.hs>`_

====================
compiler/cmm/SMRep.hs.rst
====================

Note [static constructors]
~~~~~~~~~~~~~~~~~~~~~~~~~~

We used to have a CONSTR_STATIC closure type, and each constructor had
two info tables: one with 1 (or 2 etc.), and one with
CONSTR_STATIC.

This distinction was removed, because when copying a data structure
into a compact region, we must copy static constructors into the
compact region too.  If we didn't do this, we would need to track the
references from the compact region out to the static constructors,
because they might (indirectly) refer to CAFs.

Since static constructors will be copied to the heap, if we wanted to
use different info tables for static and dynamic constructors, we
would have to switch the info pointer when copying the constructor
into the compact region, which means we would need an extra field of
the static info table to point to the dynamic one.

However, since the distinction between static and dynamic closure
types is never actually needed (other than for assertions), we can
just drop the distinction and use the same info table for both.

The GC *does* need to distinguish between static and dynamic closures,
but it does this using the HEAP_ALLOCED() macro which checks whether
the address of the closure resides within the dynamic heap.
HEAP_ALLOCED() doesn't read the closure's info table.



Note [Static NoCaf constructors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If we know that a top-level binding 'x' is not Caffy (ie no CAFs are
reachable from 'x'), then a statically allocated constructor (Just x)
is also not Caffy, and the garbage collector need not follow its
argument fields.  Exploiting this would require two static info tables
for Just, for the two cases where the argument was Caffy or non-Caffy.

Currently we don't do this; instead we treat nullary constructors
as non-Caffy, and the others as potentially Caffy.



