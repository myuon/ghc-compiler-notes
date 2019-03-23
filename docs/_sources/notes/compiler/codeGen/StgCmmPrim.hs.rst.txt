Note [Comparing stable names]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A StableName# is actually a pointer to a stable name object (SNO)
containing an index into the stable name table (SNT). We
used to compare StableName#s by following the pointers to the
SNOs and checking whether they held the same SNT indices. However,
this is not necessary: there is a one-to-one correspondence
between SNOs and entries in the SNT, so simple pointer equality
does the trick.
These primops are implemented by CallishMachOps, because they sometimes
turn into foreign calls depending on the backend.
