`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/main/FileCleanup.hs>`_

====================
compiler/main/FileCleanup.hs.rst
====================

Note [Deterministic base name]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The filename of temporary files, especially the basename of C files, can end
up in the output in some form, e.g. as part of linker debug information. In the
interest of bit-wise exactly reproducible compilation (#4012), the basename of
the temporary file no longer contains random information (it used to contain
the process id).

This is ok, as the temporary directory used contains the pid (see getTempDir).

