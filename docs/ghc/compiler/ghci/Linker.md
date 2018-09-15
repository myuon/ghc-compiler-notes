[[src]](https://github.com/ghc/ghc/tree/master/compiler/ghci/Linker.hs)
# 

                        The Linker's state

# 


The persistent linker state *must* match the actual state of the
C dynamic linker at all times, so we keep it in a private global variable.

The global IORef used for PersistentLinkerState actually contains another MVar.
The reason for this is that we want to allow another loaded copy of the GHC
library to side-effect the PLS and for those changes to be reflected here.

The PersistentLinkerState maps Names to actual closures (for
interpreted code only), for use during linking.


# 

                        Initialisation

# 

### Note: preload packages

Why do we need to preload packages from the command line?  This is an
explanation copied from #2437:

I tried to implement the suggestion from #3560, thinking it would be
easy, but there are two reasons we link in packages eagerly when they
are mentioned on the command line:

  * So that you can link in extra object files or libraries that
    depend on the packages. e.g. ghc -package foo -lbar where bar is a
    C library that depends on something in foo. So we could link in
    foo eagerly if and only if there are extra C libs or objects to
    link in, but....

  * Haskell code can depend on a C function exported by a package, and
    the normal dependency tracking that TH uses can't know about these
    dependencies. The test ghcilink004 relies on this, for example.

I conclude that we need two -package flags: one that says "this is a
package I want to make available", and one that says "this is a
package I want to link in eagerly". Would that be too complicated for
users?


# 

                        Link a byte-code expression

# 

# 

              Loading a Decls statement

# 

# 

              Loading a single module

# 

# 

                Link some linkables
        The linkables may consist of a mixture of
        byte-code modules and object modules

# 

# 

                The object-code linker

# 

# 

                The byte-code linker

# 

# 

                Unload some object modules

# 

# 

                Loading packages

# 

# 

                Helper functions

# 