`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/main/DriverPipeline.hs>`_

compiler/main/DriverPipeline.hs
===============================


Note [-Xlinker -rpath vs -Wl,-rpath]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/main/DriverPipeline.hs#L1569>`__

-Wl takes a comma-separated list of options which in the case of
-Wl,-rpath -Wl,some,path,with,commas parses the path with commas
as separate options.
Buck, the build system, produces paths with commas in them.

-Xlinker doesn't have this disadvantage and as far as I can tell
it is supported by both gcc and clang. Anecdotally nvcc supports
-Xlinker, but not -Wl.



Note [Produce big objects on Windows]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/main/DriverPipeline.hs#L2041>`__

The Windows Portable Executable object format has a limit of 32k sections, which
we tend to blow through pretty easily. Thankfully, there is a "big object"
extension, which raises this limit to 2^32. However, it must be explicitly
enabled in the toolchain:

 * the assembler accepts the -mbig-obj flag, which causes it to produce a
   bigobj-enabled COFF object.

 * the linker accepts the --oformat pe-bigobj-x86-64 flag. Despite what the name
   suggests, this tells the linker to produce a bigobj-enabled COFF object, no a
   PE executable.

We must enable bigobj output in a few places:

 * When merging object files (DriverPipeline.joinObjectFiles)

 * When assembling (DriverPipeline.runPhase (RealPhase As ...))

Unfortunately the big object format is not supported on 32-bit targets so
none of this can be used in that case.

