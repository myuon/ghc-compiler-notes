[[src]](https://github.com/ghc/ghc/tree/master/compiler/main/Ar.hs)
 Note: [The need for Ar.hs]
Building `-staticlib` required the presence of libtool, and was a such
restricted to mach-o only. As libtool on macOS and gnu libtool are very
different, there was no simple portable way to support this.

libtool for static archives does essentially: concatinate the input archives,
add the input objects, and create a symbol index. Using `ar` for this task
fails as even `ar` (bsd and gnu, llvm, ...) do not provide the same
features across platforms (e.g. index prefixed retrieval of objects with
the same name.)

As Archives are rather simple structurally, we can just build the archives
with Haskell directly and use ranlib on the final result to get the symbol
index. This should allow us to work around with the differences/abailability
of libtool across differet platforms.
