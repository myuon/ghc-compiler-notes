[[src]](https://github.com/ghc/ghc/tree/master/compiler/simplCore/CoreMonad.hs)

(c) The AQUA Project, Glasgow University, 1993-1998

# The core pipeline monad

# The CoreToDo type and related types
          Abstraction of core-to-core passes to run.




# Types for Plugins


# Counting and logging


# Monad and carried data structure definitions


# Core combinators, not exported


# Reader, writer and state accessors


# Dealing with annotations


### Note: Annotations

A Core-to-Core pass that wants to make use of annotations calls
getAnnotations or getFirstAnnotations at the beginning to obtain a UniqFM with
annotations of a specific type. This produces all annotations from interface
files read so far. However, annotations from interface files read during the
pass will not be visible until getAnnotations is called again. This is similar
to how rules work and probably isn't too bad.

The current implementation could be optimised a bit: when looking up
annotations for a thing from the HomePackageTable, we could search directly in
the module where the thing is defined rather than building one UniqFM which
contains all annotations we know of. This would work because annotations can
only be given to things defined in the same module. However, since we would
only want to deserialise every annotation once, we would have to build a cache
for every module in the HTP. In the end, it's probably not worth it as long as
we aren't using annotations heavily.

# Direct screen output


# Finding TyThings


# Template Haskell interoperability
