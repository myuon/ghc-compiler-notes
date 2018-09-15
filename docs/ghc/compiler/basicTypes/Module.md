[[src]](https://github.com/ghc/ghc/tree/master/compiler/basicTypes/Module.hs)

(c) The University of Glasgow, 2004-2006

# Module

Simply the name of a module, represented as a FastString.
These are Uniquable, hence we can build Maps with Modules as
the keys.


# \subsection{Module locations}



For a module in another package, the hs_file and obj_file
components of ModLocation are undefined.

The locations specified by a ModLocation may or may not
correspond to actual files yet: for example, even if the object
file doesn't exist, the ModLocation still contains the path to
where the object file will reside if/when it is created.


# \subsection{The name of a module}


# \subsection{A fully qualified module}


# \subsection{ComponentId}


# \subsection{UnitId}


# Hole substitutions


# \subsection{@ModuleEnv@s}


### Note: ModuleEnv performance and determinism

To prevent accidental reintroduction of nondeterminism the Ord instance
for Module was changed to not depend on Unique ordering and to use the
lexicographic order. This is potentially expensive, but when measured
there was no difference in performance.

### Note: Unique Determinism


A ModuleName has a Unique, so we can build mappings of these using
UniqFM.
