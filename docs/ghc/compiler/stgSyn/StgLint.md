[[src]](https://github.com/ghc/ghc/tree/master/compiler/stgSyn/StgLint.hs)

(c) The GRASP/AQUA Project, Glasgow University, 1993-1998

# A ``lint'' pass to check for Stg correctness


Checks for
        (a) *some* type errors
        (b) locally-defined variables used but not defined


Note: unless -dverbose-stg is on, display of lint errors will result
in "panic: bOGUS_LVs".

WARNING:
~~~~~~~~


This module has suffered bit-rot; it is likely to yield lint errors
for Stg code that is currently perfectly acceptable for code
generation.  Solution: don't use it!  (KSW 2000-05).

# \subsection{``lint'' for various constructs}


@lintStgTopBindings@ is the top-level interface function.


# \subsection[lint-monad]{The Lint monad}



Checking function applications: we only check that the type has the
right *number* of arrows, we don't actually compare the types.  This
is because we can't expect the types to be equal - the type
applications and type lambdas that we use to calculate accurate types
have long since disappeared.
