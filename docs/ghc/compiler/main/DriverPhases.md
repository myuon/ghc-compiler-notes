[[src]](https://github.com/ghc/ghc/tree/master/compiler/main/DriverPhases.hs)

   Phase of the           | Suffix saying | Flag saying   | (suffix of)
   compilation system     | ``start here''| ``stop after''| output file

   literate pre-processor | .lhs          | -             | -
   C pre-processor (opt.) | -             | -E            | -
   Haskell compiler       | .hs           | -C, -S        | .hc, .s
   C compiler (opt.)      | .hc or .c     | -S            | .s
   assembler              | .s  or .S     | -c            | .o
   linker                 | other         | -             | a.out


### Note: Partial ordering on phases

We want to know which phases will occur before which others. This is used for
sanity checking, to ensure that the pipeline will stop at some point (see
DriverPipeline.runPipeline).

A < B iff A occurs before B in a normal compilation pipeline.

There is explicitly not a total ordering on phases, because in registerised
builds, the phase `HsC` doesn't happen before nor after any other phase.

Although we check that a normal user doesn't set the stop_phase to HsC through
use of -C with registerised builds (in Main.checkOptions), it is still
possible for a ghc-api user to do so. So be careful when using the function
happensBefore, and don't think that `not (a <= b)` implies `b < a`.
