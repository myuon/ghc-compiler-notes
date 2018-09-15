[[src]](https://github.com/ghc/ghc/tree/master/compiler/utils/Util.hs)
# \subsection{Is DEBUG on, are we on Windows, etc?}


These booleans are global constants, set by CPP flags.  They allow us to
recompile a single module (this one) to change whether or not debug output
appears. They sometimes let us avoid even running CPP elsewhere.

It's important that the flags are literal constants (True/False). Then,
with -0, tests of the flags in other modules will simplify to the correct
branch of the conditional, thereby dropping debug code altogether when
the flags are off.


# \subsection{A for loop}


# \subsection[Utils-lists]{General list processing}



A paranoid @zip@ (and some @zipWith@ friends) that checks the lists
are of equal length.  Alastair Reid thinks this should only happen if
DEBUGging on; hey, why not?


# \subsubsection{Sort utils}


# \subsection[Utils-transitive-closure]{Transitive closure}


This algorithm for transitive closure is straightforward, albeit quadratic.


# \subsection[Utils-accum]{Accumulating}


A combination of foldl with zip.  It works with equal length lists.



@splitAt@, @take@, and @drop@ but with length of another
list giving the break-off point:


# \subsection[Utils-comparison]{Comparisons}


# \subsection{Edit distance}


# SPECIALIZE INLINE restrictedDamerauLevenshteinDistance'
                      :: Word32 -> Int -> Int -> String -> String -> Int #

# SPECIALIZE INLINE restrictedDamerauLevenshteinDistance'
                      :: Integer -> Int -> Int -> String -> String -> Int #

# SPECIALIZE restrictedDamerauLevenshteinDistanceWorker
               :: IM.IntMap Word32 -> Word32 -> Word32
               -> (Word32, Word32, Word32, Word32, Int)
               -> Char -> (Word32, Word32, Word32, Word32, Int) #

# SPECIALIZE restrictedDamerauLevenshteinDistanceWorker
               :: IM.IntMap Integer -> Integer -> Integer
               -> (Integer, Integer, Integer, Integer, Int)
               -> Char -> (Integer, Integer, Integer, Integer, Int) #

# \subsection[Utils-pairs]{Pairs}


# Globals and the RTS


When a plugin is loaded, it currently gets linked against a *newly
loaded* copy of the GHC package. This would not be a problem, except
that the new copy has its own mutable state that is not shared with
that state that has already been initialized by the original GHC
package.

(Note that if the GHC executable was dynamically linked this
wouldn't be a problem, because we could share the GHC library it
links to; this is only a problem if DYNAMIC_GHC_PROGRAMS=NO.)

The solution is to make use of @sharedCAF@ through @sharedGlobal@
for globals that are shared between multiple copies of ghc packages.



Akin to @Prelude.words@, but acts like the Bourne shell, treating
quoted strings as Haskell Strings, and also parses Haskell [String]
syntax.



-- -----------------------------------------------------------------------------
-- Floats


# \subsection[Utils-Data]{Utils for defining Data instances}


These functions helps us to define Data instances for abstract types.


# \subsection[Utils-C]{Utils for printing C code}


# \subsection[Utils-Hashing]{Utils for hashing}
