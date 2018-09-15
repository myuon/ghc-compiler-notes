[[src]](https://github.com/ghc/ghc/tree/master/compiler/simplCore/SimplMonad.hs)

(c) The AQUA Project, Glasgow University, 1993-1998

# The simplifier Monad

# \subsection{Monad plumbing}


For the simplifier monad, we want to {\em thread} a unique supply and a counter.
(Command-line switches move around through the explicitly-passed SimplEnv.)


# \subsection{The unique supply}


# \subsection{Counting up what we've done}
