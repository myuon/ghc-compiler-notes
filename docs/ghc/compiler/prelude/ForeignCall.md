[[src]](https://github.com/ghc/ghc/tree/master/compiler/prelude/ForeignCall.hs)

(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

# Foreign calls

# \subsubsection{Data types}


# \subsubsection{Calling C}



Stuff to do with calling convention:

ccall:          Caller allocates parameters, *and* deallocates them.

stdcall:        Caller allocates parameters, callee deallocates.
                Function name has @N after it, where N is number of arg bytes
                e.g.  _Foo@8. This convention is x86 (win32) specific.

See: http://www.programmersheaven.com/2/Calling-conventions



Generate the gcc attribute corresponding to the given
calling convention (used by PprAbsC):


" <> ppr cconv <> text "

\# CTYPE'@,
--        'ApiAnnotation.AnnHeader','ApiAnnotation.AnnVal',
--        'ApiAnnotation.AnnClose' @'\#

# CTYPE") <+> hDoc
        <+> pprWithSourceText stct (doubleQuotes (ftext ct)) <+> text "#

# \subsubsection{Misc}
