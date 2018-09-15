[[src]](https://github.com/ghc/ghc/tree/master/compiler/deSugar/DsGRHSs.hs)

(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998


Matching guarded right-hand-sides (GRHSs)



@dsGuarded@ is used for both @case@ expressions and pattern bindings.
It desugars:
\begin{verbatim}
        | g1 -> e1
        ...
        | gn -> en
        where binds
\end{verbatim}
producing an expression with a runtime error in the corner if
necessary.  The type argument gives the type of the @ei@.


# matchGuard : make a MatchResult from a guarded RHS                  


Should {\em fail} if @e@ returns @D@
\begin{verbatim}
f x | p <- e', let C y# = e, f y# = r1
    | otherwise          = r2
\end{verbatim}
