[[src]](https://github.com/ghc/ghc/tree/master/compiler/simplStg/StgStats.hs)

(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

# Gathers statistical information about programs


The program gather statistics about
\begin{enumerate}
\item number of boxed cases
\item number of unboxed cases
\item number of let-no-escapes
\item number of non-updatable lets
\item number of updatable lets
\item number of applications
\item number of primitive applications
\item number of closures (does not include lets bound to constructors)
\item number of free variables in closures
%\item number of top-level functions
%\item number of top-level CAFs
\item number of constructors
\end{enumerate}


True<=>top-level

ditto

ditto

ditto

# \subsection{Top-level list of bindings (a ``program'')}


# \subsection{Bindings}


# \subsection{Expressions}


not top-level

not top-level