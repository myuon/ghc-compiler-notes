[[src]](https://github.com/ghc/ghc/tree/master/compiler/utils/Outputable.hs)

(c) The University of Glasgow 2006-2012
(c) The GRASP Project, Glasgow University, 1992-1998


# \subsection{The @PprStyle@ data type}



Orthogonal to the above printing styles are (possibly) some
command-line flags that affect printing (often carried with the
style).  The most likely ones are variations on how much type info is
shown.

The following test decides whether or not we are actually generating
code (either C or assembly), or generating interface files.

# \subsection{The @SDoc@ data type}


# \subsection[Outputable-class]{The @Outputable@ class}


# \subsection{The @OutputableBndr@ class}


# \subsection{Random printing helpers}


# \subsection{Other helper functions}


# \subsection{Printing numbers verbally}


# \subsection{Error handling}
