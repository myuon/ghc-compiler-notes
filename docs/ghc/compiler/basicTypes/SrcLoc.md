[[src]](https://github.com/ghc/ghc/tree/master/compiler/basicTypes/SrcLoc.hs)
# \subsection[SrcLoc-SrcLocations]{Source-location information}


We keep information about the {\em definition} point for each entity;
this is the obvious stuff:


# \subsection[SrcLoc-access-fns]{Access functions}


# \subsection[SrcLoc-instances]{Instance declarations for various names}


# LINE ", int src_line, space,
--                  char '\"', pprFastFilePath src_path, text " #

# \subsection[SrcSpan]{Source Spans}


 |
A 'RealSrcSpan' delimits a portion of a text file.  It could be represented
by a pair of (line,column) coordinates, but in fact we optimise
slightly by using more compact representations for single-line and
zero-length spans, both of which are quite common.

The end position is defined to be the column /after/ the end of the
span.  That is, a span of (1,1)-(1,2) is one character long, and a
span of (1,1)-(1,1) is zero characters long.


# \subsection[SrcSpan-predicates]{Predicates}


# 

# \subsection[SrcSpan-access-fns]{Access functions}


# \subsection[SrcSpan-instances]{Instances}


# LINE ", int (srcSpanStartLine span), space,
--                 char '\"', pprFastFilePath $ srcSpanFile span, text " #

# \subsection[Located]{Attaching SrcSpans to things}


# \subsection{Ordering SrcSpans for InteractiveUI}
