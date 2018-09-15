[[src]](https://github.com/ghc/ghc/tree/master/compiler/basicTypes/Literal.hs)

(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1998

# @Literal@: Machine literals (unboxed, of course)

# \subsection{Literals}


### Note: Integer literals

An Integer literal is represented using, well, an Integer, to make it
easier to write RULEs for them. They also contain the Integer type, so
that e.g. literalType can return the right Type for them.

They only get converted into real Core,
    mkInteger [c1, c2, .., cn]
during the CorePrep phase, although TidyPgm looks ahead at what the
core will be, so that it can see whether it involves CAFs.

When we initally build an Integer literal, notably when
deserialising it from an interface file (see the Binary instance
below), we don't have convenient access to the mkInteger Id.  So we
just use an error thunk, and fill in the real Id when we do tcIfaceLit
in TcIface.


Binary instance



        Construction
        ~~~~~~~~~~~~



### Note: Word/Int underflow/overflow

According to the Haskell Report 2010 (Sections 18.1 and 23.1 about signed and
unsigned integral types): "All arithmetic is performed modulo 2^n, where n is
the number of bits in the type."

GHC stores Word# and Int# constant values as Integer. Core optimizations such
as constant folding must ensure that the Integer value remains in the valid
target Word/Int range (see #13172). The following functions are used to
ensure this.

Note that we *don't* warn the user about overflow. It's not done at runtime
either, and compilation of completely harmless things like
   ((124076834 :: Word32) + (2147483647 :: Word32))
doesn't yield a warning. Instead we simply squash the value into the *target*
Int/Word range.



        Coercions
        ~~~~~~~~~




        Predicates
        ~~~~~~~~~~




        Types
        ~~~~~




        Comparison
        ~~~~~~~~~~




        Printing
        ~~~~~~~~

### Note: Printing of literals in Core

### Note: Printing of literals in Core

The function `add_par` is used to wrap parenthesis around negative integers
(`LitInteger`) and labels (`MachLabel`), if they occur in a context requiring
an atomic thing (for example function application).

Although not all Core literals would be valid Haskell, we are trying to stay
as close as possible to Haskell syntax in the printing of Core, to make it
easier for a Haskell user to read Core.

To that end:
  * We do print parenthesis around negative `LitInteger`, because we print
  `LitInteger` using plain number literals (no prefix or suffix), and plain
  number literals in Haskell require parenthesis in contexts like function
  application (i.e. `1 - -1` is not valid Haskell).

  * We don't print parenthesis around other (negative) literals, because they
  aren't needed in GHC/Haskell either (i.e. `1# -# -1#` is accepted by GHC's
  parser).

Literal         Output             Output if context requires
                                   an atom (if different)
-------         -------            ----------------------
MachChar        'a'#
MachStr         "aaa"#
MachNullAddr    "__NULL"
MachInt         -1#
MachInt64       -1L#
MachWord         1##
MachWord64       1L##
MachFloat       -1.0#
MachDouble      -1.0##
LitInteger      -1                 (-1)
MachLabel       "__label" ...      ("__label" ...)
