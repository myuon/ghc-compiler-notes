`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/basicTypes/Literal.hs>`_

compiler/basicTypes/Literal.hs
==============================


Note [Integer literals]
~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/basicTypes/Literal.hs#L170>`__

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



Note [Natural literals]
~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/basicTypes/Literal.hs#L187>`__

Similar to Integer literals.



Note [String literals]
~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/basicTypes/Literal.hs#L191>`__

String literals are UTF-8 encoded and stored into ByteStrings in the following
ASTs: Haskell, Core, Stg, Cmm. TH can also emit ByteString based string literals
with the BytesPrimL constructor (see #14741).

It wasn't true before as [Word8] was used in Cmm AST and in TH which was quite
bad for performance with large strings (see #16198 and #14741).

To include string literals into output objects, the assembler code generator has
to embed the UTF-8 encoded binary blob. See Note [Embedding large binary blobs]
for more details.



Note [Word/Int underflow/overflow]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/basicTypes/Literal.hs#L290>`__

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



Note [Printing of literals in Core]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/basicTypes/Literal.hs#L744>`__

The function `add_par` is used to wrap parenthesis around negative integers
(`LitInteger`) and labels (`LitLabel`), if they occur in a context requiring
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
LitChar         'a'#
LitString       "aaa"#
LitNullAddr     "__NULL"
LitInt          -1#
LitInt64        -1L#
LitWord          1##
LitWord64        1L##
LitFloat        -1.0#
LitDouble       -1.0##
LitInteger      -1                 (-1)
LitLabel        "__label" ...      ("__label" ...)
LitRubbish      "__RUBBISH"



Note [Rubbish literals]
~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/basicTypes/Literal.hs#L780>`__

During worker/wrapper after demand analysis, where an argument
is unused (absent) we do the following w/w split (supposing that
y is absent):

  f x y z = e
===>
  f x y z = $wf x z
  $wf x z = let y = <absent value>
            in e

Usually the binding for y is ultimately optimised away, and
even if not it should never be evaluated -- but that's the
way the w/w split starts off.

What is <absent value>?
* For lifted values <absent value> can be a call to 'error'.
* For primitive types like Int# or Word# we can use any random
  value of that type.
* But what about /unlifted/ but /boxed/ types like MutVar# or
  Array#?   We need a literal value of that type.

That is 'LitRubbish'.  Since we need a rubbish literal for
many boxed, unlifted types, we say that LitRubbish has type
  LitRubbish :: forall (a :: TYPE UnliftedRep). a

So we might see a w/w split like
  $wf x z = let y :: Array# Int = LitRubbish @(Array# Int)
            in e

Recall that (TYPE UnliftedRep) is the kind of boxed, unlifted
heap pointers.

Here are the moving parts:

* We define LitRubbish as a constructor in Literal.Literal

* It is given its polymoprhic type by Literal.literalType

* WwLib.mk_absent_let introduces a LitRubbish for absent
  arguments of boxed, unlifted type.

* In CoreToSTG we convert (RubishLit @t) to just ().  STG is
  untyped, so it doesn't matter that it points to a lifted
  value. The important thing is that it is a heap pointer,
  which the garbage collector can follow if it encounters it.

::

  We considered maintaining LitRubbish in STG, and lowering
  it in the code genreators, but it seems simpler to do it
  once and for all in CoreToSTG.

..

::

  In ByteCodeAsm we just lower it as a 0 literal, because
  it's all boxed and lifted to the host GC anyway.

..

