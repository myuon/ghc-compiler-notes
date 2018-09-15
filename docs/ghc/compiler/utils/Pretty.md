[[src]](https://github.com/ghc/ghc/tree/master/compiler/utils/Pretty.hs)
### Note: Differences between libraries/pretty and compiler/utils/Pretty.hs

For historical reasons, there are two different copies of `Pretty` in the GHC
source tree:
 * `libraries/pretty` is a submodule containing
   https://github.com/haskell/pretty. This is the `pretty` library as released
   on hackage. It is used by several other libraries in the GHC source tree
   (e.g. template-haskell and Cabal).
 * `compiler/utils/Pretty.hs` (this module). It is used by GHC only.

There is an ongoing effort in https://github.com/haskell/pretty/issues/1 and
https://ghc.haskell.org/trac/ghc/ticket/10735 to try to get rid of GHC's copy
of Pretty.

Currently, GHC's copy of Pretty resembles pretty-1.1.2.0, with the following
major differences:
 * GHC's copy uses `Faststring` for performance reasons.
 * GHC's copy has received a backported bugfix for #12227, which was
   released as pretty-1.1.3.4 ("Remove harmful $! forcing in beside",
   https://github.com/haskell/pretty/pull/35).

Other differences are minor. Both copies define some extra functions and
instances not defined in the other copy. To see all differences, do this in a
ghc git tree:

    $ cd libraries/pretty
    $ git checkout v1.1.2.0
    $ cd -
    $ vimdiff compiler/utils/Pretty.hs \
              libraries/pretty/src/Text/PrettyPrint/HughesPJ.hs

For parity with `pretty-1.1.2.1`, the following two `pretty` commits would
have to be backported:
  * "Resolve foldr-strictness stack overflow bug"
    (307b8173f41cd776eae8f547267df6d72bff2d68)
  * "Special-case reduce for horiz/vert"
    (c57c7a9dfc49617ba8d6e4fcdb019a3f29f1044c)
This has not been done sofar, because these commits seem to cause more
allocation in the compiler (see thomie's comments in
https://github.com/haskell/pretty/pull/9).


# Laws for $$

<a1>    (x $$ y) $$ z   = x $$ (y $$ z)
<a2>    empty $$ x      = x
<a3>    x $$ empty      = x

        ...ditto $+$...

# for <>

<b1>    (x <> y) <> z   = x <> (y <> z)
<b2>    empty <> x      = empty
<b3>    x <> empty      = x

        ...ditto <+>...

# for text

<t1>    text s <> text t        = text (s++t)
<t2>    text "" <> x            = x, if x non-empty

** because of law n6, t2 only holds if x doesn't
** start with `nest'.

# Laws for nest

<n1>    nest 0 x                = x
<n2>    nest k (nest k' x)      = nest (k+k') x
<n3>    nest k (x <> y)         = nest k x <> nest k y
<n4>    nest k (x $$ y)         = nest k x $$ nest k y
<n5>    nest k empty            = empty
<n6>    x <> nest k y           = x <> y, if x non-empty

** Note the side condition on <n6>!  It is this that
** makes it OK for empty to be a left unit for <>.

Miscellaneous
~~~~~~~~~~~~~

<m1>    (text s <> x) $$ y = text s <> ((text "" <> x) $$
                                         nest (-length s) y)

<m2>    (x $$ y) <> z = x $$ (y <> z)
        if y non-empty

# Laws for list versions

<l1>    sep (ps++[empty]++qs)   = sep (ps ++ qs)
        ...ditto hsep, hcat, vcat, fill...

<l2>    nest k (sep ps) = sep (map (nest k) ps)
        ...ditto hsep, hcat, vcat, fill...

# for oneLiner

<o1>    oneLiner (nest k p) = nest k (oneLiner p)
<o2>    oneLiner (x <> y)   = oneLiner x <> oneLiner y

You might think that the following verion of <m1> would
be neater:

<3 NO>  (text s <> x) $$ y = text s <> ((empty <> x)) $$
                                         nest (-length s) y)

But it doesn't work, for if x=empty, we would have

        text s $$ y = text s <> (empty $$ nest (-length s) y)
                    = text s <> nest (-length s) y



Here are the invariants:

1) The argument of NilAbove is never Empty. Therefore
   a NilAbove occupies at least two lines.

2) The argument of @TextBeside@ is never @Nest@.

3) The layouts of the two arguments of @Union@ both flatten to the same
   string.

4) The arguments of @Union@ are either @TextBeside@, or @NilAbove@.

5) A @NoDoc@ may only appear on the first line of the left argument of an
   union. Therefore, the right argument of an union can never be equivalent
   to the empty set (@NoDoc@).

6) An empty document is always represented by @Empty@.  It can't be
   hidden inside a @Nest@, or a @Union@ of two @Empty@s.

7) The first line of every layout in the left argument of @Union@ is
   longer than the first line of any layout in the right argument.
   (1) ensures that the left argument has a first line.  In view of
   (3), this invariant means that the right argument must have at
   least two lines.

Notice the difference between
   * NoDoc (no documents)
   * Empty (one empty document; no height and no width)
   * text "" (a document containing the empty string;
              one line high, but has no width)


#UNPACK #

# RULES
  "text/str" forall a. text (unpackCString# a) = ptext (Ptr a)
 #


Q: What is the reason for negative indentation (i.e. argument to indent
   is < 0) ?

A:
This indicates an error in the library client's code.
If we compose a <> b, and the first line of b is more indented than some
other lines of b, the law <n6> (<> eats nests) may cause the pretty
printer to produce an invalid layout:

doc       |0123345
------------------
d1        |a...|
d2        |...b|
          |c...|

d1<>d2    |ab..|
         c|....|

Consider a <> b, let `s' be the length of the last line of `a', `k' the
indentation of the first line of b, and `k0' the indentation of the
left-most line b_i of b.

The produced layout will have negative indentation if `k - k0 > s', as
the first line of b will be put on the (s+1)th column, effectively
translating b horizontally by (k-s). Now if the i^th line of b has an
indentation k0 < (k-s), it is translated out-of-page, causing
`negative indentation'.
