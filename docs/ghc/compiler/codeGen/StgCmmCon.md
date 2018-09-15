[[src]](https://github.com/ghc/ghc/tree/master/compiler/codeGen/StgCmmCon.hs)
 We used to pass a boolean indicating whether all the
args were of size zero, so we could use a static
constructor; but I concluded that it just isn't worth it.
Now I/O uses unboxed tuples there just aren't any constructors
with all size-zero args.

The reason for having a separate argument, rather than looking at
the addr modes of the args is that we may be in a "knot", and
premature looking at the args will cause the compiler to black-hole!


 The following three paragraphs about @Char@-like and @Int@-like
closures are obsolete, but I don't understand the details well enough
to properly word them, sorry. I've changed the treatment of @Char@s to
be analogous to @Int@s: only a subset is preallocated, because @Char@
has now 31 bits. Only literals are handled here. -- Qrczak

Now for @Char@-like closures.  We generate an assignment of the
address of the closure to a temporary.  It would be possible simply to
generate no code, and record the addressing mode in the environment,
but we'd have to be careful if the argument wasn't a constant --- so
for simplicity we just always assign to a temporary.

Last special case: @Int@-like closures.  We only special-case the
situation in which the argument is a literal in the range
@mIN_INTLIKE@..@mAX_INTLILKE@.  NB: for @Char@-like closures we can
work with any old argument, but for @Int@-like ones the argument has
to be a literal.  Reason: @Char@ like closures have an argument type
which is guaranteed in range.

Because of this, we use can safely return an addressing mode.

We don't support this optimisation when compiling into Windows DLLs yet
because they don't support cross package data references well.
