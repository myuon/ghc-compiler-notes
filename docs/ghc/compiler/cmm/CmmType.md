[[src]](https://github.com/ghc/ghc/tree/master/compiler/cmm/CmmType.hs)
### Note: Signed vs unsigned

Should a CmmType include a signed vs. unsigned distinction?

This is very much like a "hint" in C-- terminology: it isn't necessary
in order to generate correct code, but it might be useful in that the
compiler can generate better code if it has access to higher-level
hints about data.  This is important at call boundaries, because the
definition of a function is not visible at all of its call sites, so
the compiler cannot infer the hints.

Here in Cmm, we're taking a slightly different approach.  We include
the int vs. float hint in the CmmType, because (a) the majority of
platforms have a strong distinction between float and int registers,
and (b) we don't want to do any heavyweight hint-inference in the
native code backend in order to get good code.  We're treating the
hint more like a type: our Cmm is always completely consistent with
respect to hints.  All coercions between float and int are explicit.

What about the signed vs. unsigned hint?  This information might be
useful if we want to keep sub-word-sized values in word-size
registers, which we must do if we only have word-sized registers.

On such a system, there are two straightforward conventions for
representing sub-word-sized values:

(a) Leave the upper bits undefined.  Comparison operations must
    sign- or zero-extend both operands before comparing them,
    depending on whether the comparison is signed or unsigned.

(b) Always keep the values sign- or zero-extended as appropriate.
    Arithmetic operations must narrow the result to the appropriate
    size.

A clever compiler might not use either (a) or (b) exclusively, instead
it would attempt to minimize the coercions by analysis: the same kind
of analysis that propagates hints around.  In Cmm we don't want to
have to do this, so we plump for having richer types and keeping the
type information consistent.

If signed/unsigned hints are missing from CmmType, then the only
choice we have is (a), because we don't know whether the result of an
operation should be sign- or zero-extended.

Many architectures have extending load operations, which work well
with (b).  To make use of them with (a), you need to know whether the
value is going to be sign- or zero-extended by an enclosing comparison
(for example), which involves knowing above the context.  This is
doable but more complex.

Further complicating the issue is foreign calls: a foreign calling
convention can specify that signed 8-bit quantities are passed as
sign-extended 32 bit quantities, for example (this is the case on the
PowerPC).  So we *do* need sign information on foreign call arguments.

Pros for adding signed vs. unsigned to CmmType:

  - It would let us use convention (b) above, and get easier
    code generation for extending loads.

  - Less information required on foreign calls.

  - MachOp type would be simpler

Cons:

  - More complexity

  - What is the CmmType for a VanillaReg?  Currently it is
    always wordRep, but now we have to decide whether it is
    signed or unsigned.  The same VanillaReg can thus have
    different CmmType in different parts of the program.

  - Extra coercions cluttering up expressions.

Currently for GHC, the foreign call point is moot, because we do our
own promotion of sub-word-sized values to word-sized values.  The Int8
type is represented by an Int# which is kept sign-extended at all times
(this is slightly naughty, because we're making assumptions about the
C calling convention rather early on in the compiler).  However, given
this, the cons outweigh the pros.

