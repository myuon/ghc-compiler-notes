[[src]](https://github.com/ghc/ghc/tree/master/compiler/cmm/CmmSink.hs)
 TODO: enable this later, when we have some good tests in place to
   measure the effect and tune it.

-- small: an expression we don't mind duplicating
isSmall :: CmmExpr -> Bool
isSmall (CmmReg (CmmLocal _)) = True  --
isSmall (CmmLit _) = True
isSmall (CmmMachOp (MO_Add _) [x,y]) = isTrivial x && isTrivial y
isSmall (CmmRegOff (CmmLocal _) _) = True
isSmall _ = False


### Note: Improving conditionals

Given
  CmmCondBranch ((a >## b) != 1) t f
where a,b, are Floats, the constant folder /cannot/ turn it into
  CmmCondBranch (a <=## b) t f
because comparison on floats are not invertible
(see CmmMachOp.maybeInvertComparison).

What we want instead is simply to reverse the true/false branches thus
  CmmCondBranch ((a >## b) != 1) t f
-->
  CmmCondBranch (a >## b) f t

And we do that right here in tryToInline, just as we do cmmMachOpFold.


### Note: Inline GlobalRegs?

Should we freely inline GlobalRegs?

Actually it doesn't make a huge amount of difference either way, so we
*do* currently treat GlobalRegs as "trivial" and inline them
everywhere, but for what it's worth, here is what I discovered when I
(SimonM) looked into this:

Common sense says we should not inline GlobalRegs, because when we
have

  x = R1

the register allocator will coalesce this assignment, generating no
code, and simply record the fact that x is bound to $rbx (or
whatever).  Furthermore, if we were to sink this assignment, then the
range of code over which R1 is live increases, and the range of code
over which x is live decreases.  All things being equal, it is better
for x to be live than R1, because R1 is a fixed register whereas x can
live in any register.  So we should neither sink nor inline 'x = R1'.

However, not inlining GlobalRegs can have surprising
consequences. e.g. (cgrun020)

  c3EN:
      _s3DB::P64 = R1;
      _c3ES::P64 = _s3DB::P64 & 7;
      if (_c3ES::P64 >= 2) goto c3EU; else goto c3EV;
  c3EU:
      _s3DD::P64 = P64[_s3DB::P64 + 6];
      _s3DE::P64 = P64[_s3DB::P64 + 14];
      I64[Sp - 8] = c3F0;
      R1 = _s3DE::P64;
      P64[Sp] = _s3DD::P64;

inlining the GlobalReg gives:

  c3EN:
      if (R1 & 7 >= 2) goto c3EU; else goto c3EV;
  c3EU:
      I64[Sp - 8] = c3F0;
      _s3DD::P64 = P64[R1 + 6];
      R1 = P64[R1 + 14];
      P64[Sp] = _s3DD::P64;

but if we don't inline the GlobalReg, instead we get:

      _s3DB::P64 = R1;
      if (_s3DB::P64 & 7 >= 2) goto c3EU; else goto c3EV;
  c3EU:
      I64[Sp - 8] = c3F0;
      R1 = P64[_s3DB::P64 + 14];
      P64[Sp] = P64[_s3DB::P64 + 6];

This looks better - we managed to inline _s3DD - but in fact it
generates an extra reg-reg move:

.Lc3EU:
        movq $c3F0_info,-8(%rbp)
        movq %rbx,%rax
        movq 14(%rbx),%rbx
        movq 6(%rax),%rax
        movq %rax,(%rbp)

because _s3DB is now live across the R1 assignment, we lost the
benefit of coalescing.

Who is at fault here?  Perhaps if we knew that _s3DB was an alias for
R1, then we would not sink a reference to _s3DB past the R1
assignment.  Or perhaps we *should* do that - we might gain by sinking
it, despite losing the coalescing opportunity.

Sometimes not inlining global registers wins by virtue of the rule
about not inlining into arguments of a foreign call, e.g. (T7163) this
is what happens when we inlined F1:

      _s3L2::F32 = F1;
      _c3O3::F32 = %MO_F_Mul_W32(F1, 10.0 :: W32);
      (_s3L7::F32) = call "ccall" arg hints:  []  result hints:  [] rintFloat(_c3O3::F32);

but if we don't inline F1:

      (_s3L7::F32) = call "ccall" arg hints:  []  result hints:  [] rintFloat(%MO_F_Mul_W32(_s3L2::F32,
                                                                                            10.0 :: W32));
