[[src]](https://github.com/ghc/ghc/tree/master/compiler/nativeGen/X86/Regs.hs)

Intel x86 architecture:
- All registers except 7 (esp) are available for use.
- Only ebx, esi, edi and esp are available across a C call (they are callee-saves).
- Registers 0-7 have 16-bit counterparts (ax, bx etc.)
- Registers 0-3 have 8 bit counterparts (ah, bh etc.)
- Registers fake0..fake5 are fakes; we pretend x86 has 6 conventionally-addressable
  fp registers, and 3-operand insns for them, and we translate this into
  real stack-based x86 fp code after register allocation.

The fp registers are all Double registers; we don't have any RcFloat class
regs.  @regClass@ barfs if you give it a VirtualRegF, and mkVReg above should
never generate them.



AMD x86_64 architecture:
- All 16 integer registers are addressable as 8, 16, 32 and 64-bit values:

  8     16    32    64
  ---------------------
  al    ax    eax   rax
  bl    bx    ebx   rbx
  cl    cx    ecx   rcx
  dl    dx    edx   rdx
  sil   si    esi   rsi
  dil   si    edi   rdi
  bpl   bp    ebp   rbp
  spl   sp    esp   rsp
  r10b  r10w  r10d  r10
  r11b  r11w  r11d  r11
  r12b  r12w  r12d  r12
  r13b  r13w  r13d  r13
  r14b  r14w  r14d  r14
  r15b  r15w  r15d  r15



eax = rax
ebx = rbx
ecx = rcx
edx = rdx
esi = rsi
edi = rdi
ebp = rbp
esp = rsp
