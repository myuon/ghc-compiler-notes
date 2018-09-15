[[src]](https://github.com/ghc/ghc/tree/master/compiler/nativeGen/PIC.hs)

  This module handles generation of position independent code and
  dynamic-linking related issues for the native code generator.

  This depends both the architecture and OS, so we define it here
  instead of in one of the architecture specific modules.

  Things outside this module which are related to this:

  + module CLabel
    - PIC base label (pretty printed as local label 1)
    - DynamicLinkerLabels - several kinds:
        CodeStub, SymbolPtr, GotSymbolPtr, GotSymbolOffset
    - labelDynamic predicate
  + module Cmm
    - The GlobalReg datatype has a PicBaseReg constructor
    - The CmmLit datatype has a CmmLabelDiffOff constructor
  + codeGen & RTS
    - When tablesNextToCode, no absolute addresses are stored in info tables
      any more. Instead, offsets from the info label are used.
    - For Win32 only, SRTs might contain addresses of __imp_ symbol pointers
      because Win32 doesn't support external references in data sections.
      TODO: make sure this still works, it might be bitrotted
  + NCG
    - The cmmToCmm pass in AsmCodeGen calls cmmMakeDynamicReference for all
      labels.
    - nativeCodeGen calls pprImportedSymbol and pprGotDeclaration to output
      all the necessary stuff for imported symbols.
    - The NCG monad keeps track of a list of imported symbols.
    - MachCodeGen invokes initializePicBase to generate code to initialize
      the PIC base register when needed.
    - MachCodeGen calls cmmMakeDynamicReference whenever it uses a CLabel
      that wasn't in the original Cmm code (e.g. floating point literals).



We would like to do approximately this, but spill slot allocation
might be added before the first BasicBlock. That violates the ABI.

For now we will emit the prologue code in the pretty printer,
which is also what we do for ELF v1.
initializePicBase_ppc (ArchPPC_64 ELF_V2) OSLinux picReg
        (CmmProc info lab live (ListGraph (entry:blocks)) : statics)
        = do
           bID <-getUniqueM
           return (CmmProc info lab live (ListGraph (b':entry:blocks))
                                         : statics)
        where   BasicBlock entryID _ = entry
                b' = BasicBlock bID [PPC.FETCHTOC picReg lab,
                                     PPC.BCC PPC.ALWAYS entryID]
