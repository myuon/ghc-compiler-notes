[[src]](https://github.com/ghc/ghc/tree/master/compiler/llvmGen/LlvmCodeGen/Regs.hs)
 Need to make sure the names here can't conflict with the unique generated
   names. Uniques generated names containing only base62 chars. So using say
   the '_' char guarantees this.
