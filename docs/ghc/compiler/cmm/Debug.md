[[src]](https://github.com/ghc/ghc/tree/master/compiler/cmm/Debug.hs)
### Note: What is this unwinding business?


Unwinding tables are a variety of debugging information used by debugging tools
to reconstruct the execution history of a program at runtime. These tables
consist of sets of "instructions", one set for every instruction in the program,
which describe how to reconstruct the state of the machine at the point where
the current procedure was called. For instance, consider the following annotated
pseudo-code,

  a_fun:
    add rsp, 8            -- unwind: rsp = rsp - 8
    mov rax, 1            -- unwind: rax = unknown
    call another_block
    sub rsp, 8            -- unwind: rsp = rsp

We see that attached to each instruction there is an "unwind" annotation, which
provides a relationship between each updated register and its value at the
time of entry to a_fun. This is the sort of information that allows gdb to give
you a stack backtrace given the execution state of your program. This
unwinding information is captured in various ways by various debug information
formats; in the case of DWARF (the only format supported by GHC) it is known as
Call Frame Information (CFI) and can be found in the .debug.frames section of
your object files.

Currently we only bother to produce unwinding information for registers which
are necessary to reconstruct flow-of-execution. On x86_64 this includes $rbp
(which is the STG stack pointer) and $rsp (the C stack pointer).

Let's consider how GHC would annotate a C-- program with unwinding information
with a typical C-- procedure as would come from the STG-to-Cmm code generator,

  entry()
     { c2fe:
           v :: P64 = R2;
           if ((Sp + 8) - 32 < SpLim) (likely: False) goto c2ff; else goto c2fg;
       c2ff:
           R2 = v :: P64;
           R1 = test_closure;
           call (stg_gc_fun)(R2, R1) args: 8, res: 0, upd: 8;
       c2fg:
           I64[Sp - 8] = c2dD;
           R1 = v :: P64;
           Sp = Sp - 8;          // Sp updated here
           if (R1 & 7 != 0) goto c2dD; else goto c2dE;
       c2dE:
           call (I64[R1])(R1) returns to c2dD, args: 8, res: 8, upd: 8;
       c2dD:
           w :: P64 = R1;
           Hp = Hp + 48;
           if (Hp > HpLim) (likely: False) goto c2fj; else goto c2fi;
       ...
  },

Let's consider how this procedure will be decorated with unwind information
(largely by CmmLayoutStack). Naturally, when we enter the procedure `entry` the
value of Sp is no different from what it was at its call site. Therefore we will
add an `unwind` statement saying this at the beginning of its unwind-annotated
code,

  entry()
     { c2fe:
           unwind Sp = Just Sp + 0;
           v :: P64 = R2;
           if ((Sp + 8) - 32 < SpLim) (likely: False) goto c2ff; else goto c2fg;

After c2fe we we may pass to either c2ff or c2fg; let's first consider the
former. In this case there is nothing in particular that we need to do other
than reiterate what we already know about Sp,

       c2ff:
           unwind Sp = Just Sp + 0;
           R2 = v :: P64;
           R1 = test_closure;
           call (stg_gc_fun)(R2, R1) args: 8, res: 0, upd: 8;

In contrast, c2fg updates Sp midway through its body. To ensure that unwinding
can happen correctly after this point we must include an unwind statement there,
in addition to the usual beginning-of-block statement,

       c2fg:
           unwind Sp = Just Sp + 0;
           I64[Sp - 8] = c2dD;
           R1 = v :: P64;
           unwind Sp = Just Sp + 8;
           Sp = Sp - 8;
           if (R1 & 7 != 0) goto c2dD; else goto c2dE;

The remaining blocks are simple,

       c2dE:
           unwind Sp = Just Sp + 8;
           call (I64[R1])(R1) returns to c2dD, args: 8, res: 8, upd: 8;
       c2dD:
           unwind Sp = Just Sp + 8;
           w :: P64 = R1;
           Hp = Hp + 48;
           if (Hp > HpLim) (likely: False) goto c2fj; else goto c2fi;
       ...
  },


The flow of unwinding information through the compiler is a bit convoluted:

 * C-- begins life in StgCmm without any unwind information. This is because we
   haven't actually done any register assignment or stack layout yet, so there
   is no need for unwind information.

 * CmmLayoutStack figures out how to layout each procedure's stack, and produces
   appropriate unwinding nodes for each adjustment of the STG Sp register.

 * The unwind nodes are carried through the sinking pass. Currently this is
   guaranteed not to invalidate unwind information since it won't touch stores
   to Sp, but this will need revisiting if CmmSink gets smarter in the future.

 * Eventually we make it to the native code generator backend which can then
   preserve the unwind nodes in its machine-specific instructions. In so doing
   the backend can also modify or add unwinding information; this is necessary,
   for instance, in the case of x86-64, where adjustment of $rsp may be
   necessary during calls to native foreign code due to the native calling
   convention.

 * The NCG then retrieves the final unwinding table for each block from the
   backend with extractUnwindPoints.

 * This unwind information is converted to DebugBlocks by Debug.cmmDebugGen

 * These DebugBlcosk are then converted to, e.g., DWARF unwinding tables
   (by the Dwarf module) and emitted in the final object.

### Note: Unwinding information in the NCG