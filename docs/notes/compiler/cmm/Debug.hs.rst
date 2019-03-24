`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/cmm/Debug.hs>`_

Note [What is this unwinding business?]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Unwinding tables are a variety of debugging information used by debugging tools
to reconstruct the execution history of a program at runtime. These tables
consist of sets of "instructions", one set for every instruction in the program,
which describe how to reconstruct the state of the machine at the point where
the current procedure was called. For instance, consider the following annotated
pseudo-code,

.. code-block:: haskell

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

.. code-block:: haskell

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

.. code-block:: haskell

  entry()
     { c2fe:
           unwind Sp = Just Sp + 0;
           v :: P64 = R2;
           if ((Sp + 8) - 32 < SpLim) (likely: False) goto c2ff; else goto c2fg;

After c2fe we may pass to either c2ff or c2fg; let's first consider the
former. In this case there is nothing in particular that we need to do other
than reiterate what we already know about Sp,

.. code-block:: haskell

       c2ff:
           unwind Sp = Just Sp + 0;
           R2 = v :: P64;
           R1 = test_closure;
           call (stg_gc_fun)(R2, R1) args: 8, res: 0, upd: 8;

In contrast, c2fg updates Sp midway through its body. To ensure that unwinding
can happen correctly after this point we must include an unwind statement there,
in addition to the usual beginning-of-block statement,

.. code-block:: haskell

       c2fg:
           unwind Sp = Just Sp + 0;
           I64[Sp - 8] = c2dD;
           R1 = v :: P64;
           Sp = Sp - 8;
           unwind Sp = Just Sp + 8;
           if (R1 & 7 != 0) goto c2dD; else goto c2dE;

The remaining blocks are simple,

.. code-block:: haskell

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

 * These DebugBlocks are then converted to, e.g., DWARF unwinding tables
   (by the Dwarf module) and emitted in the final object.

See also:
  Note [Unwinding information in the NCG] in AsmCodeGen,
  Note [Unwind pseudo-instruction in Cmm],
  Note [Debugging DWARF unwinding info].




Note [Debugging DWARF unwinding info]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

For debugging generated unwinding info I've found it most useful to dump the
disassembled binary with objdump -D and dump the debug info with
readelf --debug-dump=frames-interp.

You should get something like this:

  0000000000000010 <stg_catch_frame_info>:
    10:   48 83 c5 18             add    $0x18,%rbp
    14:   ff 65 00                jmpq   *0x0(%rbp)

and:

.. code-block:: haskell

  Contents of the .debug_frame section:

.. code-block:: haskell

  00000000 0000000000000014 ffffffff CIE "" cf=1 df=-8 ra=16
     LOC           CFA      rbp   rsp   ra
  0000000000000000 rbp+0    v+0   s     c+0

.. code-block:: haskell

  00000018 0000000000000024 00000000 FDE cie=00000000 pc=000000000000000f..0000000000000017
     LOC           CFA      rbp   rsp   ra
  000000000000000f rbp+0    v+0   s     c+0
  000000000000000f rbp+24   v+0   s     c+0

To read it http://www.dwarfstd.org/doc/dwarf-2.0.0.pdf has a nice example in
Appendix 5 (page 101 of the pdf) and more details in the relevant section.

The key thing to keep in mind is that the value at LOC is the value from
*before* the instruction at LOC executes. In other words it answers the
question: if my $rip is at LOC, how do I get the relevant values given the
values obtained through unwinding so far.

If the readelf --debug-dump=frames-interp output looks wrong, it may also be
useful to look at readelf --debug-dump=frames, which is closer to the
information that GHC generated.

It's also useful to dump the relevant Cmm with -ddump-cmm -ddump-opt-cmm
-ddump-cmm-proc -ddump-cmm-verbose. Note [Unwind pseudo-instruction in Cmm]
explains how to interpret it.

Inside gdb there are a couple useful commands for inspecting frames.
For example:

.. code-block:: haskell

  gdb> info frame <num>

It shows the values of registers obtained through unwinding.

Another useful thing to try when debugging the DWARF unwinding is to enable
extra debugging output in GDB:

.. code-block:: haskell

  gdb> set debug frame 1

This makes GDB produce a trace of its internal workings. Having gone this far,
it's just a tiny step to run GDB in GDB. Make sure you install debugging
symbols for gdb if you obtain it through a package manager.

Keep in mind that the current release of GDB has an instruction pointer handling
heuristic that works well for C-like languages, but doesn't always work for
Haskell. See Note [Info Offset] in Dwarf.Types for more details.



Note [Unwind pseudo-instruction in Cmm]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

One of the possible CmmNodes is a CmmUnwind pseudo-instruction. It doesn't
generate any assembly, but controls what DWARF unwinding information gets
generated.

It's important to understand what ranges of code the unwind pseudo-instruction
refers to.
For a sequence of CmmNodes like:

.. code-block:: haskell

  A // starts at addr X and ends at addr Y-1
  unwind Sp = Just Sp + 16;
  B // starts at addr Y and ends at addr Z

the unwind statement reflects the state after A has executed, but before B
has executed. If you consult the Note [Debugging DWARF unwinding info], the
LOC this information will end up in is Y.

