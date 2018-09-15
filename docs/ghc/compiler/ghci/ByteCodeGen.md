[[src]](https://github.com/ghc/ghc/tree/master/compiler/ghci/ByteCodeGen.hs)
### Note: generating code for top-level string literal bindings

Here is a summary on how the byte code generator deals with top-level string
literals:

1. Top-level string literal bindings are separated from the rest of the module.

2. The strings are allocated via iservCmd, in allocateTopStrings

3. The mapping from binders to allocated strings (topStrings) are maintained in
   BcM and used when generating code for variable references.



ppBCEnv :: BCEnv -> SDoc
ppBCEnv p
   = text "begin-env"
     $$ nest 4 (vcat (map pp_one (sortBy cmp_snd (Map.toList p))))
     $$ text "end-env"
     where
        pp_one (var, offset) = int offset <> colon <+> ppr var <+> ppr (bcIdArgRep var)
        cmp_snd x y = compare (snd x) (snd y)


no bitmap

not alts

 No free variables 


   | trace (showSDoc (
              (char ' '
               $$ (ppr.filter (not.isTyVar).dVarSetElems.fst) rhs
               $$ pprCoreExpr (deAnnotate rhs)
               $$ char ' '
              ))) False
   = undefined
   | otherwise


not alts

not an unboxed tuple


   Ticked Expressions
   ------------------

  The idea is that the "breakpoint<n,fvs> E" is really just an annotation on
  the code. When we find such a thing, we pull out the useful information,
  and then compile the code as if it was just the expression E.



shouldn't really happen

size

no arity

is alts


         Because the Haskell stack grows down, the a_reps refer to
         lowest to highest addresses in that order.  The args for the call
         are on the stack.  Now push an unboxed Addr# indicating
         the C function to call.  Then push a dummy placeholder for the
         result.  Finally, emit a CCALL insn with an offset pointing to the
         Addr# just pushed, and a literal field holding the mallocville
         address of the piece of marshalling code we generate.
         So, just prior to the CCALL insn, the stack looks like this
         (growing down, as usual):

            <arg_n>
            ...
            <arg_1>
            Addr# address_of_C_fn
            <placeholder-for-result#> (must be an unboxed type)

         The interpreter then calls the marshall code mentioned
         in the CCALL insn, passing it (& <placeholder-for-result#>),
         that is, the addr of the topmost word in the stack.
         When this returns, the placeholder will have been
         filled in.  The placeholder is slid down to the sequel
         depth, and we RETURN.

         This arrangement makes it simple to do f-i-dynamic since the Addr#
         value is the first arg anyway.

         The marshalling code is generated specifically for this
         call site, and so knows exactly the (Haskell) stack
         offsets of the args, fn address and placeholder.  It
         copies the args to the C stack, calls the stacked addr,
         and parks the result back in the placeholder.  The interpreter
         calls it as a normal C call, assuming it has a signature
            void marshall_code ( StgWord* ptr_to_top_of_stack )
         

### Note: Implementing tagToEnum#

(implement_tagToId arg names) compiles code which takes an argument
'arg', (call it i), and enters the i'th closure in the supplied list
as a consequence.  The [Name] is a list of the constructors of this
(enumeration) type.

The code we generate is this:
                push arg
                push bogus-word

                TESTEQ_I 0 L1
                  PUSH_G <lbl for first data con>
                  JMP L_Exit

        L1:     TESTEQ_I 1 L2
                  PUSH_G <lbl for second data con>
                  JMP L_Exit
        ...etc...
        Ln:     TESTEQ_I n L_fail
                  PUSH_G <lbl for last data con>
                  JMP L_Exit

        L_fail: CASEFAIL

        L_exit: SLIDE 1 n
                ENTER

The 'bogus-word' push is because TESTEQ_I expects the top of the stack
to have an info-table, and the next word to have the value to be
tested.  This is very weird, but it's the way it is right now.  See
Interpreter.c.  We don't acutally need an info-table here; we just
need to have the argument to be one-from-top on the stack, hence pushing
a 1-word null. See Trac #8383.
