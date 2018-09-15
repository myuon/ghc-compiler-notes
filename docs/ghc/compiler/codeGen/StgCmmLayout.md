[[src]](https://github.com/ghc/ghc/tree/master/compiler/codeGen/StgCmmLayout.hs)
### Note: over-saturated calls

The natural thing to do for an over-saturated call would be to call
the function with the correct number of arguments, and then apply the
remaining arguments to the value returned, e.g.

  f a b c d   (where f has arity 2)
  -->
  r = call f(a,b)
  call r(c,d)

but this entails
  - saving c and d on the stack
  - making a continuation info table
  - at the continuation, loading c and d off the stack into regs
  - finally, call r

Note that since there are a fixed number of different r's
(e.g.  stg_ap_pp_fast), we can also pre-compile continuations
that correspond to each of them, rather than generating a fresh
one for each over-saturated call.

Not only does this generate much less code, it is faster too.  We will
generate something like:

Sp[old+16] = c
Sp[old+24] = d
Sp[old+32] = stg_ap_pp_info
call f(a,b) -- usual calling convention

For the purposes of the CmmCall node, we count this extra stack as
just more arguments that we are passing on the stack (cml_args).
