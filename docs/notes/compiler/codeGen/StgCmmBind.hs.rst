`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/codeGen/StgCmmBind.hs>`_

compiler/codeGen/StgCmmBind.hs
==============================


Note [Selectors]
~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/codeGen/StgCmmBind.hs#L233>`__

We look at the body of the closure to see if it's a selector---turgid,
but nothing deep.  We are looking for a closure of {\em exactly} the
form:

...  = [the_fv] \ u [] ->
         case the_fv of
           con a_1 ... a_n -> a_i



Note [Ap thunks]
~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/codeGen/StgCmmBind.hs#L243>`__

A more generic AP thunk of the form

::

        x = [ x_1...x_n ] \.. [] -> x_1 ... x_n

..

A set of these is compiled statically into the RTS, so we just use
those.  We could extend the idea to thunks where some of the x_i are
global ids (and hence not free variables), but this would entail
generating a larger thunk.  It might be an option for non-optimising
compilation, though.

We only generate an Ap thunk if all the free variables are pointers,
for semi-obvious reasons.


-------- Note [Selectors] ------------------



Note [NodeReg clobbered with loopification]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/codeGen/StgCmmBind.hs#L511>`__

Previously we used to pass nodeReg (aka R1) here. With profiling, upon
entering a closure, enterFunCCS was called with R1 passed to it. But since R1
may get clobbered inside the body of a closure, and since a self-recursive
tail call does not restore R1, a subsequent call to enterFunCCS received a
possibly bogus value from R1. The solution is to not pass nodeReg (aka R1) to
enterFunCCS. Instead, we pass node, the callee-saved temporary that stores
the original value of R1. This way R1 may get modified but loopification will
not care.

A function closure pointer may be tagged, so we
must take it into account when accessing the free variables.

