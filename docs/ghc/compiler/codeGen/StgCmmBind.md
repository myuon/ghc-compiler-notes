[[src]](https://github.com/ghc/ghc/tree/master/compiler/codeGen/StgCmmBind.hs)
### Note: cgBind rec

   Recursive let-bindings are tricky.
   Consider the following pseudocode:

     let x = \_ ->  ... y ...
         y = \_ ->  ... z ...
         z = \_ ->  ... x ...
     in ...

   For each binding, we need to allocate a closure, and each closure must
   capture the address of the other closures.
   We want to generate the following C-- code:
     // Initialization Code
     x = hp - 24; // heap address of x's closure
     y = hp - 40; // heap address of x's closure
     z = hp - 64; // heap address of x's closure
     // allocate and initialize x
     m[hp-8]   = ...
     m[hp-16]  = y       // the closure for x captures y
     m[hp-24] = x_info;
     // allocate and initialize y
     m[hp-32] = z;       // the closure for y captures z
     m[hp-40] = y_info;
     // allocate and initialize z
     ...

   For each closure, we must generate not only the code to allocate and
   initialize the closure itself, but also some initialization Code that
   sets a variable holding the closure pointer.

   We could generate a pair of the (init code, body code), but since
   the bindings are recursive we also have to initialise the
   environment with the CgIdInfo for all the bindings before compiling
   anything.  So we do this in 3 stages:

     1. collect all the CgIdInfos and initialise the environment
     2. compile each binding into (init, body) code
     3. emit all the inits, and then all the bodies

   We'd rather not have separate functions to do steps 1 and 2 for
   each binding, since in pratice they share a lot of code.  So we
   have just one function, cgRhs, that returns a pair of the CgIdInfo
   for step 1, and a monadic computation to generate the code in step
   2.

   The alternative to separating things in this way is to use a
   fixpoint.  That's what we used to do, but it introduces a
   maintenance nightmare because there is a subtle dependency on not
   being too strict everywhere.  Doing things this way means that the
   FCode monad can be strict, for example.
 

### Note: GC recovery

 mkRhsClosure looks for two special forms of the right-hand side:
        a) selector thunks
        b) AP thunks

If neither happens, it just calls mkClosureLFInfo.  You might think
that mkClosureLFInfo should do all this, but it seems wrong for the
latter to look at the structure of an expression

### Note: Selectors

We look at the body of the closure to see if it's a selector---turgid,
but nothing deep.  We are looking for a closure of {\em exactly} the
form:

...  = [the_fv] \ u [] ->
         case the_fv of
           con a_1 ... a_n -> a_i

### Note: Ap thunks

A more generic AP thunk of the form

        x = [ x_1...x_n ] \.. [] -> x_1 ... x_n

A set of these is compiled statically into the RTS, so we just use
those.  We could extend the idea to thunks where some of the x_i are
global ids (and hence not free variables), but this would entail
generating a larger thunk.  It might be an option for non-optimising
compilation, though.

We only generate an Ap thunk if all the free variables are pointers,
for semi-obvious reasons.



no args

no args

 no args

 There are two main cases for the code for closures.

* If there are *no arguments*, then the closure is a thunk, and not in
  normal form. So it should set up an update frame (if it is
  shared). NB: Thunks cannot have a primitive type!

* If there is *at least one* argument, then this closure is in
  normal form, so there is no need to set up an update frame.
