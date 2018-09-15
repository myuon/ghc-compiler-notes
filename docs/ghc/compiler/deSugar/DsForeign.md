[[src]](https://github.com/ghc/ghc/tree/master/compiler/deSugar/DsForeign.hs)

(c) The University of Glasgow 2006
(c) The AQUA Project, Glasgow University, 1998


Desugaring foreign declarations (see also DsCCall).



Desugaring of @foreign@ declarations is naturally split up into
parts, an @import@ and an @export@  part. A @foreign import@
declaration
\begin{verbatim}
  foreign import cc nm f :: prim_args -> IO prim_res
\end{verbatim}
is the same as
\begin{verbatim}
  f :: prim_args -> IO prim_res
  f a1 ... an = _ccall_ nm cc a1 ... an
\end{verbatim}
so we reuse the desugaring code in @DsCCall@ to deal with these.


# \subsection{Foreign import}


Desugaring foreign imports is just the matter of creating a binding
that on its RHS unboxes its arguments, performs the external call
(using the @CCallOp@ primop), before boxing the result up and returning it.

However, we create a worker/wrapper pair, thus:

        fw s x# = ccall f s x#

The strictness/CPR analyser won't do this automatically because it doesn't look
inside returned tuples; but inlining this wrapper is a Really Good Idea
because it exposes the boxing to the call site.


# \subsection{Foreign calls}


# \subsection{Primitive calls}


This is for `@foreign import prim@' declarations.

Currently, at the core level we pretend that these primitive calls are
foreign calls. It may make more sense in future to have them as a distinct
kind of Id, or perhaps to bundle them with PrimOps since semantically and
for calling convention they are really prim ops.


# \subsection{Foreign export}


The function that does most of the work for `@foreign export@' declarations.
(see below for the boilerplate code a `@foreign export@' declaration expands
 into.)

For each `@foreign export foo@' in a module M we generate:
\begin{itemize}
\item a C function `@foo@', which calls
\item a Haskell stub `@M.\$ffoo@', which calls
\end{itemize}
the user-written Haskell function `@M.foo@'.



@foreign import "wrapper"@ (previously "foreign export dynamic") lets
you dress up Haskell IO actions of some fixed type behind an
externally callable interface (i.e., as a C function pointer). Useful
for callbacks and stuff.

\begin{verbatim}
type Fun = Bool -> Int -> IO Int
foreign import "wrapper" f :: Fun -> IO (FunPtr Fun)

-- Haskell-visible constructor, which is generated from the above:
-- SUP: No check for NULL from createAdjustor anymore???

foreign import "&f_helper" f_helper :: FunPtr (StablePtr Fun -> Fun)

-- and the helper in C: (approximately; see `mkFExportCBits` below)

f_helper(StablePtr s, HsBool b, HsInt i)
{
        Capability *cap;
        cap = rts_lock();
        rts_evalIO(&cap,
                   rts_apply(rts_apply(deRefStablePtr(s),
                                       rts_mkBool(b)), rts_mkInt(i)));
        rts_unlock(cap);
}
\end{verbatim}



          The arguments to the external function which will
          create a little bit of (template) code on the fly
          for allowing the (stable pointed) Haskell closure
          to be entered using an external calling convention
          (stdcall, ccall).
         


*

\subsection{Generating @foreign export@ stubs}

*

For each @foreign export@ function, a C stub function is generated.
The C stub constructs the application of the exported Haskell function
using the hugs/ghc rts invocation API.
