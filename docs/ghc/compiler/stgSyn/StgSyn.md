[[src]](https://github.com/ghc/ghc/tree/master/compiler/stgSyn/StgSyn.hs)

(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

# Shared term graph (STG) syntax for spineless-tagless code generation

This data type represents programs just before code generation (conversion to
@Cmm@): basically, what we have is a stylised form of @CoreSyntax@, the style
being one that happens to be ideally suited to spineless tagless code
generation.


 instances 

# \subsection{@GenStgBinding@}


As usual, expressions are interesting; other things are boring. Here
are the boring things [except note the @GenStgRhs@], parameterised
with respect to binder and occurrence information (just as in
@CoreSyn@):


# \subsection{@GenStgArg@}


# \subsection{STG expressions}


The @GenStgExpr@ data type is parameterised on binder and occurrence
info, as before.

# \subsubsection{@GenStgExpr@ application}


An application is of a function to a list of atoms [not expressions].
Operationally, we want to push the arguments on the stack and call the
function. (If the arguments were expressions, we would have to build
their closures first.)

There is no constructor for a lone variable; it would appear as
@StgApp var []@.


# \subsubsection{@StgConApp@ and @StgPrimApp@---saturated applications}


There are specialised forms of application, for constructors,
primitives, and literals.


# \subsubsection{@StgLam@}


StgLam is used *only* during CoreToStg's work. Before CoreToStg has
finished it encodes (\x -> e) as (let f = \x -> e in f)


# \subsubsection{@GenStgExpr@: case-expressions}


This has the same boxed/unboxed business as Core case expressions.


# \subsubsection{@GenStgExpr@: @let(rec)@-expressions}


The various forms of let(rec)-expression encode most of the
interesting things we want to do.
\begin{enumerate}
\item
\begin{verbatim}
let-closure x = [free-vars] [args] expr
in e
\end{verbatim}
is equivalent to
\begin{verbatim}
let x = (\free-vars -> \args -> expr) free-vars
\end{verbatim}
\tr{args} may be empty (and is for most closures).  It isn't under
circumstances like this:
\begin{verbatim}
let x = (\y -> y+z)
\end{verbatim}
This gets mangled to
\begin{verbatim}
let-closure x = [z] [y] (y+z)
\end{verbatim}
The idea is that we compile code for @(y+z)@ in an environment in which
@z@ is bound to an offset from \tr{Node}, and @y@ is bound to an
offset from the stack pointer.

(A let-closure is an @StgLet@ with a @StgRhsClosure@ RHS.)

\item
\begin{verbatim}
let-constructor x = Constructor [args]
in e
\end{verbatim}

(A let-constructor is an @StgLet@ with a @StgRhsCon@ RHS.)

\item
Letrec-expressions are essentially the same deal as
let-closure/let-constructor, so we use a common structure and
distinguish between them with an @is_recursive@ boolean flag.

\item
\begin{verbatim}
let-unboxed u = an arbitrary arithmetic expression in unboxed values
in e
\end{verbatim}
All the stuff on the RHS must be fully evaluated.
No function calls either!

(We've backed away from this toward case-expressions with
suitably-magical alts ...)

\item
~[Advanced stuff here! Not to start with, but makes pattern matching
generate more efficient code.]

\begin{verbatim}
let-escapes-not fail = expr
in e'
\end{verbatim}
Here the idea is that @e'@ guarantees not to put @fail@ in a data structure,
or pass it to another function. All @e'@ will ever do is tail-call @fail@.
Rather than build a closure for @fail@, all we need do is to record the stack
level at the moment of the @let-escapes-not@; then entering @fail@ is just
a matter of adjusting the stack pointer back down to that point and entering
the code for it.

Another example:
\begin{verbatim}
f x y = let z = huge-expression in
        if y==1 then z else
        if y==2 then z else
        1
\end{verbatim}

(A let-escapes-not is an @StgLetNoEscape@.)

\item
We may eventually want:
\begin{verbatim}
let-literal x = Literal
in e
\end{verbatim}
\end{enumerate}

And so the code for let(rec)-things:


# 

Finally for @hpc@ expressions we introduce a new STG construct.


# \subsection{STG right-hand sides}


Here's the rest of the interesting stuff for @StgLet@s; the first
flavour is for closures:



An example may be in order.  Consider:
\begin{verbatim}
let t = \x -> \y -> ... x ... y ... p ... q in e
\end{verbatim}
Pulling out the free vars and stylising somewhat, we get the equivalent:
\begin{verbatim}
let t = (\[p,q] -> \[x,y] -> ... x ... y ... p ...q) p q
\end{verbatim}
Stg-operationally, the @[x,y]@ are on the stack, the @[p,q]@ are
offsets from @Node@ into the closure, and the code ptr for the closure
will be exactly that in parentheses above.

The second flavour of right-hand-side is for constructors (simple but important):


# \subsection[Stg-case-alternatives]{STG case alternatives}


Very like in @CoreSyntax@ (except no type-world stuff).

The type constructor is guaranteed not to be abstract; that is, we can
see its representation. This is important because the code generator
uses it to determine return conventions etc. But it's not trivial
where there's a module loop involved, because some versions of a type
constructor might not have all the constructors visible. So
mkStgAlgAlts (in CoreToStg) ensures that it gets the TyCon from the
constructors or literals (which are guaranteed to have the Real McCoy)
rather than from the scrutinee type.


# \subsection[Stg]{The Plain STG parameterisation}


This happens to be the only one we use at the moment.


 Many passes apply a substitution, and it's very handy to have type
   synonyms to remind us whether or not the substitution has been applied.
   See CoreSyn for precedence in Core land




# \subsubsection[UpdateFlag-datatype]{@UpdateFlag@}


This is also used in @LambdaFormInfo@ in the @ClosureInfo@ module.

A @ReEntrant@ closure may be entered multiple times, but should not be
updated or blackholed. An @Updatable@ closure should be updated after
evaluation (and may be blackholed during evaluation). A @SingleEntry@
closure will only be entered once, and so need not be updated but may
safely be blackholed.


# \subsubsection{StgOp}


An StgOp allows us to group together PrimOps and ForeignCalls.
It's quite useful to move these around together, notably
in StgOpApp and COpStmt.


# \subsection[Stg-pretty-printing]{Pretty-printing}


Robin Popplestone asked for semi-colon separators on STG binds; here's
hoping he likes terminators instead...  Ditto for case alternatives.


 StgRec (begin) 

 StgRec (end) 


pprStgExpr (StgLet srt (StgNonRec bndr (StgRhsClosure cc bi free_vars upd_flag args rhs))
                        expr@(StgLet _ _))
  = ($$)
      (hang (hcat [text "let { ", ppr bndr, ptext (sLit " = "),
                          ppr cc,
                          pp_binder_info bi,
                          text " [", whenPprDebug (interppSP free_vars), ptext (sLit "] \\"),
                          ppr upd_flag, text " [",
                          interppSP args, char ']'])
            8 (sep [hsep [ppr rhs, text "} in"]]))
      (ppr expr)


no args