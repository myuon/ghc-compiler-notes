[[src]](https://github.com/ghc/ghc/tree/master/compiler/coreSyn/PprCore.hs)

(c) The University of Glasgow 2006
(c) The AQUA Project, Glasgow University, 1996-1998


Printing of Core syntax


# \subsection{Public interfaces for Core printing (excluding instances)}


@pprParendCoreExpr@ puts parens around non-atomic Core expressions.


# \subsection{The guts}



ppr_expr add_par (Let bind@(NonRec val_bdr rhs@(Let _ _)) body)
  = add_par $
    vcat [
      hsep [text "let {", (pprBndr LetBind val_bdr $$ ppr val_bndr), equals],
      nest 2 (pprCoreExpr rhs),
      text "} in",
      pprCoreExpr body ]

ppr_expr add_par (Let bind@(NonRec val_bdr rhs) expr@(Let _ _))
  = add_par
    (hang (text "let {")
          2 (hsep [ppr_binding (val_bdr,rhs),
                   text "} in"])
     $$
     pprCoreExpr expr)


### Note: Print case as let

Single-branch case expressions are very common:
   case x of y { I# x' ->
   case p of q { I# p' -> ... } }
These are, in effect, just strict let's, with pattern matching.
With -dppr-case-as-let we print them as such:
   let! { I# x' ~ y <- x } in
   let! { I# p' ~ q <- p } in ...


Other printing bits-and-bobs used with the general @pprCoreBinding@
and @pprCoreExpr@ functions.

### Note: Binding-site specific printing


pprCoreBinder and pprTypedLamBinder receive a BindingSite argument to adjust
the information printed.

Let-bound binders are printed with their full type and idInfo.

Case-bound variables (both the case binder and pattern variables) are printed
without a type and without their unfolding.

Furthermore, a dead case-binder is completely ignored, while otherwise, dead
binders are printed as "_".



-----------------------------------------------------
--      IdDetails and IdInfo
-----------------------------------------------------



-----------------------------------------------------
--      Unfolding and UnfoldingGuidance
-----------------------------------------------------



-----------------------------------------------------
--      Rules
-----------------------------------------------------



-----------------------------------------------------
--      Tickish
-----------------------------------------------------



-----------------------------------------------------
--      Vectorisation declarations
-----------------------------------------------------
