`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/coreSyn/PprCore.hs>`_

compiler/coreSyn/PprCore.hs
===========================


Note [Print case as let]
~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/coreSyn/PprCore.hs#L327>`__

Single-branch case expressions are very common:
   case x of y { I# x' ->
   case p of q { I# p' -> ... } }
These are, in effect, just strict let's, with pattern matching.
With -dppr-case-as-let we print them as such:
   let! { I# x' ~ y <- x } in
   let! { I# p' ~ q <- p } in ...


Other printing bits-and-bobs used with the general @pprCoreBinding@
and @pprCoreExpr@ functions.



Note [Binding-site specific printing]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/coreSyn/PprCore.hs#L342>`__

pprCoreBinder and pprTypedLamBinder receive a BindingSite argument to adjust
the information printed.

Let-bound binders are printed with their full type and idInfo.

Case-bound variables (both the case binder and pattern variables) are printed
without a type and without their unfolding.

Furthermore, a dead case-binder is completely ignored, while otherwise, dead
binders are printed as "_".
These instances are sadly orphans

