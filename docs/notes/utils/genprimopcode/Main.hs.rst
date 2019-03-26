`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/utils/genprimopcode/Main.hs>`_

utils/genprimopcode/Main.hs
===========================


Note [Placeholder declarations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/utils/genprimopcode/Main.hs#L387>`__

We are generating fake declarations for things in GHC.Prim, just to
keep GHC's renamer and typechecker happy enough for what Haddock
needs.  Our main plan is to say
        foo :: <type>
        foo = foo
We have to silence GHC's complaints about unboxed-top-level declarations
with an ad-hoc fix in TcBinds: see Note [Compiling GHC.Prim] in TcBinds.

That works for all the primitive functions except tagToEnum#.
If we generate the binding
        tagToEnum# = tagToEnum#
GHC will complain about "tagToEnum# must appear applied to one argument".
We could hack GHC to silence this complaint when compiling GHC.Prim,
but it seems easier to generate
        tagToEnum# = let x = x in x
We don't do this for *all* bindings because for ones with an unboxed
RHS we would get other complaints (e.g.can't unify "*" with "#").

