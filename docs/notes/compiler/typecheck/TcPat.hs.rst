`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcPat.hs>`_

compiler/typecheck/TcPat.hs
===========================


Note [Subsumption check at pattern variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcPat.hs#L239>`__

When we come across a variable with a type signature, we need to do a
subsumption, not equality, check against the context type.  e.g.

::

    data T = MkT (forall a. a->a)
      f :: forall b. [b]->[b]
      MkT f = blah

..

Since 'blah' returns a value of type T, its payload is a polymorphic
function of type (forall a. a->a).  And that's enough to bind the
less-polymorphic function 'f', but we need some impedance matching
to witness the instantiation.



Note [Nesting]
~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcPat.hs#L260>`__

tcPat takes a "thing inside" over which the pattern scopes.  This is partly
so that tcPat can extend the environment for the thing_inside, but also
so that constraints arising in the thing_inside can be discharged by the
pattern.

This does not work so well for the ErrCtxt carried by the monad: we don't
want the error-context for the pattern to scope over the RHS.
Hence the getErrCtxt/setErrCtxt stuff in tcMultiple



Note [NPlusK patterns]
~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcPat.hs#L544>`__

From

::

  case v of x + 5 -> blah

..

we get

::

  if v >= 5 then (\x -> blah) (v - 5) else ...

..

There are two bits of rebindable syntax:
  (>=) :: pat_ty -> lit1_ty -> Bool
  (-)  :: pat_ty -> lit2_ty -> var_ty

lit1_ty and lit2_ty could conceivably be different.
var_ty is the type inferred for x, the variable in the pattern.

If the pushed-down pattern type isn't a tau-type, the two pat_ty's above
could conceivably be different specializations. But this is very much
like the situation in Note [Case branches must be taus] in TcMatches.
So we tauify the pat_ty before proceeding.

Note that we need to type-check the literal twice, because it is used
twice, and may be used at different types. The second HsOverLit stored in the
AST is used for the subtraction operation.

See Note [NPlusK patterns]



Note [Hopping the LIE in lazy patterns]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcPat.hs#L620>`__

In a lazy pattern, we must *not* discharge constraints from the RHS
from dictionaries bound in the pattern.  E.g.
        f ~(C x) = 3
We can't discharge the Num constraint from dictionaries bound by
the pattern C!

So we have to make the constraints from thing_inside "hop around"
the pattern.  Hence the captureConstraints and emitConstraints.

The same thing ensures that equality constraints in a lazy match
are not made available in the RHS of the match. For example
        data T a where { T1 :: Int -> T Int; ... }
        f :: T a -> Int -> a
        f ~(T1 i) y = y
It's obviously not sound to refine a to Int in the right
hand side, because the argument might not match T1 at all!

Finally, a lazy pattern should not bind any existential type variables
because they won't be in scope when we do the desugaring



Note [Matching constructor patterns]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcPat.hs#L930>`__

Suppose (coi, tys) = matchExpectedConType data_tc pat_ty

 * In the simple case, pat_ty = tc tys

 * If pat_ty is a polytype, we want to instantiate it
   This is like part of a subsumption check.  Eg
      f :: (forall a. [a]) -> blah
      f [] = blah

 * In a type family case, suppose we have
          data family T a
          data instance T (p,q) = A p | B q
       Then we'll have internally generated
              data T7 p q = A p | B q
              axiom coT7 p q :: T (p,q) ~ T7 p q

::

       So if pat_ty = T (ty1,ty2), we return (coi, [ty1,ty2]) such that
           coi = coi2 . coi1 : T7 t ~ pat_ty
           coi1 : T (ty1,ty2) ~ pat_ty
           coi2 : T7 ty1 ty2 ~ T (ty1,ty2)

..

::

   For families we do all this matching here, not in the unifier,
   because we never want a whisper of the data_tycon to appear in
   error messages; it's a purely internal thing

..



Note [Arrows and patterns]
~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcPat.hs#L1048>`__

(Oct 07) Arrow notation has the odd property that it involves
"holes in the scope". For example:
  expr :: Arrow a => a () Int
  expr = proc (y,z) -> do
          x <- term -< y
          expr' -< x

Here the 'proc (y,z)' binding scopes over the arrow tails but not the
arrow body (e.g 'term').  As things stand (bogusly) all the
constraints from the proc body are gathered together, so constraints
from 'term' will be seen by the tcPat for (y,z).  But we must *not*
bind constraints from 'term' here, because the desugarer will not make
these bindings scope over 'term'.

The Right Thing is not to confuse these constraints together. But for
now the Easy Thing is to ensure that we do not have existential or
GADT constraints in a 'proc', and to short-cut the constraint
simplification for such vanilla patterns so that it binds no
constraints. Hence the 'fast path' in tcConPat; but it's also a good
plan for ordinary vanilla patterns to bypass the constraint
simplification step.



Note [Existential check]
~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcPat.hs#L1139>`__

Lazy patterns can't bind existentials.  They arise in two ways:
  * Let bindings      let { C a b = e } in b
  * Twiddle patterns  f ~(C a b) = e
The pe_lazy field of PatEnv says whether we are inside a lazy
pattern (perhaps deeply)

See also Note [Typechecking pattern bindings] in TcBinds

