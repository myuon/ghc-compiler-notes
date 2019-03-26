`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/basicTypes/PatSyn.hs>`_

compiler/basicTypes/PatSyn.hs
=============================


Note [Pattern synonym signature contexts]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/basicTypes/PatSyn.hs#L113>`__

In a pattern synonym signature we write
   pattern P :: req => prov => t1 -> ... tn -> res_ty

Note that the "required" context comes first, then the "provided"
context.  Moreover, the "required" context must not mention
existentially-bound type variables; that is, ones not mentioned in
res_ty.  See lots of discussion in #10928.

If there is no "provided" context, you can omit it; but you
can't omit the "required" part (unless you omit both).

Example 1:
      pattern P1 :: (Num a, Eq a) => b -> Maybe (a,b)
      pattern P1 x = Just (3,x)

::

  We require (Num a, Eq a) to match the 3; there is no provided
  context.

Example 2:
      data T2 where
        MkT2 :: (Num a, Eq a) => a -> a -> T2

::

      pattern P2 :: () => (Num a, Eq a) => a -> T2
      pattern P2 x = MkT2 3 x

::

  When we match against P2 we get a Num dictionary provided.
  We can use that to check the match against 3.

Example 3:
      pattern P3 :: Eq a => a -> b -> T3 b

::

   This signature is illegal because the (Eq a) is a required
   constraint, but it mentions the existentially-bound variable 'a'.
   You can see it's existential because it doesn't appear in the
   result type (T3 b).



Note [Pattern synonym result type]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/basicTypes/PatSyn.hs#L151>`__

Consider
   data T a b = MkT b a

::

   pattern P :: a -> T [a] Bool
   pattern P x = MkT True [x]

P's psResultTy is (T a Bool), and it really only matches values of
type (T [a] Bool).  For example, this is ill-typed

::

   f :: T p q -> String
   f (P x) = "urk"

This is different to the situation with GADTs:

::

   data S a where
     MkS :: Int -> S Bool

Now MkS (and pattern synonyms coming from MkS) can match a
value of type (S a), not just (S Bool); we get type refinement.

That in turn means that if you have a pattern

::

   P x :: T [ty] Bool

it's not entirely straightforward to work out the instantiation of
P's universal tyvars. You have to /match/
  the type of the pattern, (T [ty] Bool)
against
  the psResultTy for the pattern synonym, T [a] Bool
to get the instantiation a := ty.

This is very unlike DataCons, where univ tyvars match 1-1 the
arguments of the TyCon.



Note [Pattern synonym representation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/basicTypes/PatSyn.hs#L188>`__

Consider the following pattern synonym declaration

::

        pattern P x = MkT [x] (Just 42)

where
        data T a where
              MkT :: (Show a, Ord b) => [b] -> a -> T a

so pattern P has type

::

        b -> T (Maybe t)

with the following typeclass constraints:

::

        requires: (Eq t, Num t)
        provides: (Show (Maybe t), Ord b)

In this case, the fields of MkPatSyn will be set as follows:

::

  psArgs       = [b]
  psArity      = 1
  psInfix      = False

::

  psUnivTyVars = [t]
  psExTyVars   = [b]
  psProvTheta  = (Show (Maybe t), Ord b)
  psReqTheta   = (Eq t, Num t)
  psResultTy  = T (Maybe t)



Note [Matchers and builders for pattern synonyms]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/basicTypes/PatSyn.hs#L219>`__

For each pattern synonym P, we generate

  * a "matcher" function, used to desugar uses of P in patterns,
    which implements pattern matching

  * A "builder" function (for bidirectional pattern synonyms only),
    used to desugar uses of P in expressions, which constructs P-values.

For the above example, the matcher function has type:

::

        $mP :: forall (r :: ?) t. (Eq t, Num t)
            => T (Maybe t)
            -> (forall b. (Show (Maybe t), Ord b) => b -> r)
            -> (Void# -> r)
            -> r

with the following implementation:

::

        $mP @r @t $dEq $dNum scrut cont fail
          = case scrut of
              MkT @b $dShow $dOrd [x] (Just 42) -> cont @b $dShow $dOrd x
              _                                 -> fail Void#

Notice that the return type 'r' has an open kind, so that it can
be instantiated by an unboxed type; for example where we see
     f (P x) = 3#

The extra Void# argument for the failure continuation is needed so that
it is lazy even when the result type is unboxed.

For the same reason, if the pattern has no arguments, an extra Void#
argument is added to the success continuation as well.

For *bidirectional* pattern synonyms, we also generate a "builder"
function which implements the pattern synonym in an expression
context. For our running example, it will be:

::

        $bP :: forall t b. (Eq t, Num t, Show (Maybe t), Ord b)
            => b -> T (Maybe t)
        $bP x = MkT [x] (Just 42)

NB: the existential/universal and required/provided split does not
apply to the builder since you are only putting stuff in, not getting
stuff out.

Injectivity of bidirectional pattern synonyms is checked in
tcPatToExpr which walks the pattern and returns its corresponding
expression when available.



Note [Builder for pattern synonyms with unboxed type]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/basicTypes/PatSyn.hs#L270>`__

For bidirectional pattern synonyms that have no arguments and have an
unboxed type, we add an extra Void# argument to the builder, else it
would be a top-level declaration with an unboxed type.

::

        pattern P = 0#

::

        $bP :: Void# -> Int#
        $bP _ = 0#

This means that when typechecking an occurrence of P in an expression,
we must remember that the builder has this void argument. This is
done by TcPatSyn.patSynBuilderOcc.



Note [Pattern synonyms and the data type Type]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/basicTypes/PatSyn.hs#L285>`__

The type of a pattern synonym is of the form (See Note
[Pattern synonym signatures] in TcSigs):

::

    forall univ_tvs. req => forall ex_tvs. prov => ...

We cannot in general represent this by a value of type Type:

 - if ex_tvs is empty, then req and prov cannot be distinguished from
   each other
 - if req is empty, then univ_tvs and ex_tvs cannot be distinguished
   from each other, and moreover, prov is seen as the "required" context
   (as it is the only context)

