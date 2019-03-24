`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/deSugar/Check.hs>`_

====================
compiler/deSugar/Check.hs.rst
====================

Note [Recovering from unsatisfiable pattern-matching constraints]
~~~~~~~~~~~~~~~~
Consider the following code (see #12957 and #15450):

.. code-block:: haskell

  f :: Int ~ Bool => ()
  f = case True of { False -> () }

We want to warn that the pattern-matching in `f` is non-exhaustive. But GHC
used not to do this; in fact, it would warn that the match was /redundant/!
This is because the constraint (Int ~ Bool) in `f` is unsatisfiable, and the
coverage checker deems any matches with unsatifiable constraint sets to be
unreachable.

We decide to better than this. When beginning coverage checking, we first
check if the constraints in scope are unsatisfiable, and if so, we start
afresh with an empty set of constraints. This way, we'll get the warnings
that we expect.


Note [Type normalisation for EmptyCase]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
EmptyCase is an exception for pattern matching, since it is strict. This means
that it boils down to checking whether the type of the scrutinee is inhabited.
Function pmTopNormaliseType_maybe gets rid of the outermost type function/data
family redex and newtypes, in search of an algebraic type constructor, which is
easier to check for inhabitation.

It returns 3 results instead of one, because there are 2 subtle points:
1. Newtypes are isomorphic to the underlying type in core but not in the source
   language,
2. The representational data family tycon is used internally but should not be
   shown to the user

Hence, if pmTopNormaliseType_maybe env ty_cs ty = Just (src_ty, dcs, core_ty),
then
  (a) src_ty is the rewritten type which we can show to the user. That is, the
      type we get if we rewrite type families but not data families or
      newtypes.
  (b) dcs is the list of data constructors "skipped", every time we normalise a
      newtype to its core representation, we keep track of the source data
      constructor.
  (c) core_ty is the rewritten type. That is,
        pmTopNormaliseType_maybe env ty_cs ty = Just (src_ty, dcs, core_ty)
      implies
        topNormaliseType_maybe env ty = Just (co, core_ty)
      for some coercion co.

To see how all cases come into play, consider the following example:

.. code-block:: haskell

  data family T a :: *
  data instance T Int = T1 | T2 Bool
  -- Which gives rise to FC:
  --   data T a
  --   data R:TInt = T1 | T2 Bool
  --   axiom ax_ti : T Int ~R R:TInt

.. code-block:: haskell

  newtype G1 = MkG1 (T Int)
  newtype G2 = MkG2 G1

.. code-block:: haskell

  type instance F Int  = F Char
  type instance F Char = G2

In this case pmTopNormaliseType_maybe env ty_cs (F Int) results in

.. code-block:: haskell

  Just (G2, [MkG2,MkG1], R:TInt)

Which means that in source Haskell:
  - G2 is equivalent to F Int (in contrast, G1 isn't).
  - if (x : R:TInt) then (MkG2 (MkG1 x) : F Int).

-----
-- Wrinkle: Local equalities
-----

Given the following type family:

.. code-block:: haskell

  type family F a
  type instance F Int = Void

Should the following program (from #14813) be considered exhaustive?

.. code-block:: haskell

  f :: (i ~ Int) => F i -> a
  f x = case x of {}

You might think "of course, since `x` is obviously of type Void". But the
idType of `x` is technically F i, not Void, so if we pass F i to
inhabitationCandidates, we'll mistakenly conclude that `f` is non-exhaustive.
In order to avoid this pitfall, we need to normalise the type passed to
pmTopNormaliseType_maybe, using the constraint solver to solve for any local
equalities (such as i ~ Int) that may be in scope.


Note [Checking EmptyCase Expressions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Empty case expressions are strict on the scrutinee. That is, `case x of {}`
will force argument `x`. Hence, `checkMatches` is not sufficient for checking
empty cases, because it assumes that the match is not strict (which is true
for all other cases, apart from EmptyCase). This gave rise to #10746. Instead,
we do the following:

1. We normalise the outermost type family redex, data family redex or newtype,
   using pmTopNormaliseType_maybe (in types/FamInstEnv.hs). This computes 3
   things:
   (a) A normalised type src_ty, which is equal to the type of the scrutinee in
       source Haskell (does not normalise newtypes or data families)
   (b) The actual normalised type core_ty, which coincides with the result
       topNormaliseType_maybe. This type is not necessarily equal to the input
       type in source Haskell. And this is precicely the reason we compute (a)
       and (c): the reasoning happens with the underlying types, but both the
       patterns and types we print should respect newtypes and also show the
       family type constructors and not the representation constructors.

.. code-block:: haskell

   (c) A list of all newtype data constructors dcs, each one corresponding to a
       newtype rewrite performed in (b).

.. code-block:: haskell

   For an example see also Note [Type normalisation for EmptyCase]
   in types/FamInstEnv.hs.

2. Function checkEmptyCase' performs the check:
   - If core_ty is not an algebraic type, then we cannot check for
     inhabitation, so we emit (_ :: src_ty) as missing, conservatively assuming
     that the type is inhabited.
   - If core_ty is an algebraic type, then we unfold the scrutinee to all
     possible constructor patterns, using inhabitationCandidates, and then
     check each one for constraint satisfiability, same as we for normal
     pattern match checking.

%************************************************************************
%*                                                                      *
              Transform source syntax to *our* syntax
%*                                                                      *
%************************************************************************
-----------------------------------------------------------------------


Note [Translate Overloaded Literal for Exhaustiveness Checking]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The translation of @NPat@ in exhaustiveness checker is a bit different
from translation in pattern matcher.

  * In pattern matcher (see `tidyNPat' in deSugar/MatchLit.hs), we
    translate integral literals to HsIntPrim or HsWordPrim and translate
    overloaded strings to HsString.

  * In exhaustiveness checker, in `genCaseTmCs1/genCaseTmCs2`, we use
    `lhsExprToPmExpr` to generate uncovered set. In `hsExprToPmExpr`,
    however we generate `PmOLit` for HsOverLit, rather than refine
    `HsOverLit` inside `NPat` to HsIntPrim/HsWordPrim. If we do
    the same thing in `translatePat` as in `tidyNPat`, the exhaustiveness
    checker will fail to match the literals patterns correctly. See
    #14546.

.. code-block:: haskell

  In Note [Undecidable Equality for Overloaded Literals], we say: "treat
  overloaded literals that look different as different", but previously we
  didn't do such things.

.. code-block:: haskell

  Now, we translate the literal value to match and the literal patterns
  consistently:

  * For integral literals, we parse both the integral literal value and
    the patterns as OverLit HsIntegral. For example:

      case 0::Int of
          0 -> putStrLn "A"
          1 -> putStrLn "B"
          _ -> putStrLn "C"

    When checking the exhaustiveness of pattern matching, we translate the 0
    in value position as PmOLit, but translate the 0 and 1 in pattern position
    as PmSLit. The inconsistency leads to the failure of eqPmLit to detect the
    equality and report warning of "Pattern match is redundant" on pattern 0,
    as reported in #14546. In this patch we remove the specialization of
    OverLit patterns, and keep the overloaded number literal in pattern as it
    is to maintain the consistency. We know nothing about the `fromInteger`
    method (see Note [Undecidable Equality for Overloaded Literals]). Now we
    can capture the exhaustiveness of pattern 0 and the redundancy of pattern
    1 and _.

  * For string literals, we parse the string literals as HsString. When
    OverloadedStrings is enabled, it further be turned as HsOverLit HsIsString.
    For example:

.. code-block:: haskell

      case "foo" of
          "foo" -> putStrLn "A"
          "bar" -> putStrLn "B"
          "baz" -> putStrLn "C"

.. code-block:: haskell

    Previously, the overloaded string values are translated to PmOLit and the
    non-overloaded string values are translated to PmSLit. However the string
    patterns, both overloaded and non-overloaded, are translated to list of
    characters. The inconsistency leads to wrong warnings about redundant and
    non-exhaustive pattern matching warnings, as reported in #14546.

.. code-block:: haskell

    In order to catch the redundant pattern in following case:

.. code-block:: haskell

      case "foo" of
          ('f':_) -> putStrLn "A"
          "bar" -> putStrLn "B"

.. code-block:: haskell

    in this patch, we translate non-overloaded string literals, both in value
    position and pattern position, as list of characters. For overloaded string
    literals, we only translate it to list of characters only when it's type
    is stringTy, since we know nothing about the toString methods. But we know
    that if two overloaded strings are syntax equal, then they are equal. Then
    if it's type is not stringTy, we just translate it to PmOLit. We can still
    capture the exhaustiveness of pattern "foo" and the redundancy of pattern
    "bar" and "baz" in the following code:

.. code-block:: haskell

      {-# LANGUAGE OverloadedStrings #-}
      main = do
        case "foo" of
            "foo" -> putStrLn "A"
            "bar" -> putStrLn "B"
            "baz" -> putStrLn "C"

.. code-block:: haskell

  We must ensure that doing the same translation to literal values and patterns
  in `translatePat` and `hsExprToPmExpr`. The previous inconsistent work led to
  #14546.


Note [Guards and Approximation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Even if the algorithm is really expressive, the term oracle we use is not.
Hence, several features are not translated *properly* but we approximate.
The list includes:

1. View Patterns
----------------
A view pattern @(f -> p)@ should be translated to @x (p <- f x)@. The term
oracle does not handle function applications so we know that the generated
constraints will not be handled at the end. Hence, we distinguish between two
cases:
  a) Pattern @p@ cannot fail. Then this is just a binding and we do the *right
     thing*.
  b) Pattern @p@ can fail. This means that when checking the guard, we will
     generate several cases, with no useful information. E.g.:

.. code-block:: haskell

       h (f -> [a,b]) = ...
       h x ([a,b] <- f x) = ...

.. code-block:: haskell

       uncovered set = { [x |> { False ~ (f x ~ [])            }]
                       , [x |> { False ~ (f x ~ (t1:[]))       }]
                       , [x |> { False ~ (f x ~ (t1:t2:t3:t4)) }] }

     So we have two problems:
       1) Since we do not print the constraints in the general case (they may
          be too many), the warning will look like this:

            Pattern match(es) are non-exhaustive
            In an equation for `h':
                Patterns not matched:
                    _
                    _
                    _
          Which is not short and not more useful than a single underscore.
       2) The size of the uncovered set increases a lot, without gaining more
          expressivity in our warnings.

.. code-block:: haskell

     Hence, in this case, we replace the guard @([a,b] <- f x)@ with a *dummy*
     @fake_pat@: @True <- _@. That is, we record that there is a possibility
     of failure but we minimize it to a True/False. This generates a single
     warning and much smaller uncovered sets.

2. Overloaded Lists
-------------------
An overloaded list @[...]@ should be translated to @x ([...] <- toList x)@. The
problem is exactly like above, as its solution. For future reference, the code
below is the *right thing to do*:

.. code-block:: haskell

   ListPat (ListPatTc elem_ty (Just (pat_ty, _to_list))) lpats
     otherwise -> do
       (xp, xe) <- mkPmId2Forms pat_ty
       ps       <- translatePatVec (map unLoc lpats)
       let pats = foldr (mkListPatVec elem_ty) [nilPattern elem_ty] ps
           g    = mkGuard pats (HsApp (noLoc to_list) xe)
       return [xp,g]

3. Overloaded Literals
----------------------
The case with literals is a bit different. a literal @l@ should be translated
to @x (True <- x == from l)@. Since we want to have better warnings for
overloaded literals as it is a very common feature, we treat them differently.
They are mainly covered in Note [Undecidable Equality for Overloaded Literals]
in PmExpr.

4. N+K Patterns & Pattern Synonyms
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
An n+k pattern (n+k) should be translated to @x (True <- x >= k) (n <- x-k)@.
Since the only pattern of the three that causes failure is guard @(n <- x-k)@,
and has two possible outcomes. Hence, there is no benefit in using a dummy and
we implement the proper thing. Pattern synonyms are simply not implemented yet.
Hence, to be conservative, we generate a dummy pattern, assuming that the
pattern can fail.

5. Actual Guards
----------------
During translation, boolean guards and pattern guards are translated properly.
Let bindings though are omitted by function @translateLet@. Since they are lazy
bindings, we do not actually want to generate a (strict) equality (like we do
in the pattern bind case). Hence, we safely drop them.

Additionally, top-level guard translation (performed by @translateGuards@)
replaces guards that cannot be reasoned about (like the ones we described in
1-4) with a single @fake_pat@ to record the possibility of failure to match.



Note [Translate CoPats]
~~~~~~~~~~~~~~~~~~~~~~~
The pattern match checker did not know how to handle coerced patterns `CoPat`
efficiently, which gave rise to #11276. The original approach translated
`CoPat`s:

.. code-block:: haskell

    pat |> co    ===>    x (pat <- (e |> co))

Instead, we now check whether the coercion is a hole or if it is just refl, in
which case we can drop it. Unfortunately, data families generate useful
coercions so guards are still generated in these cases and checking data
families is not really efficient.

%************************************************************************
%*                                                                      *
                 Utilities for Pattern Match Checking
%*                                                                      *
%************************************************************************
----------------------------------------------------------------------------


Note [Extensions to GADTs Meet Their Match]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The GADTs Meet Their Match paper presents the formalism that GHC's coverage
checker adheres to. Since the paper's publication, there have been some
additional features added to the coverage checker which are not described in
the paper. This Note serves as a reference for these new features.

-----
-- Strict argument type constraints
-----

In the ConVar case of clause processing, each conlike K traditionally
generates two different forms of constraints:

* A term constraint (e.g., x ~ K y1 ... yn)
* Type constraints from the conlike's context (e.g., if K has type
  forall bs. Q => s1 .. sn -> T tys, then Q would be its type constraints)

As it turns out, these alone are not enough to detect a certain class of
unreachable code. Consider the following example (adapted from #15305):

.. code-block:: haskell

  data K = K1 | K2 !Void

.. code-block:: haskell

  f :: K -> ()
  f K1 = ()

Even though `f` doesn't match on `K2`, `f` is exhaustive in its patterns. Why?
Because it's impossible to construct a terminating value of type `K` using the
`K2` constructor, and thus it's impossible for `f` to ever successfully match
on `K2`.

The reason is because `K2`'s field of type `Void` is //strict//. Because there
are no terminating values of type `Void`, any attempt to construct something
using `K2` will immediately loop infinitely or throw an exception due to the
strictness annotation. (If the field were not strict, then `f` could match on,
say, `K2 undefined` or `K2 (let x = x in x)`.)

Since neither the term nor type constraints mentioned above take strict
argument types into account, we make use of the `nonVoid` function to
determine whether a strict type is inhabitable by a terminating value or not.

`nonVoid ty` returns True when either:
1. `ty` has at least one InhabitationCandidate for which both its term and type
   constraints are satifiable, and `nonVoid` returns `True` for all of the
   strict argument types in that InhabitationCandidate.
2. We're unsure if it's inhabited by a terminating value.

`nonVoid ty` returns False when `ty` is definitely uninhabited by anything
(except bottom). Some examples:

* `nonVoid Void` returns False, since Void has no InhabitationCandidates.
  (This is what lets us discard the `K2` constructor in the earlier example.)
* `nonVoid (Int :~: Int)` returns True, since it has an InhabitationCandidate
  (through the Refl constructor), and its term constraint (x ~ Refl) and
  type constraint (Int ~ Int) are satisfiable.
* `nonVoid (Int :~: Bool)` returns False. Although it has an
  InhabitationCandidate (by way of Refl), its type constraint (Int ~ Bool) is
  not satisfiable.
* Given the following definition of `MyVoid`:

.. code-block:: haskell

    data MyVoid = MkMyVoid !Void

.. code-block:: haskell

  `nonVoid MyVoid` returns False. The InhabitationCandidate for the MkMyVoid
  constructor contains Void as a strict argument type, and since `nonVoid Void`
  returns False, that InhabitationCandidate is discarded, leaving no others.

* Performance considerations

We must be careful when recursively calling `nonVoid` on the strict argument
types of an InhabitationCandidate, because doing so naïvely can cause GHC to
fall into an infinite loop. Consider the following example:

.. code-block:: haskell

  data Abyss = MkAbyss !Abyss

.. code-block:: haskell

  stareIntoTheAbyss :: Abyss -> a
  stareIntoTheAbyss x = case x of {}

In principle, stareIntoTheAbyss is exhaustive, since there is no way to
construct a terminating value using MkAbyss. However, both the term and type
constraints for MkAbyss are satisfiable, so the only way one could determine
that MkAbyss is unreachable is to check if `nonVoid Abyss` returns False.
There is only one InhabitationCandidate for Abyss—MkAbyss—and both its term
and type constraints are satisfiable, so we'd need to check if `nonVoid Abyss`
returns False... and now we've entered an infinite loop!

To avoid this sort of conundrum, `nonVoid` uses a simple test to detect the
presence of recursive types (through `checkRecTc`), and if recursion is
detected, we bail out and conservatively assume that the type is inhabited by
some terminating value. This avoids infinite loops at the expense of making
the coverage checker incomplete with respect to functions like
stareIntoTheAbyss above. Then again, the same problem occurs with recursive
newtypes, like in the following code:

.. code-block:: haskell

  newtype Chasm = MkChasm Chasm

.. code-block:: haskell

  gazeIntoTheChasm :: Chasm -> a
  gazeIntoTheChasm x = case x of {} -- Erroneously warned as non-exhaustive

So this limitation is somewhat understandable.

Note that even with this recursion detection, there is still a possibility that
`nonVoid` can run in exponential time. Consider the following data type:

.. code-block:: haskell

  data T = MkT !T !T !T

If we call `nonVoid` on each of its fields, that will require us to once again
check if `MkT` is inhabitable in each of those three fields, which in turn will
require us to check if `MkT` is inhabitable again... As you can see, the
branching factor adds up quickly, and if the recursion depth limit is, say,
100, then `nonVoid T` will effectively take forever.

To mitigate this, we check the branching factor every time we are about to call
`nonVoid` on a list of strict argument types. If the branching factor exceeds 1
(i.e., if there is potential for exponential runtime), then we limit the
maximum recursion depth to 1 to mitigate the problem. If the branching factor
is exactly 1 (i.e., we have a linear chain instead of a tree), then it's okay
to stick with a larger maximum recursion depth.

Another microoptimization applies to data types like this one:

.. code-block:: haskell

  data S a = ![a] !T

Even though there is a strict field of type [a], it's quite silly to call
nonVoid on it, since it's "obvious" that it is inhabitable. To make this
intuition formal, we say that a type is definitely inhabitable (DI) if:

  * It has at least one constructor C such that:
    1. C has no equality constraints (since they might be unsatisfiable)
    2. C has no strict argument types (since they might be uninhabitable)

It's relatively cheap to cheap if a type is DI, so before we call `nonVoid`
on a list of strict argument types, we filter out all of the DI ones.


Note [Filtering out non-matching COMPLETE sets]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Currently, conlikes in a COMPLETE set are simply grouped by the
type constructor heading the return type. This is nice and simple, but it does
mean that there are scenarios when a COMPLETE set might be incompatible with
the type of a scrutinee. For instance, consider (from #14135):

.. code-block:: haskell

  data Foo a = Foo1 a | Foo2 a

.. code-block:: haskell

  pattern MyFoo2 :: Int -> Foo Int
  pattern MyFoo2 i = Foo2 i

.. code-block:: haskell

  {-# COMPLETE Foo1, MyFoo2 #-}

.. code-block:: haskell

  f :: Foo a -> a
  f (Foo1 x) = x

`f` has an incomplete pattern-match, so when choosing which constructors to
report as unmatched in a warning, GHC must choose between the original set of
data constructors {Foo1, Foo2} and the COMPLETE set {Foo1, MyFoo2}. But observe
that GHC shouldn't even consider the COMPLETE set as a possibility: the return
type of MyFoo2, Foo Int, does not match the type of the scrutinee, Foo a, since
there's no substitution `s` such that s(Foo Int) = Foo a.

To ensure that GHC doesn't pick this COMPLETE set, it checks each pattern
synonym constructor's return type matches the type of the scrutinee, and if one
doesn't, then we remove the whole COMPLETE set from consideration.

One might wonder why GHC only checks /pattern synonym/ constructors, and not
/data/ constructors as well. The reason is because that the type of a
GADT constructor very well may not match the type of a scrutinee, and that's
OK. Consider this example (from #14059):

.. code-block:: haskell

  data SBool (z :: Bool) where
    SFalse :: SBool False
    STrue  :: SBool True

.. code-block:: haskell

  pattern STooGoodToBeTrue :: forall (z :: Bool). ()
                           => z ~ True
                           => SBool z
  pattern STooGoodToBeTrue = STrue
  {-# COMPLETE SFalse, STooGoodToBeTrue #-}

.. code-block:: haskell

  wobble :: SBool z -> Bool
  wobble STooGoodToBeTrue = True

In the incomplete pattern match for `wobble`, we /do/ want to warn that SFalse
should be matched against, even though its type, SBool False, does not match
the scrutinee type, SBool z.
-----------------------------------------------------------------------


Note [Coverage checking and existential tyvars]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GHC's implementation of the pattern-match coverage algorithm (as described in
the GADTs Meet Their Match paper) must take some care to emit enough type
constraints when handling data constructors with exisentially quantified type
variables. To better explain what the challenge is, consider a constructor K
of the form:

.. code-block:: haskell

  K @e_1 ... @e_m ev_1 ... ev_v ty_1 ... ty_n :: T u_1 ... u_p

Where:

* e_1, ..., e_m are the existentially bound type variables.
* ev_1, ..., ev_v are evidence variables, which may inhabit a dictionary type
  (e.g., Eq) or an equality constraint (e.g., e_1 ~ Int).
* ty_1, ..., ty_n are the types of K's fields.
* T u_1 ... u_p is the return type, where T is the data type constructor, and
  u_1, ..., u_p are the universally quantified type variables.

In the ConVar case, the coverage algorithm will have in hand the constructor
K as well as a pattern variable (pv :: T PV_1 ... PV_p), where PV_1, ..., PV_p
are some types that instantiate u_1, ... u_p. The idea is that we should
substitute PV_1 for u_1, ..., and PV_p for u_p when forming a PmCon (the
mkOneConFull function accomplishes this) and then hand this PmCon off to the
ConCon case.

The presence of existentially quantified type variables adds a significant
wrinkle. We always grab e_1, ..., e_m from the definition of K to begin with,
but we don't want them to appear in the final PmCon, because then
calling (mkOneConFull K) for other pattern variables might reuse the same
existential tyvars, which is certainly wrong.

Previously, GHC's solution to this wrinkle was to always create fresh names
for the existential tyvars and put them into the PmCon. This works well for
many cases, but it can break down if you nest GADT pattern matches in just
the right way. For instance, consider the following program:

.. code-block:: haskell

    data App f a where
      App :: f a -> App f (Maybe a)

.. code-block:: haskell

    data Ty a where
      TBool :: Ty Bool
      TInt  :: Ty Int

.. code-block:: haskell

    data T f a where
      C :: T Ty (Maybe Bool)

.. code-block:: haskell

    foo :: T f a -> App f a -> ()
    foo C (App TBool) = ()

foo is a total program, but with the previous approach to handling existential
tyvars, GHC would mark foo's patterns as non-exhaustive.

When foo is desugared to Core, it looks roughly like so:

.. code-block:: haskell

    foo @f @a (C co1 _co2) (App @a1 _co3 (TBool |> co1)) = ()

(Where `a1` is an existential tyvar.)

That, in turn, is processed by the coverage checker to become:

.. code-block:: haskell

    foo @f @a (C co1 _co2) (App @a1 _co3 (pmvar123 :: f a1))
      | TBool <- pmvar123 |> co1
      = ()

Note that the type of pmvar123 is `f a1`—this will be important later.

Now, we proceed with coverage-checking as usual. When we come to the
ConVar case for App, we create a fresh variable `a2` to represent its
existential tyvar. At this point, we have the equality constraints
`(a ~ Maybe a2, a ~ Maybe Bool, f ~ Ty)` in scope.

However, when we check the guard, it will use the type of pmvar123, which is
`f a1`. Thus, when considering if pmvar123 can match the constructor TInt,
it will generate the constraint `a1 ~ Int`. This means our final set of
equality constraints would be:

.. code-block:: haskell

    f  ~ Ty
    a  ~ Maybe Bool
    a  ~ Maybe a2
    a1 ~ Int

Which is satisfiable! Freshening the existential tyvar `a` to `a2` doomed us,
because GHC is unable to relate `a2` to `a1`, which really should be the same
tyvar.

Luckily, we can avoid this pitfall. Recall that the ConVar case was where we
generated a PmCon with too-fresh existentials. But after ConVar, we have the
ConCon case, which considers whether each constructor of a particular data type
can be matched on in a particular spot.

In the case of App, when we get to the ConCon case, we will compare our
original App PmCon (from the source program) to the App PmCon created from the
ConVar case. In the former PmCon, we have `a1` in hand, which is exactly the
existential tyvar we want! Thus, we can force `a1` to be the same as `a2` here
by emitting an additional `a1 ~ a2` constraint. Now our final set of equality
constraints will be:

.. code-block:: haskell

    f  ~ Ty
    a  ~ Maybe Bool
    a  ~ Maybe a2
    a1 ~ Int
    a1 ~ a2

Which is unsatisfiable, as we desired, since we now have that
Int ~ a1 ~ a2 ~ Bool.

In general, App might have more than one constructor, in which case we
couldn't reuse the existential tyvar for App for a different constructor. This
means that we can only use this trick in ConCon when the constructors are the
same. But this is fine, since this is the only scenario where this situation
arises in the first place!
----------------------------------------------------------------------------


Note [Type and Term Equality Propagation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When checking a match it would be great to have all type and term information
available so we can get more precise results. For this reason we have functions
`addDictsDs' and `addTmCsDs' in PmMonad that store in the environment type and
term constraints (respectively) as we go deeper.

The type constraints we propagate inwards are collected by `collectEvVarsPats'
in HsPat.hs. This handles bug #4139 ( see example
  https://ghc.haskell.org/trac/ghc/attachment/ticket/4139/GADTbug.hs )
where this is needed.

For term equalities we do less, we just generate equalities for HsCase. For
example we accurately give 2 redundancy warnings for the marked cases:

f :: [a] -> Bool
f x = case x of

.. code-block:: haskell

  []    -> case x of        -- brings (x ~ []) in scope
             []    -> True
             (_:_) -> False -- can't happen

.. code-block:: haskell

  (_:_) -> case x of        -- brings (x ~ (_:_)) in scope
             (_:_) -> True
             []    -> False -- can't happen

Functions `genCaseTmCs1' and `genCaseTmCs2' are responsible for generating
these constraints.


Note [Literals in PmPat]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
Instead of translating a literal to a variable accompanied with a guard, we
treat them like constructor patterns. The following example from
"./libraries/base/GHC/IO/Encoding.hs" shows why:

mkTextEncoding' :: CodingFailureMode -> String -> IO TextEncoding
mkTextEncoding' cfm enc = case [toUpper c | c <- enc, c /= '-'] of
    "UTF8"    -> return $ UTF8.mkUTF8 cfm
    "UTF16"   -> return $ UTF16.mkUTF16 cfm
    "UTF16LE" -> return $ UTF16.mkUTF16le cfm
    ...

Each clause gets translated to a list of variables with an equal number of
guards. For every guard we generate two cases (equals True/equals False) which
means that we generate 2^n cases to feed the oracle with, where n is the sum of
the length of all strings that appear in the patterns. For this particular
example this means over 2^40 cases. Instead, by representing them like with
constructor we get the following:
  1. We exploit the common prefix with our representation of VSAs
  2. We prune immediately non-reachable cases
     (e.g. False == (x == "U"), True == (x == "U"))



Note [Translating As Patterns]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Instead of translating x@p as:  x (p <- x)
we instead translate it as:     p (x <- coercePattern p)
for performance reasons. For example:

.. code-block:: haskell

  f x@True  = 1
  f y@False = 2

Gives the following with the first translation:

.. code-block:: haskell

  x |> {x == False, x == y, y == True}

If we use the second translation we get an empty set, independently of the
oracle. Since the pattern `p' may contain guard patterns though, it cannot be
used as an expression. That's why we call `coercePatVec' to drop the guard and
`vaToPmExpr' to transform the value abstraction to an expression in the
guard pattern (value abstractions are a subset of expressions). We keep the
guards in the first pattern `p' though.


%************************************************************************
%*                                                                      *
      Pretty printing of exhaustiveness/redundancy check warnings
%*                                                                      *
%************************************************************************


Note [Inaccessible warnings for record updates]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider (#12957)
  data T a where
    T1 :: { x :: Int } -> T Bool
    T2 :: { x :: Int } -> T a
    T3 :: T a

.. code-block:: haskell

  f :: T Char -> T a
  f r = r { x = 3 }

The desugarer will (conservatively generate a case for T1 even though
it's impossible:
  f r = case r of
          T1 x -> T1 3   -- Inaccessible branch
          T2 x -> T2 3
          _    -> error "Missing"

We don't want to warn about the inaccessible branch because the programmer
didn't put it there!  So we filter out the warning here.


Note [Representation of Term Equalities]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In the paper, term constraints always take the form (x ~ e). Of course, a more
general constraint of the form (e1 ~ e1) can always be transformed to an
equivalent set of the former constraints, by introducing a fresh, intermediate
variable: { y ~ e1, y ~ e1 }. Yet, implementing this representation gave rise
to #11160 (incredibly bad performance for literal pattern matching). Two are
the main sources of this problem (the actual problem is how these two interact
with each other):

1. Pattern matching on literals generates twice as many constraints as needed.
   Consider the following (tests/ghci/should_run/ghcirun004):

.. code-block:: haskell

    foo :: Int -> Int
    foo 1    = 0
    ...
    foo 5000 = 4999

.. code-block:: haskell

   The covered and uncovered set *should* look like:
     U0 = { x |> {} }

.. code-block:: haskell

     C1  = { 1  |> { x ~ 1 } }
     U1  = { x  |> { False ~ (x ~ 1) } }
     ...
     C10 = { 10 |> { False ~ (x ~ 1), .., False ~ (x ~ 9), x ~ 10 } }
     U10 = { x  |> { False ~ (x ~ 1), .., False ~ (x ~ 9), False ~ (x ~ 10) } }
     ...

.. code-block:: haskell

     If we replace { False ~ (x ~ 1) } with { y ~ False, y ~ (x ~ 1) }
     we get twice as many constraints. Also note that half of them are just the
     substitution [x |-> False].

2. The term oracle (`tmOracle` in deSugar/TmOracle) uses equalities of the form
   (x ~ e) as substitutions [x |-> e]. More specifically, function
   `extendSubstAndSolve` applies such substitutions in the residual constraints
   and partitions them in the affected and non-affected ones, which are the new
   worklist. Essentially, this gives quadradic behaviour on the number of the
   residual constraints. (This would not be the case if the term oracle used
   mutable variables but, since we use it to handle disjunctions on value set
   abstractions (`Union` case), we chose a pure, incremental interface).

Now the problem becomes apparent (e.g. for clause 300):
  * Set U300 contains 300 substituting constraints [y_i |-> False] and 300
    constraints that we know that will not reduce (stay in the worklist).
  * To check for consistency, we apply the substituting constraints ONE BY ONE
    (since `tmOracle` is called incrementally, it does not have all of them
    available at once). Hence, we go through the (non-progressing) constraints
    over and over, achieving over-quadradic behaviour.

If instead we allow constraints of the form (e ~ e),
  * All uncovered sets Ui contain no substituting constraints and i
    non-progressing constraints of the form (False ~ (x ~ lit)) so the oracle
    behaves linearly.
  * All covered sets Ci contain exactly (i-1) non-progressing constraints and
    a single substituting constraint. So the term oracle goes through the
    constraints only once.

The performance improvement becomes even more important when more arguments are
involved.
Debugging Infrastructre

