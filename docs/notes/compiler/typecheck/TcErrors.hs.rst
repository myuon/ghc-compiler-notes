`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcErrors.hs>`_

Note [Deferring coercion errors to runtime]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
While developing, sometimes it is desirable to allow compilation to succeed even
if there are type errors in the code. Consider the following case:

.. code-block:: haskell

  module Main where

.. code-block:: haskell

  a :: Int
  a = 'a'

.. code-block:: haskell

  main = print "b"

Even though `a` is ill-typed, it is not used in the end, so if all that we're
interested in is `main` it is handy to be able to ignore the problems in `a`.

Since we treat type equalities as evidence, this is relatively simple. Whenever
we run into a type mismatch in TcUnify, we normally just emit an error. But it
is always safe to defer the mismatch to the main constraint solver. If we do
that, `a` will get transformed into

.. code-block:: haskell

  co :: Int ~ Char
  co = ...

.. code-block:: haskell

  a :: Int
  a = 'a' `cast` co

The constraint solver would realize that `co` is an insoluble constraint, and
emit an error with `reportUnsolved`. But we can also replace the right-hand side
of `co` with `error "Deferred type error: Int ~ Char"`. This allows the program
to compile, and it will run fine unless we evaluate `a`. This is what
`deferErrorsToRuntime` does.

It does this by keeping track of which errors correspond to which coercion
in TcErrors. TcErrors.reportTidyWanteds does not print the errors
and does not fail if -fdefer-type-errors is on, so that we can continue
compilation. The errors are turned into warnings in `reportUnsolved`.


Note [Suppressing error messages]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The cec_suppress flag says "don't report any errors".  Instead, just create
evidence bindings (as usual).  It's used when more important errors have occurred.

Specifically (see reportWanteds)
  * If there are insoluble Givens, then we are in unreachable code and all bets
    are off.  So don't report any further errors.
  * If there are any insolubles (eg Int~Bool), here or in a nested implication,
    then suppress errors from the simple constraints here.  Sometimes the
    simple-constraint errors are a knock-on effect of the insolubles.

This suppression behaviour is controlled by the Bool flag in
ReportErrorSpec, as used in reportWanteds.

But we need to take care: flags can turn errors into warnings, and we
don't want those warnings to suppress subsequent errors (including
suppressing the essential addTcEvBind for them: #15152). So in
tryReporter we use askNoErrs to see if any error messages were
/actually/ produced; if not, we don't switch on suppression.

A consequence is that warnings never suppress warnings, so turning an
error into a warning may allow subsequent warnings to appear that were
previously suppressed.   (e.g. partial-sigs/should_fail/T14584)


Note [Redundant constraints in instance decls]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For instance declarations, we don't report unused givens if
they can give rise to improvement.  Example (#10100):
    class Add a b ab | a b -> ab, a ab -> b
    instance Add Zero b b
    instance Add a b ab => Add (Succ a) b (Succ ab)
The context (Add a b ab) for the instance is clearly unused in terms
of evidence, since the dictionary has no fields.  But it is still
needed!  With the context, a wanted constraint
   Add (Succ Zero) beta (Succ Zero)
we will reduce to (Add Zero beta Zero), and thence we get beta := Zero.
But without the context we won't find beta := Zero.

This only matters in instance declarations..


Note [Given errors]
~~~~~~~~~~~~~~~~~~~~~~
Given constraints represent things for which we have (or will have)
evidence, so they aren't errors.  But if a Given constraint is
insoluble, this code is inaccessible, and we might want to at least
warn about that.  A classic case is

.. code-block:: haskell

   data T a where
     T1 :: T Int
     T2 :: T a
     T3 :: T Bool

.. code-block:: haskell

   f :: T Int -> Bool
   f T1 = ...
   f T2 = ...
   f T3 = ...  -- We want to report this case as inaccessible

We'd like to point out that the T3 match is inaccessible. It
will have a Given constraint [G] Int ~ Bool.

But we don't want to report ALL insoluble Given constraints.  See Trac
#12466 for a long discussion.  For example, if we aren't careful
we'll complain about
   f :: ((Int ~ Bool) => a -> a) -> Int
which arguably is OK.  It's more debatable for
   g :: (Int ~ Bool) => Int -> Int
but it's tricky to distinguish these cases so we don't report
either.

The bottom line is this: has_gadt_match looks for an enclosing
pattern match which binds some equality constraints.  If we
find one, we report the insoluble Given.


Note [Always warn with -fdefer-type-errors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When -fdefer-type-errors is on we warn about *all* type errors, even
if cec_suppress is on.  This can lead to a lot more warnings than you
would get errors without -fdefer-type-errors, but if we suppress any of
them you might get a runtime error that wasn't warned about at compile
time.

This is an easy design choice to change; just flip the order of the
first two equations for maybeReportError

To be consistent, we should also report multiple warnings from a single
location in mkGroupReporter, when -fdefer-type-errors is on.  But that
is perhaps a bit *over*-consistent! Again, an easy choice to change.

With #10283, you can now opt out of deferred type error warnings.



Note [Deferred errors for coercion holes]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we need to defer a type error where the destination for the evidence
is a coercion hole. We can't just put the error in the hole, because we can't
make an erroneous coercion. (Remember that coercions are erased for runtime.)
Instead, we invent a new EvVar, bind it to an error and then make a coercion
from that EvVar, filling the hole with that coercion. Because coercions'
types are unlifted, the error is guaranteed to be hit before we get to the
coercion.



Note [Do not report derived but soluble errors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The wc_simples include Derived constraints that have not been solved,
but are not insoluble (in that case they'd be reported by 'report1').
We do not want to report these as errors:

* Superclass constraints. If we have an unsolved [W] Ord a, we'll also have
  an unsolved [D] Eq a, and we do not want to report that; it's just noise.

* Functional dependencies.  For givens, consider
      class C a b | a -> b
      data T a where
         MkT :: C a d => [d] -> T a
      f :: C a b => T a -> F Int
      f (MkT xs) = length xs
  Then we get a [D] b~d.  But there *is* a legitimate call to
  f, namely   f (MkT [True]) :: T Bool, in which b=d.  So we should
  not reject the program.

.. code-block:: haskell

  For wanteds, something similar
      data T a where
        MkT :: C Int b => a -> b -> T a
      g :: C Int c => c -> ()
      f :: T a -> ()
      f (MkT x y) = g x
  Here we get [G] C Int b, [W] C Int a, hence [D] a~b.
  But again f (MkT True True) is a legitimate call.

(We leave the Deriveds in wc_simple until reportErrors, so that we don't lose
derived superclasses between iterations of the solver.)

For functional dependencies, here is a real example,
stripped off from libraries/utf8-string/Codec/Binary/UTF8/Generic.hs

.. code-block:: haskell

  class C a b | a -> b
  g :: C a b => a -> b -> ()
  f :: C a b => a -> b -> ()
  f xa xb =
      let loop = g xa
      in loop xb

We will first try to infer a type for loop, and we will succeed:
    C a b' => b' -> ()
Subsequently, we will type check (loop xb) and all is good. But,
recall that we have to solve a final implication constraint:
    C a b => (C a b' => .... cts from body of loop .... ))
And now we have a problem as we will generate an equality b ~ b' and fail to
solve it.




Note [Constraints include ...]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'givenConstraintsMsg' returns the "Constraints include ..." message enabled by
-fshow-hole-constraints. For example, the following hole:

.. code-block:: haskell

    foo :: (Eq a, Show a) => a -> String
    foo x = _

would generate the message:

.. code-block:: haskell

    Constraints include
      Eq a (from foo.hs:1:1-36)
      Show a (from foo.hs:1:1-36)

Constraints are displayed in order from innermost (closest to the hole) to
outermost. There's currently no filtering or elimination of duplicates.




Note [OutOfScope exact matches]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When constructing an out-of-scope error message, we not only generate a list of
possible in-scope alternatives but also search for an exact, unambiguous match
in a later inter-splice group.  If we find such a match, we report its presence
(and indirectly, its scope) in the message.  For example, if a module A contains
the following declarations,

.. code-block:: haskell

   foo :: Int
   foo = x

.. code-block:: haskell

   $(return [])  -- Empty top-level splice

.. code-block:: haskell

   x :: Int
   x = 23

we will issue an error similar to

.. code-block:: haskell

   A.hs:6:7: error:
       • Variable not in scope: x :: Int
       • ‘x’ (line 11) is not in scope before the splice on line 8

By providing information about the match, we hope to clarify why declaring a
variable after a top-level splice but using it before the splice generates an
out-of-scope error (a situation which is often confusing to Haskell newcomers).

Note that if we find multiple exact matches to the out-of-scope variable
(hereafter referred to as x), we report nothing.  Such matches can only be
duplicate record fields, as the presence of any other duplicate top-level
declarations would have already halted compilation.  But if these record fields
are declared in a later inter-splice group, then so too are their corresponding
types.  Thus, these types must not occur in the inter-splice group containing x
(any unknown types would have already been reported), and so the matches to the
record fields are most likely coincidental.

One oddity of the exact match portion of the error message is that we specify
where the match to x is NOT in scope.  Why not simply state where the match IS
in scope?  It most cases, this would be just as easy and perhaps a little
clearer for the user.  But now consider the following example:

.. code-block:: haskell

    {-# LANGUAGE TemplateHaskell #-}

.. code-block:: haskell

    module A where

.. code-block:: haskell

    import Language.Haskell.TH
    import Language.Haskell.TH.Syntax

.. code-block:: haskell

    foo = x

.. code-block:: haskell

    $(do -------------------------------------------------
        ds <- [d| ok1 = x
                |]
        addTopDecls ds
        return [])

.. code-block:: haskell

    bar = $(do
            ds <- [d| x = 23
                      ok2 = x
                    |]
            addTopDecls ds
            litE $ stringL "hello")

.. code-block:: haskell

    $(return []) -----------------------------------------

.. code-block:: haskell

    ok3 = x

Here, x is out-of-scope in the declaration of foo, and so we report

.. code-block:: haskell

    A.hs:8:7: error:
        • Variable not in scope: x
        • ‘x’ (line 16) is not in scope before the splice on lines 10-14

If we instead reported where x IS in scope, we would have to state that it is in
scope after the second top-level splice as well as among all the top-level
declarations added by both calls to addTopDecls.  But doing so would not only
add complexity to the code but also overwhelm the user with unneeded
information.

The logic which determines where x is not in scope is straightforward: it simply
finds the last top-level splice which occurs after x but before (or at) the
match to x (assuming such a splice exists).  In most cases, the check that the
splice occurs after x acts only as a sanity check.  For example, when the match
to x is a non-TH top-level declaration and a splice S occurs before the match,
then x must precede S; otherwise, it would be in scope.  But when dealing with
addTopDecls, this check serves a practical purpose.  Consider the following
declarations:

.. code-block:: haskell

    $(do
        ds <- [d| ok = x
                  x = 23
                |]
        addTopDecls ds
        return [])

.. code-block:: haskell

    foo = x

In this case, x is not in scope in the declaration for foo.  Since x occurs
AFTER the splice containing the match, the logic does not find any splices after
x but before or at its match, and so we report nothing about x's scope.  If we
had not checked whether x occurs before the splice, we would have instead
reported that x is not in scope before the splice.  While correct, such an error
message is more likely to confuse than to enlighten.


Note [Inaccessible code]
~~~~~~~~~~~~~~~~~~~~~~~~
Consider
   data T a where
     T1 :: T a
     T2 :: T Bool

.. code-block:: haskell

   f :: (a ~ Int) => T a -> Int
   f T1 = 3
   f T2 = 4   -- Unreachable code

Here the second equation is unreachable. The original constraint
(a~Int) from the signature gets rewritten by the pattern-match to
(Bool~Int), so the danger is that we report the error as coming from
the *signature* (#7293).  So, for Given errors we replace the
env (and hence src-loc) on its CtLoc with that from the immediately
enclosing implication.



Note [Error messages for untouchables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider (#9109)
  data G a where { GBool :: G Bool }
  foo x = case x of GBool -> True

Here we can't solve (t ~ Bool), where t is the untouchable result
meta-var 't', because of the (a ~ Bool) from the pattern match.
So we infer the type
   f :: forall a t. G a -> t
making the meta-var 't' into a skolem.  So when we come to report
the unsolved (t ~ Bool), t won't look like an untouchable meta-var
any more.  So we don't assert that it is.
Don't have multiple equality errors from the same location
E.g.   (Int,Bool) ~ (Bool,Int)   one error will do!


Note [Suppress redundant givens during error reporting]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When GHC is unable to solve a constraint and prints out an error message, it
will print out what given constraints are in scope to provide some context to
the programmer. But we shouldn't print out /every/ given, since some of them
are not terribly helpful to diagnose type errors. Consider this example:

.. code-block:: haskell

  foo :: Int :~: Int -> a :~: b -> a :~: c
  foo Refl Refl = Refl

When reporting that GHC can't solve (a ~ c), there are two givens in scope:
(Int ~ Int) and (a ~ b). But (Int ~ Int) is trivially soluble (i.e.,
redundant), so it's not terribly useful to report it in an error message.
To accomplish this, we discard any Implications that do not bind any
equalities by filtering the `givens` selected in `misMatchOrCND` (based on
the `ic_no_eqs` field of the Implication).

But this is not enough to avoid all redundant givens! Consider this example,
from #15361:

.. code-block:: haskell

  goo :: forall (a :: Type) (b :: Type) (c :: Type).
         a :~~: b -> a :~~: c
  goo HRefl = HRefl

Matching on HRefl brings the /single/ given (* ~ *, a ~ b) into scope.
The (* ~ *) part arises due the kinds of (:~~:) being unified. More
importantly, (* ~ *) is redundant, so we'd like not to report it. However,
the Implication (* ~ *, a ~ b) /does/ bind an equality (as reported by its
ic_no_eqs field), so the test above will keep it wholesale.

To refine this given, we apply mkMinimalBySCs on it to extract just the (a ~ b)
part. This works because mkMinimalBySCs eliminates reflexive equalities in
addition to superclasses (see Note [Remove redundant provided dicts]
in TcPatSyn).


Note [Insoluble occurs check wins]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider [G] a ~ [a],  [W] a ~ [a] (#13674).  The Given is insoluble
so we don't use it for rewriting.  The Wanted is also insoluble, and
we don't solve it from the Given.  It's very confusing to say
    Cannot solve a ~ [a] from given constraints a ~ [a]

And indeed even thinking about the Givens is silly; [W] a ~ [a] is
just as insoluble as Int ~ Bool.

Conclusion: if there's an insoluble occurs check (isInsolubleOccursCheck)
then report it first.

(NB: there are potentially-soluble ones, like (a ~ F a b), and we don't
want to be as draconian with them.)



Note [Expanding type synonyms to make types similar]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In type error messages, if -fprint-expanded-types is used, we want to expand
type synonyms to make expected and found types as similar as possible, but we
shouldn't expand types too much to make type messages even more verbose and
harder to understand. The whole point here is to make the difference in expected
and found types clearer.

`expandSynonymsToMatch` does this, it takes two types, and expands type synonyms
only as much as necessary. Given two types t1 and t2:

  * If they're already same, it just returns the types.

  * If they're in form `C1 t1_1 .. t1_n` and `C2 t2_1 .. t2_m` (C1 and C2 are
    type constructors), it expands C1 and C2 if they're different type synonyms.
    Then it recursively does the same thing on expanded types. If C1 and C2 are
    same, then it applies the same procedure to arguments of C1 and arguments of
    C2 to make them as similar as possible.

.. code-block:: haskell

    Most important thing here is to keep number of synonym expansions at
    minimum. For example, if t1 is `T (T3, T5, Int)` and t2 is `T (T5, T3,
    Bool)` where T5 = T4, T4 = T3, ..., T1 = X, it returns `T (T3, T3, Int)` and
    `T (T3, T3, Bool)`.

  * Otherwise types don't have same shapes and so the difference is clearly
    visible. It doesn't do any expansions and show these types.

Note that we only expand top-layer type synonyms. Only when top-layer
constructors are the same we start expanding inner type synonyms.

Suppose top-layer type synonyms of t1 and t2 can expand N and M times,
respectively. If their type-synonym-expanded forms will meet at some point (i.e.
will have same shapes according to `sameShapes` function), it's possible to find
where they meet in O(N+M) top-layer type synonym expansions and O(min(N,M))
comparisons. We first collect all the top-layer expansions of t1 and t2 in two
lists, then drop the prefix of the longer list so that they have same lengths.
Then we search through both lists in parallel, and return the first pair of
types that have same shapes. Inner types of these two types with same shapes
are then expanded using the same algorithm.

In case they don't meet, we return the last pair of types in the lists, which
has top-layer type synonyms completely expanded. (in this case the inner types
are not expanded at all, as the current form already shows the type error)


Note [Suggest adding a type signature]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The OutsideIn algorithm rejects GADT programs that don't have a principal
type, and indeed some that do.  Example:
   data T a where
     MkT :: Int -> T Int

.. code-block:: haskell

   f (MkT n) = n

Does this have type f :: T a -> a, or f :: T a -> Int?
The error that shows up tends to be an attempt to unify an
untouchable type variable.  So suggestAddSig sees if the offending
type variable is bound by an *inferred* signature, and suggests
adding a declared signature instead.

This initially came up in #8968, concerning pattern synonyms.



Note [Disambiguating (X ~ X) errors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
See #8278



Note [Reporting occurs-check errors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Given (a ~ [a]), if 'a' is a rigid type variable bound by a user-supplied
type signature, then the best thing is to report that we can't unify
a with [a], because a is a skolem variable.  That avoids the confusing
"occur-check" error message.

But nowadays when inferring the type of a function with no type signature,
even if there are errors inside, we still generalise its signature and
carry on. For example
   f x = x:x
Here we will infer something like
   f :: forall a. a -> [a]
with a deferred error of (a ~ [a]).  So in the deferred unsolved constraint
'a' is now a skolem, but not one bound by the programmer in the context!
Here we really should report an occurs check.

So isUserSkolem distinguishes the two.



Note [Non-injective type functions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It's very confusing to get a message like
     Couldn't match expected type `Depend s'
            against inferred type `Depend s1'
so mkTyFunInfoMsg adds:
       NB: `Depend' is type function, and hence may not be injective

Warn of loopy local equalities that were dropped.




Note [Report candidate instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If we have an unsolved (Num Int), where `Int` is not the Prelude Int,
but comes from some other module, then it may be helpful to point out
that there are some similarly named instances elsewhere.  So we get
something like
    No instance for (Num Int) arising from the literal ‘3’
    There are instances for similar types:
      instance Num GHC.Types.Int -- Defined in ‘GHC.Num’
Discussion in #9611.

Note [Highlighting ambiguous type variables]
~-------------------------------------------
When we encounter ambiguous type variables (i.e. type variables
that remain metavariables after type inference), we need a few more
conditions before we can reason that *ambiguity* prevents constraints
from being solved:
  - We can't have any givens, as encountering a typeclass error
    with given constraints just means we couldn't deduce
    a solution satisfying those constraints and as such couldn't
    bind the type variable to a known type.
  - If we don't have any unifiers, we don't even have potential
    instances from which an ambiguity could arise.
  - Lastly, I don't want to mess with error reporting for
    unknown runtime types so we just fall back to the old message there.
Once these conditions are satisfied, we can safely say that ambiguity prevents
the constraint from being solved.



Note [discardProvCtxtGivens]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
In most situations we call all enclosing implications "useful". There is one
exception, and that is when the constraint that causes the error is from the
"provided" context of a pattern synonym declaration:

.. code-block:: haskell

  pattern Pat :: (Num a, Eq a) => Show a   => a -> Maybe a
             --  required      => provided => type
  pattern Pat x <- (Just x, 4)

When checking the pattern RHS we must check that it does actually bind all
the claimed "provided" constraints; in this case, does the pattern (Just x, 4)
bind the (Show a) constraint.  Answer: no!

But the implication we generate for this will look like
   forall a. (Num a, Eq a) => [W] Show a
because when checking the pattern we must make the required
constraints available, since they are needed to match the pattern (in
this case the literal '4' needs (Num a, Eq a)).

BUT we don't want to suggest adding (Show a) to the "required" constraints
of the pattern synonym, thus:
  pattern Pat :: (Num a, Eq a, Show a) => Show a => a -> Maybe a
It would then typecheck but it's silly.  We want the /pattern/ to bind
the alleged "provided" constraints, Show a.

So we suppress that Implication in discardProvCtxtGivens.  It's
painfully ad-hoc but the truth is that adding it to the "required"
constraints would work.  Suppressing it solves two problems.  First,
we never tell the user that we could not deduce a "provided"
constraint from the "required" context. Second, we never give a
possible fix that suggests to add a "provided" constraint to the
"required" context.

For example, without this distinction the above code gives a bad error
message (showing both problems):

.. code-block:: haskell

  error: Could not deduce (Show a) ... from the context: (Eq a)
         ... Possible fix: add (Show a) to the context of
         the signature for pattern synonym `Pat' ...



Note [Displaying potential instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When showing a list of instances for
  - overlapping instances (show ones that match)
  - no such instance (show ones that could match)
we want to give it a bit of structure.  Here's the plan

* Say that an instance is "in scope" if all of the
  type constructors it mentions are lexically in scope.
  These are the ones most likely to be useful to the programmer.

* Show at most n_show in-scope instances,
  and summarise the rest ("plus 3 others")

* Summarise the not-in-scope instances ("plus 4 not in scope")

* Add the flag -fshow-potential-instances which replaces the
  summary with the full list


Note [Flattening in error message generation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider (C (Maybe (F x))), where F is a type function, and we have
instances
                C (Maybe Int) and C (Maybe a)
Since (F x) might turn into Int, this is an overlap situation, and
indeed (because of flattening) the main solver will have refrained
from solving.  But by the time we get to error message generation, we've
un-flattened the constraint.  So we must *re*-flatten it before looking
up in the instance environment, lest we only report one matching
instance when in fact there are two.

Re-flattening is pretty easy, because we don't need to keep track of
evidence.  We don't re-use the code in TcCanonical because that's in
the TcS monad, and we are in TcM here.



Note [Kind arguments in error messages]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It can be terribly confusing to get an error message like (#9171)

.. code-block:: haskell

    Couldn't match expected type ‘GetParam Base (GetParam Base Int)’
                with actual type ‘GetParam Base (GetParam Base Int)’

The reason may be that the kinds don't match up.  Typically you'll get
more useful information, but not when it's as a result of ambiguity.

To mitigate this, GHC attempts to enable the -fprint-explicit-kinds flag
whenever any error message arises due to a kind mismatch. This means that
the above error message would instead be displayed as:

.. code-block:: haskell

    Couldn't match expected type
                  ‘GetParam @* @k2 @* Base (GetParam @* @* @k2 Base Int)’
                with actual type
                  ‘GetParam @* @k20 @* Base (GetParam @* @* @k20 Base Int)’

Which makes it clearer that the culprit is the mismatch between `k2` and `k20`.


Note [Runtime skolems]
~~~~~~~~~~~~~~~~~~~~~~
We want to give a reasonably helpful error message for ambiguity
arising from *runtime* skolems in the debugger.  These
are created by in RtClosureInspect.zonkRTTIType.


