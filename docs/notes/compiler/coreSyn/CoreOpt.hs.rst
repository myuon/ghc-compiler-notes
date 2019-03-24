`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/coreSyn/CoreOpt.hs>`_

====================
compiler/coreSyn/CoreOpt.hs.rst
====================

Note [The simple optimiser]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
The simple optimiser is a lightweight, pure (non-monadic) function
that rapidly does a lot of simple optimisations, including

  - inlining things that occur just once,
      or whose RHS turns out to be trivial
  - beta reduction
  - case of known constructor
  - dead code elimination

It does NOT do any call-site inlining; it only inlines a function if
it can do so unconditionally, dropping the binding.  It thereby
guarantees to leave no un-reduced beta-redexes.

It is careful to follow the guidance of "Secrets of the GHC inliner",
and in particular the pre-inline-unconditionally and
post-inline-unconditionally story, to do effective beta reduction on
functions called precisely once, without repeatedly optimising the same
expression.  In fact, the simple optimiser is a good example of this
little dance in action; the full Simplifier is a lot more complicated.



Note [Exported Ids and trivial RHSs]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We obviously do not want to unconditionally inline an Id that is exported.
In SimplUtils, Note [Top level and postInlineUnconditionally], we
explain why we don't inline /any/ top-level things unconditionally, even
trivial ones.  But we do here!  Why?  In the simple optimiser

  * We do no rule rewrites
  * We do no call-site inlining

Those differences obviate the reasons for not inlining a trivial rhs,
and increase the benefit for doing so.  So we unconditionally inline trivial
rhss here.



Note [Preserve join-binding arity]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Be careful /not/ to eta-reduce the RHS of a join point, lest we lose
the join-point arity invariant.  #15108 was caused by simplifying
the RHS with simple_opt_expr, which does eta-reduction.  Solution:
simplify the RHS of a join point by simplifying under the lambdas
(which of course should be there).
--------------------


Note [Inline prag in simplOpt]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If there's an INLINE/NOINLINE pragma that restricts the phase in
which the binder can be inlined, we don't inline here; after all,
we don't know what phase we're in.  Here's an example

.. code-block:: haskell

  foo :: Int -> Int -> Int
  {-# INLINE foo #-}
  foo m n = inner m
     where
       {-# INLINE [1] inner #-}
       inner m = m+n

.. code-block:: haskell

  bar :: Int -> Int
  bar n = foo n 1

When inlining 'foo' in 'bar' we want the let-binding for 'inner'
to remain visible until Phase 1



Note [Unfold compulsory unfoldings in LHSs]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When the user writes `RULES map coerce = coerce` as a rule, the rule
will only ever match if simpleOptExpr replaces coerce by its unfolding
on the LHS, because that is the core that the rule matching engine
will find. So do that for everything that has a compulsory
unfolding. Also see Note [Desugaring coerce as cast] in Desugar.

However, we don't want to inline 'seq', which happens to also have a
compulsory unfolding, so we only do this unfolding only for things
that are always-active.  See Note [User-defined RULES for seq] in MkId.



Note [Getting the map/coerce RULE to work]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We wish to allow the "map/coerce" RULE to fire:

.. code-block:: haskell

  {-# RULES "map/coerce" map coerce = coerce #-}

The naive core produced for this is

.. code-block:: haskell

  forall a b (dict :: Coercible * a b).
    map @a @b (coerce @a @b @dict) = coerce @[a] @[b] @dict'

.. code-block:: haskell

  where dict' :: Coercible [a] [b]
        dict' = ...

This matches literal uses of `map coerce` in code, but that's not what we
want. We want it to match, say, `map MkAge` (where newtype Age = MkAge Int)
too. Some of this is addressed by compulsorily unfolding coerce on the LHS,
yielding

.. code-block:: haskell

  forall a b (dict :: Coercible * a b).
    map @a @b (\(x :: a) -> case dict of
      MkCoercible (co :: a ~R# b) -> x |> co) = ...

Getting better. But this isn't exactly what gets produced. This is because
Coercible essentially has ~R# as a superclass, and superclasses get eagerly
extracted during solving. So we get this:

.. code-block:: haskell

  forall a b (dict :: Coercible * a b).
    case Coercible_SCSel @* @a @b dict of
      _ [Dead] -> map @a @b (\(x :: a) -> case dict of
                               MkCoercible (co :: a ~R# b) -> x |> co) = ...

Unfortunately, this still abstracts over a Coercible dictionary. We really
want it to abstract over the ~R# evidence. So, we have Desugar.unfold_coerce,
which transforms the above to (see also Note [Desugaring coerce as cast] in
Desugar)

.. code-block:: haskell

  forall a b (co :: a ~R# b).
    let dict = MkCoercible @* @a @b co in
    case Coercible_SCSel @* @a @b dict of
      _ [Dead] -> map @a @b (\(x :: a) -> case dict of
         MkCoercible (co :: a ~R# b) -> x |> co) = let dict = ... in ...

Now, we need simpleOptExpr to fix this up. It does so by taking three
separate actions:
  1. Inline certain non-recursive bindings. The choice whether to inline
     is made in simple_bind_pair. Note the rather specific check for
     MkCoercible in there.

  2. Stripping case expressions like the Coercible_SCSel one.
     See the `Case` case of simple_opt_expr's `go` function.

  3. Look for case expressions that unpack something that was
     just packed and inline them. This is also done in simple_opt_expr's
     `go` function.

This is all a fair amount of special-purpose hackery, but it's for
a good cause. And it won't hurt other RULES and such that it comes across.




Note [Strictness and join points]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have

.. code-block:: haskell

   let f = \x.  if x>200 then e1 else e1

and we know that f is strict in x.  Then if we subsequently
discover that f is an arity-2 join point, we'll eta-expand it to

.. code-block:: haskell

   let f = \x y.  if x>200 then e1 else e1

and now it's only strict if applied to two arguments.  So we should
adjust the strictness info.

A more common case is when

.. code-block:: haskell

   f = \x. error ".."

and again its arity increases (#15517)


Note [Unfolding DFuns]
~~~~~~~~~~~~~~~~~~~~~~
DFuns look like

.. code-block:: haskell

  df :: forall a b. (Eq a, Eq b) -> Eq (a,b)
  df a b d_a d_b = MkEqD (a,b) ($c1 a b d_a d_b)
                               ($c2 a b d_a d_b)

So to split it up we just need to apply the ops $c1, $c2 etc
to the very same args as the dfun.  It takes a little more work
to compute the type arguments to the dictionary constructor.



Note [DFun arity check]
~~~~~~~~~~~~~~~~~~~~~~~
Here we check that the total number of supplied arguments (inclding
type args) matches what the dfun is expecting.  This may be *less*
than the ordinary arity of the dfun: see Note [DFun unfoldings] in CoreSyn


Note [exprIsLambda_maybe]
~~~~~~~~~~~~~~~~~~~~~~~~~~
exprIsLambda_maybe will, given an expression `e`, try to turn it into the form
`Lam v e'` (returned as `Just (v,e')`). Besides using lambdas, it looks through
casts (using the Push rule), and it unfolds function calls if the unfolding
has a greater arity than arguments are present.

Currently, it is used in Rules.match, and is required to make
"map coerce = coerce" match.


Note [collectBindersPushingCo]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We just look for coercions of form
   <type> -> blah
(and similarly for foralls) to keep this function simple.  We could do
more elaborate stuff, but it'd involve substitution etc.

