`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/simplCore/CSE.hs>`_

compiler/simplCore/CSE.hs
=========================


Note [Shadowing]
~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/simplCore/CSE.hs#L57>`__

We have to be careful about shadowing.
For example, consider
        f = \x -> let y = x+x in
                      h = \x -> x+x
                  in ...

Here we must *not* do CSE on the inner x+x!  The simplifier used to guarantee no
shadowing, but it doesn't any more (it proved too hard), so we clone as we go.
We can simply add clones to the substitution already described.



Note [CSE for bindings]
~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/simplCore/CSE.hs#L70>`__

Let-bindings have two cases, implemented by addBinding.

* SUBSTITUTE: applies when the RHS is a variable

::

     let x = y in ...(h x)....

::

  Here we want to extend the /substitution/ with x -> y, so that the
  (h x) in the body might CSE with an enclosing (let v = h y in ...).
  NB: the substitution maps InIds, so we extend the substitution with
      a binding for the original InId 'x'

::

  How can we have a variable on the RHS? Doesn't the simplifier inline them?

    - First, the original RHS might have been (g z) which has CSE'd
      with an enclosing (let y = g z in ...).  This is super-important.
      See #5996:
         x1 = C a b
         x2 = C x1 b
         y1 = C a b
         y2 = C y1 b
      Here we CSE y1's rhs to 'x1', and then we must add (y1->x1) to
      the substitution so that we can CSE the binding for y2.

    - Second, we use addBinding for case expression scrutinees too;
      see Note [CSE for case expressions]

* EXTEND THE REVERSE MAPPING: applies in all other cases

::

     let x = h y in ...(h y)...

::

  Here we want to extend the /reverse mapping (cs_map)/ so that
  we CSE the (h y) call to x.

::

  Note that we use EXTEND even for a trivial expression, provided it
  is not a variable or literal. In particular this /includes/ type
  applications. This can be important (#13156); e.g.
     case f @ Int of { r1 ->
     case f @ Int of { r2 -> ...
  Here we want to common-up the two uses of (f @ Int) so we can
  remove one of the case expressions.

::

  See also Note [Corner case for case expressions] for another
  reason not to use SUBSTITUTE for all trivial expressions.

Notice that
  - The SUBSTITUTE situation extends the substitution (cs_subst)
  - The EXTEND situation extends the reverse mapping (cs_map)

Notice also that in the SUBSTITUTE case we leave behind a binding
  x = y
even though we /also/ carry a substitution x -> y.  Can we just drop
the binding instead?  Well, not at top level! See SimplUtils
Note [Top level and postInlineUnconditionally]; and in any case CSE
applies only to the /bindings/ of the program, and we leave it to the
simplifier to propate effects to the RULES.  Finally, it doesn't seem
worth the effort to discard the nested bindings because the simplifier
will do it next.



Note [CSE for case expressions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/simplCore/CSE.hs#L130>`__

Consider
  case scrut_expr of x { ...alts... }
This is very like a strict let-binding
  let !x = scrut_expr in ...
So we use (addBinding x scrut_expr) to process scrut_expr and x, and as a
result all the stuff under Note [CSE for bindings] applies directly.

For example:

* Trivial scrutinee
     f = \x -> case x of wild {
                 (a:as) -> case a of wild1 {
                             (p,q) -> ...(wild1:as)...

::

  Here, (wild1:as) is morally the same as (a:as) and hence equal to
  wild. But that's not quite obvious.  In the rest of the compiler we
  want to keep it as (wild1:as), but for CSE purpose that's a bad
  idea.

::

  By using addBinding we add the binding (wild1 -> a) to the substitution,
  which does exactly the right thing.

::

  (Notice this is exactly backwards to what the simplifier does, which
  is to try to replaces uses of 'a' with uses of 'wild1'.)

::

  This is the main reason that addBinding is called with a trivial rhs.

* Non-trivial scrutinee
     case (f x) of y { pat -> ...let z = f x in ... }

::

  By using addBinding we'll add (f x :-> y) to the cs_map, and
  thereby CSE the inner (f x) to y.



Note [CSE for INLINE and NOINLINE]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/simplCore/CSE.hs#L165>`__

There are some subtle interactions of CSE with functions that the user
has marked as INLINE or NOINLINE. (Examples from Roman Leshchinskiy.)
Consider

::

        yes :: Int  {-# NOINLINE yes #-}
        yes = undefined

::

        no :: Int   {-# NOINLINE no #-}
        no = undefined

::

        foo :: Int -> Int -> Int  {-# NOINLINE foo #-}
        foo m n = n

::

        {-# RULES "foo/no" foo no = id #-}

::

        bar :: Int -> Int
        bar = foo yes

We do not expect the rule to fire.  But if we do CSE, then we risk
getting yes=no, and the rule does fire.  Actually, it won't because
NOINLINE means that 'yes' will never be inlined, not even if we have
yes=no.  So that's fine (now; perhaps in the olden days, yes=no would
have substituted even if 'yes' was NOINLINE).

But we do need to take care.  Consider

::

        {-# NOINLINE bar #-}
        bar = <rhs>     -- Same rhs as foo

::

        foo = <rhs>

If CSE produces
        foo = bar
then foo will never be inlined to <rhs> (when it should be, if <rhs>
is small).  The conclusion here is this:

::

   We should not add
       <rhs> :-> bar
  to the CSEnv if 'bar' has any constraints on when it can inline;
  that is, if its 'activation' not always active.  Otherwise we
  might replace <rhs> by 'bar', and then later be unable to see that it
  really was <rhs>.

An except to the rule is when the INLINE pragma is not from the user, e.g. from
WorkWrap (see Note [Wrapper activation]). We can tell because noUserInlineSpec
is then true.

Note that we do not (currently) do CSE on the unfolding stored inside
an Id, even if it is a 'stable' unfolding.  That means that when an
unfolding happens, it is always faithful to what the stable unfolding
originally was.



Note [CSE for stable unfoldings]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/simplCore/CSE.hs#L219>`__

Consider
   {-# Unf = Stable (\pq. build blah) #-}
   foo = x

Here 'foo' has a stable unfolding, but its (optimised) RHS is trivial.
(Turns out that this actually happens for the enumFromTo method of
the Integer instance of Enum in GHC.Enum.)  Suppose moreover that foo's
stable unfolding originates from an INLINE or INLINEABLE pragma on foo.
Then we obviously do NOT want to extend the substitution with (foo->x),
because we promised to inline foo as what the user wrote.  See similar
SimplUtils Note [Stable unfoldings and postInlineUnconditionally].

Nor do we want to change the reverse mapping. Suppose we have

::

   {-# Unf = Stable (\pq. build blah) #-}
   foo = <expr>
   bar = <expr>

There could conceivably be merit in rewriting the RHS of bar:
   bar = foo
but now bar's inlining behaviour will change, and importing
modules might see that.  So it seems dodgy and we don't do it.

Stable unfoldings are also created during worker/wrapper when we decide
that a function's definition is so small that it should always inline.
In this case we still want to do CSE (#13340). Hence the use of
isAnyInlinePragma rather than isStableUnfolding.



Note [Corner case for case expressions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/simplCore/CSE.hs#L249>`__

Here is another reason that we do not use SUBSTITUTE for
all trivial expressions. Consider
   case x |> co of (y::Array# Int) { ... }

We do not want to extend the substitution with (y -> x |> co); since y
is of unlifted type, this would destroy the let/app invariant if (x |>
co) was not ok-for-speculation.

But surely (x |> co) is ok-for-speculation, becasue it's a trivial
expression, and x's type is also unlifted, presumably.  Well, maybe
not if you are using unsafe casts.  I actually found a case where we
had
   (x :: HValue) |> (UnsafeCo :: HValue ~ Array# Int)



Note [CSE for join points?]
~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/simplCore/CSE.hs#L265>`__

We must not be naive about join points in CSE:
   join j = e in
   if b then jump j else 1 + e
The expression (1 + jump j) is not good (see Note [Invariants on join points] in
CoreSyn). This seems to come up quite seldom, but it happens (first seen
compiling ppHtml in Haddock.Backends.Xhtml).

We could try and be careful by tracking which join points are still valid at
each subexpression, but since join points aren't allocated or shared, there's
less to gain by trying to CSE them. (#13219)



Note [Look inside join-point binders]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/simplCore/CSE.hs#L278>`__

Another way how CSE for joint points is tricky is

::

  let join foo x = (x, 42)
      join bar x = (x, 42)
  in … jump foo 1 … jump bar 2 …

naively, CSE would turn this into

::

  let join foo x = (x, 42)
      join bar = foo
  in … jump foo 1 … jump bar 2 …

but now bar is a join point that claims arity one, but its right-hand side
is not a lambda, breaking the join-point invariant (this was #15002).

So `cse_bind` must zoom past the lambdas of a join point (using
`collectNBinders`) and resume searching for CSE opportunities only in
the body of the join point.



Note [CSE for recursive bindings]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/simplCore/CSE.hs#L299>`__

Consider
  f = \x ... f....
  g = \y ... g ...
where the "..." are identical.  Could we CSE them?  In full generality
with mutual recursion it's quite hard; but for self-recursive bindings
(which are very common) it's rather easy:

* Maintain a separate cs_rec_map, that maps
      (\f. (\x. ...f...) ) -> f
  Note the \f in the domain of the mapping!

* When we come across the binding for 'g', look up (\g. (\y. ...g...))
  Bingo we get a hit.  So we can replace the 'g' binding with
     g = f

We can't use cs_map for this, because the key isn't an expression of
the program; it's a kind of synthetic key for recursive bindings.



Note [Take care with literal strings]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/simplCore/CSE.hs#L447>`__

Consider this example:

::

  x = "foo"#
  y = "foo"#
  ...x...y...x...y....

We would normally turn this into:

::

  x = "foo"#
  y = x
  ...x...x...x...x....

But this breaks an invariant of Core, namely that the RHS of a top-level binding
of type Addr# must be a string literal, not another variable. See Note
[CoreSyn top-level string literals] in CoreSyn.

For this reason, we special case top-level bindings to literal strings and leave
the original RHS unmodified. This produces:

::

  x = "foo"#
  y = "foo"#
  ...x...x...x...x....

Now 'y' will be discarded as dead code, and we are done.

The net effect is that for the y-binding we want to
  - Use SUBSTITUTE, by extending the substitution with  y :-> x
  - but leave the original binding for y undisturbed

This is done by cse_bind.  I got it wrong the first time (#13367).



Note [Delay inlining after CSE]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/simplCore/CSE.hs#L480>`__

Suppose (#15445) we have
   f,g :: Num a => a -> a
   f x = ...f (x-1).....
   g y = ...g (y-1) ....

and we make some specialisations of 'g', either automatically, or via
a SPECIALISE pragma.  Then CSE kicks in and notices that the RHSs of
'f' and 'g' are identical, so we get
   f x = ...f (x-1)...
   g = f
   {-# RULES g @Int _ = $sg #-}

Now there is terrible danger that, in an importing module, we'll inline
'g' before we have a chance to run its specialisation!

Solution: during CSE, when adding a top-level
  g = f
binding after a "hit" in the CSE cache, add a NOINLINE[2] activation
to it, to ensure it's not inlined right away.

Why top level only?  Because for nested bindings we are already past
phase 2 and will never return there.



Note [Combine case alternatives]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/simplCore/CSE.hs#L609>`__

combineAlts is just a more heavyweight version of the use of
combineIdenticalAlts in SimplUtils.prepareAlts.  The basic idea is
to transform

    DEFAULT -> e1
    K x     -> e1
    W y z   -> e2
===>
   DEFAULT -> e1
   W y z   -> e2

In the simplifier we use cheapEqExpr, because it is called a lot.
But here in CSE we use the full eqExpr.  After all, two alternatives usually
differ near the root, so it probably isn't expensive to compare the full
alternative.  It seems like the same kind of thing that CSE is supposed
to be doing, which is why I put it here.

I acutally saw some examples in the wild, where some inlining made e1 too
big for cheapEqExpr to catch it.

