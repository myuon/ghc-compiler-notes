`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/rename/RnExpr.hs>`_

compiler/rename/RnExpr.hs
=========================


Note [Deterministic ApplicativeDo and RecursiveDo desugaring]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/rename/RnExpr.hs#L660>`__

Both ApplicativeDo and RecursiveDo need to create tuples not
present in the source text.

For ApplicativeDo we create:

::

  (a,b,c) <- (\c b a -> (a,b,c)) <$>

..

For RecursiveDo we create:

::

  mfix (\ ~(a,b,c) -> do ...; return (a',b',c'))

..

The order of the components in those tuples needs to be stable
across recompilations, otherwise they can get optimized differently
and we end up with incompatible binaries.
To get a stable order we use nameSetElemsStable.
See Note [Deterministic UniqFM] to learn more about nondeterminism.



Note [Failing pattern matches in Stmts]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/rename/RnExpr.hs#L791>`__

Many things desugar to HsStmts including monadic things like `do` and `mdo`
statements, pattern guards, and list comprehensions (see 'HsStmtContext' for an
exhaustive list). How we deal with pattern match failure is context-dependent.

 * In the case of list comprehensions and pattern guards we don't need any 'fail'
   function; the desugarer ignores the fail function field of 'BindStmt' entirely.
 * In the case of monadic contexts (e.g. monad comprehensions, do, and mdo
   expressions) we want pattern match failure to be desugared to the appropriate
   'fail' function (either that of Monad or MonadFail, depending on whether
   -XMonadFailDesugaring is enabled.)

At one point we failed to make this distinction, leading to #11216.



Note [Renaming parallel Stmts]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/rename/RnExpr.hs#L1026>`__

Renaming parallel statements is painful.  Given, say
     [ a+c | a <- as, bs <- bss
           | c <- bs, a <- ds ]
Note that
  (a) In order to report "Defined but not used" about 'bs', we must
      rename each group of Stmts with a thing_inside whose FreeVars
      include at least {a,c}

::

  (b) We want to report that 'a' is illegally bound in both branches

..

::

  (c) The 'bs' in the second group must obviously not be captured by
      the binding in the first group

..

To satisfy (a) we nest the segements.
To satisfy (b) we check for duplicates just before thing_inside.
To satisfy (c) we reset the LocalRdrEnv each time.



Note [Segmenting mdo]
~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/rename/RnExpr.hs#L1304>`__

NB. June 7 2012: We only glom segments that appear in an explicit mdo;
and leave those found in "do rec"'s intact.  See
https://gitlab.haskell.org/ghc/ghc/issues/4148 for the discussion
leading to this design choice.  Hence the test in segmentRecStmts.



Note [Glomming segments]
~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/rename/RnExpr.hs#L1311>`__

Glomming the singleton segments of an mdo into minimal recursive groups.

At first I thought this was just strongly connected components, but
there's an important constraint: the order of the stmts must not change.

Consider
     mdo { x <- ...y...
           p <- z
           y <- ...x...
           q <- x
           z <- y
           r <- x }

Here, the first stmt mention 'y', which is bound in the third.
But that means that the innocent second stmt (p <- z) gets caught
up in the recursion.  And that in turn means that the binding for
'z' has to be included... and so on.

Start at the tail { r <- x }
Now add the next one { z <- y ; r <- x }
Now add one more     { q <- x ; z <- y ; r <- x }
Now one more... but this time we have to group a bunch into rec
     { rec { y <- ...x... ; q <- x ; z <- y } ; r <- x }
Now one more, which we can add on without a rec
     { p <- z ;
       rec { y <- ...x... ; q <- x ; z <- y } ;
       r <- x }
Finally we add the last one; since it mentions y we have to
glom it together with the first two groups
     { rec { x <- ...y...; p <- z ; y <- ...x... ;
             q <- x ; z <- y } ;
       r <- x }



Note [Monad fail : Rebindable syntax, overloaded strings]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/rename/RnExpr.hs#L2119>`__

Given the code
  foo x = do { Just y <- x; return y }

we expect it to desugar as
  foo x = x >>= \r -> case r of
                        Just y  -> return y
                        Nothing -> fail "Pattern match error"

But with RebindableSyntax and OverloadedStrings, we really want
it to desugar thus:
  foo x = x >>= \r -> case r of
                        Just y  -> return y
                        Nothing -> fail (fromString "Patterm match error")

So, in this case, we synthesize the function
  \x -> fail (fromString x)

(rather than plain 'fail') for the 'fail' operation. This is done in
'getMonadFailOp'.

