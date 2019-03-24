`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/deSugar/PmExpr.hs>`_

Note [PmExprOther in PmExpr]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Since there is no plan to extend the (currently pretty naive) term oracle in
the near future, instead of playing with the verbose (HsExpr Id), we lift it to
PmExpr. All expressions the term oracle does not handle are wrapped by the
constructor PmExprOther. Note that we do not perform substitution in
PmExprOther. Because of this, we do not even print PmExprOther, since they may
refer to variables that are otherwise substituted away.
----------------------------------------------------------------------------


Note [Undecidable Equality for Overloaded Literals]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Equality on overloaded literals is undecidable in the general case. Consider
the following example:

.. code-block:: haskell

  instance Num Bool where
    ...
    fromInteger 0 = False -- C-like representation of booleans
    fromInteger _ = True

.. code-block:: haskell

    f :: Bool -> ()
    f 1 = ()        -- Clause A
    f 2 = ()        -- Clause B

Clause B is redundant but to detect this, we should be able to solve the
constraint: False ~ (fromInteger 2 ~ fromInteger 1) which means that we
have to look through function `fromInteger`, whose implementation could
be anything. This poses difficulties for:

1. The expressive power of the check.
   We cannot expect a reasonable implementation of pattern matching to detect
   that fromInteger 2 ~ fromInteger 1 is True, unless we unfold function
   fromInteger. This puts termination at risk and is undecidable in the
   general case.

2. Performance.
   Having an unresolved constraint False ~ (fromInteger 2 ~ fromInteger 1)
   lying around could become expensive really fast. Ticket #11161 illustrates
   how heavy use of overloaded literals can generate plenty of those
   constraints, effectively undermining the term oracle's performance.

3. Error nessages/Warnings.
   What should our message for `f` above be? A reasonable approach would be
   to issue:

.. code-block:: haskell

     Pattern matches are (potentially) redundant:
       f 2 = ...    under the assumption that 1 == 2

.. code-block:: haskell

   but seems to complex and confusing for the user.

We choose to treat overloaded literals that look different as different. The
impact of this is the following:

  * Redundancy checking is rather conservative, since it cannot see that clause
    B above is redundant.

  * We have instant equality check for overloaded literals (we do not rely on
    the term oracle which is rather expensive, both in terms of performance and
    memory). This significantly improves the performance of functions `covered`
    `uncovered` and `divergent` in deSugar/Check.hs and effectively addresses
    #11161.

  * The warnings issued are simpler.

  * We do not play on the safe side, strictly speaking. The assumption that
    1 /= 2 makes the redundancy check more conservative but at the same time
    makes its dual (exhaustiveness check) unsafe. This we can live with, mainly
    for two reasons:
    1. At the moment we do not use the results of the check during compilation
       where this would be a disaster (could result in runtime errors even if
       our function was deemed exhaustive).
    2. Pattern matcing on literals can never be considered exhaustive unless we
       have a catch-all clause. Hence, this assumption affects mainly the
       appearance of the warnings and is, in practice safe.

