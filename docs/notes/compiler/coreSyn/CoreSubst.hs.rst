`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/coreSyn/CoreSubst.hs>`_

====================
compiler/coreSyn/CoreSubst.hs.rst
====================

Note [Extending the Subst]
~~~~~~~~~~~~~~~~~~~~~~~~~~
For a core Subst, which binds Ids as well, we make a different choice for Ids
than we do for TyVars.

For TyVars, see Note [Extending the TCvSubst] with Type.TvSubstEnv

For Ids, we have a different invariant
        The IdSubstEnv is extended *only* when the Unique on an Id changes
        Otherwise, we just extend the InScopeSet

In consequence:

* If all subst envs are empty, substExpr would be a
  no-op, so substExprSC ("short cut") does nothing.

.. code-block:: haskell

  However, substExpr still goes ahead and substitutes.  Reason: we may
  want to replace existing Ids with new ones from the in-scope set, to
  avoid space leaks.

* In substIdBndr, we extend the IdSubstEnv only when the unique changes

* If the CvSubstEnv, TvSubstEnv and IdSubstEnv are all empty,
  substExpr does nothing (Note that the above rule for substIdBndr
  maintains this property.  If the incoming envts are both empty, then
  substituting the type and IdInfo can't change anything.)

* In lookupIdSubst, we *must* look up the Id in the in-scope set, because
  it may contain non-trivial changes.  Example:
        (/\a. \x:a. ...x...) Int
  We extend the TvSubstEnv with [a |-> Int]; but x's unique does not change
  so we only extend the in-scope set.  Then we must look up in the in-scope
  set when we find the occurrence of x.

* The requirement to look up the Id in the in-scope set means that we
  must NOT take no-op short cut when the IdSubst is empty.
  We must still look up every Id in the in-scope set.

* (However, we don't need to do so for expressions found in the IdSubst
  itself, whose range is assumed to be correct wrt the in-scope set.)

Why do we make a different choice for the IdSubstEnv than the
TvSubstEnv and CvSubstEnv?

* For Ids, we change the IdInfo all the time (e.g. deleting the
  unfolding), and adding it back later, so using the TyVar convention
  would entail extending the substitution almost all the time

* The simplifier wants to look up in the in-scope set anyway, in case it
  can see a better unfolding from an enclosing case expression

* For TyVars, only coercion variables can possibly change, and they are
  easy to spot


Note [Substitute lazily]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
The functions that substitute over IdInfo must be pretty lazy, because
they are knot-tied by substRecBndrs.

One case in point was #10627 in which a rule for a function 'f'
referred to 'f' (at a different type) on the RHS.  But instead of just
substituting in the rhs of the rule, we were calling simpleOptExpr, which
looked at the idInfo for 'f'; result <<loop>>.

In any case we don't need to optimise the RHS of rules, or unfoldings,
because the simplifier will do that.




Note [substTickish]
~~~~~~~~~~~~~~~~~~~~~~
A Breakpoint contains a list of Ids.  What happens if we ever want to
substitute an expression for one of these Ids?

First, we ensure that we only ever substitute trivial expressions for
these Ids, by marking them as NoOccInfo in the occurrence analyser.
Then, when substituting for the Id, we unwrap any type applications
and abstractions to get back to an Id, with getIdFromTrivialExpr.

Second, we have to ensure that we never try to substitute a literal
for an Id in a breakpoint.  We ensure this by never storing an Id with
an unlifted type in a Breakpoint - see Coverage.mkTickish.
Breakpoints can't handle free variables with unlifted types anyway.


Note [Worker inlining]
~~~~~~~~~~~~~~~~~~~~~~
A worker can get sustituted away entirely.
        - it might be trivial
        - it might simply be very small
We do not treat an InlWrapper as an 'occurrence' in the occurrence
analyser, so it's possible that the worker is not even in scope any more.

In all all these cases we simply drop the special case, returning to
InlVanilla.  The WARN is just so I can see if it happens a lot.

