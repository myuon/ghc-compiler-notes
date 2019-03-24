`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/deSugar/Desugar.hs>`_

====================
compiler/deSugar/Desugar.hs.rst
====================

Note [Top-level evidence]
~~~~~~~~~~~~~~~~~~~~~~~~~
Top-level evidence bindings may be mutually recursive with the top-level value
bindings, so we must put those in a Rec.  But we can't put them *all* in a Rec
because the occurrence analyser doesn't take account of type/coercion variables
when computing dependencies.

So we pull out the type/coercion variables (which are in dependency order),
and Rec the rest.


Note [Adding export flags]
~~~~~~~~~~~~~~~~~~~~~~~~~~
Set the no-discard flag if either
        a) the Id is exported
        b) it's mentioned in the RHS of an orphan rule
        c) it's in the keep-alive set

It means that the binding won't be discarded EVEN if the binding
ends up being trivial (v = w) -- the simplifier would usually just
substitute w for v throughout, but we don't apply the substitution to
the rules (maybe we should?), so this substitution would make the rule
bogus.

You might wonder why exported Ids aren't already marked as such;
it's just because the type checker is rather busy already and
I didn't want to pass in yet another mapping.



Note [Attach rules to local ids]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Find the rules for locally-defined Ids; then we can attach them
to the binders in the top-level bindings

Reason
  - It makes the rules easier to look up
  - It means that transformation rules and specialisations for
    locally defined Ids are handled uniformly
  - It keeps alive things that are referred to only from a rule
    (the occurrence analyser knows about rules attached to Ids)
  - It makes sure that, when we apply a rule, the free vars
    of the RHS are more likely to be in scope
  - The imported rules are carried in the in-scope set
    which is extended on each iteration by the new wave of
    local binders; any rules which aren't on the binding will
    thereby get dropped




Note [Desugaring RULE left hand sides]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For the LHS of a RULE we do *not* want to desugar
    [x]   to    build (\cn. x `c` n)
We want to leave explicit lists simply as chains
of cons's. We can achieve that slightly indirectly by
switching off EnableRewriteRules.  See DsExpr.dsExplicitList.

That keeps the desugaring of list comprehensions simple too.

Nor do we want to warn of conversion identities on the LHS;
the rule is precisely to optimise them:
  {-# RULES "fromRational/id" fromRational = id :: Rational -> Rational #-}



Note [Desugaring coerce as cast]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We want the user to express a rule saying roughly “mapping a coercion over a
list can be replaced by a coercion”. But the cast operator of Core (▷) cannot
be written in Haskell. So we use `coerce` for that (#2110). The user writes
    map coerce = coerce
as a RULE, and this optimizes any kind of mapped' casts away, including `map
MkNewtype`.

For that we replace any forall'ed `c :: Coercible a b` value in a RULE by
corresponding `co :: a ~#R b` and wrap the LHS and the RHS in
`let c = MkCoercible co in ...`. This is later simplified to the desired form
by simpleOptExpr (for the LHS) resp. the simplifiers (for the RHS).
See also Note [Getting the map/coerce RULE to work] in CoreSubst.



Note [Rules and inlining/other rules]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If you have
  f x = ...
  g x = ...
  {-# RULES "rule-for-f" forall x. f (g x) = ... #-}
then there's a good chance that in a potential rule redex
    ...f (g e)...
then 'f' or 'g' will inline befor the rule can fire.  Solution: add an
INLINE [n] or NOINLINE [n] pragma to 'f' and 'g'.

Note that this applies to all the free variables on the LHS, both the
main function and things in its arguments.

We also check if there are Ids on the LHS that have competing RULES.
In the above example, suppose we had
  {-# RULES "rule-for-g" forally. g [y] = ... #-}
Then "rule-for-f" and "rule-for-g" would compete.  Better to add phase
control, so "rule-for-f" has a chance to fire before "rule-for-g" becomes
active; or perhpas after "rule-for-g" has become inactive. This is checked
by 'competesWith'

Class methods have a built-in RULE to select the method from the dictionary,
so you can't change the phase on this.  That makes id very dubious to
match on class methods in RULE lhs's.   See #10595.   I'm not happy
about this. For example in Control.Arrow we have

{-# RULES "compose/arr"   forall f g .
                          (arr f) . (arr g) = arr (f . g) #-}

and similar, which will elicit exactly these warnings, and risk never
firing.  But it's not clear what to do instead.  We could make the
class method rules inactive in phase 2, but that would delay when
subsequent transformations could fire.

