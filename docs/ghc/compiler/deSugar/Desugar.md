[[src]](https://github.com/ghc/ghc/tree/master/compiler/deSugar/Desugar.hs)

(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998


The Desugarer: turning HsSyn into Core.


# The main function: deSugar


### Note: Top-level evidence

Top-level evidence bindings may be mutually recursive with the top-level value
bindings, so we must put those in a Rec.  But we can't put them *all* in a Rec
because the occurrence analyser doesn't teke account of type/coercion variables
when computing dependencies.

So we pull out the type/coercion variables (which are in dependency order),
and Rec the rest.


# Add rules and export flags to binders


### Note: Adding export flags

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

### Note: Attach rules to local ids

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

# Desugaring transformation rules


### Note: Desugaring RULE left hand sides

For the LHS of a RULE we do *not* want to desugar
    [x]   to    build (\cn. x `c` n)
We want to leave explicit lists simply as chains
of cons's. We can achieve that slightly indirectly by
switching off EnableRewriteRules.  See DsExpr.dsExplicitList.

That keeps the desugaring of list comprehensions simple too.

# RULES "compose/arr"   forall f g .
                          (arr f) . (arr g) = arr (f . g) #