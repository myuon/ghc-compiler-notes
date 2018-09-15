[[src]](https://github.com/ghc/ghc/tree/master/compiler/specialise/Rules.hs)

(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

# Transformation rules

### Note: Overall plumbing for rules

### Note: Attach rules to local ids

* TidyPgm strips off all the rules from local Ids and adds them to
  mg_rules, so that the ModGuts has *all* the locally-declared rules.

* The HomePackageTable contains a ModDetails for each home package
  module.  Each contains md_rules :: [CoreRule] of rules declared in
  that module.  The HomePackageTable grows as ghc --make does its
  up-sweep.  In batch mode (ghc -c), the HPT is empty; all imported modules
  are treated by the "external" route, discussed next, regardless of
  which package they come from.

* The ExternalPackageState has a single eps_rule_base :: RuleBase for
  Ids in other packages.  This RuleBase simply grow monotonically, as
  ghc --make compiles one module after another.

  During simplification, interface files may get demand-loaded,
  as the simplifier explores the unfoldings for Ids it has in
  its hand.  (Via an unsafePerformIO; the EPS is really a cache.)
  That in turn may make the EPS rule-base grow.  In contrast, the
  HPT never grows in this way.

* The result of all this is that during Core-to-Core optimisation
  there are four sources of rules:

    (a) Rules in the IdInfo of the Id they are a rule for.  These are
        easy: fast to look up, and if you apply a substitution then
        it'll be applied to the IdInfo as a matter of course.

    (b) Rules declared in this module for imported Ids, kept in the
        ModGuts. If you do a substitution, you'd better apply the
        substitution to these.  There are seldom many of these.

    (c) Rules declared in the HomePackageTable.  These never change.

    (d) Rules in the ExternalPackageTable. These can grow in response
        to lazy demand-loading of interfaces.

* At the moment (c) is carried in a reader-monad way by the CoreMonad.
  The HomePackageTable doesn't have a single RuleBase because technically
  we should only be able to "see" rules "below" this module; so we
  generate a RuleBase for (c) by combing rules from all the modules
  "below" us.  That's why we can't just select the home-package RuleBase
  from HscEnv.

  [NB: we are inconsistent here.  We should do the same for external
  packages, but we don't.  Same for type-class instances.]

* So in the outer simplifier loop, we combine (b-d) into a single
  RuleBase, reading
     (b) from the ModGuts,
     (c) from the CoreMonad, and
     (d) from its mutable variable
  [Of coures this means that we won't see new EPS rules that come in
  during a single simplifier iteration, but that probably does not
  matter.]

# \subsection[specialisation-IdInfo]{Specialisation info about an @Id@}


A @CoreRule@ holds details of one rule for an @Id@, which
includes its specialisations.

For example, if a rule for @f@ contains the mapping:
\begin{verbatim}
        forall a b d. [Type (List a), Type b, Var d]  ===>  f' a b
\end{verbatim}
then when we find an application of f to matching types, we simply replace
it by the matching RHS:
\begin{verbatim}
        f (List Int) Bool dict ===>  f' Int Bool
\end{verbatim}
All the stuff about how many dictionaries to discard, and what types
to apply the specialised function to, are handled by the fact that the
Rule contains a template for the result of the specialisation.

There is one more exciting case, which is dealt with in exactly the same
way.  If the specialised value is unboxed then it is lifted at its
definition site and unlifted at its uses.  For example:

        pi :: forall a. Num a => a

might have a specialisation

        [Int#] ===>  (case pi' of Lift pi# -> pi#)

where pi' :: Lift Int# is the specialised version of pi.


### Note: Care with roughTopName

Consider this
    module M where { x = a:b }
    module N where { ...f x...
                     RULE f (p:q) = ... }
You'd expect the rule to match, because the matcher can
look through the unfolding of 'x'.  So we must avoid roughTopName
returning 'M.x' for the call (f x), or else it'll say "can't match"
and we won't even try!!

However, suppose we have
         RULE g (M.h x) = ...
         foo = ...(g (M.k v))....
where k is a *function* exported by M.  We never really match
functions (lambdas) except by name, so in this case it seems like
a good idea to treat 'M.k' as a roughTopName of the call.


# RuleInfo: the rules in an IdInfo


### Note: Where rules are found

The rules for an Id come from two places:
  (a) the ones it is born with, stored inside the Id iself (idCoreRules fn),
  (b) rules added in other modules, stored in the global RuleBase (imp_rules)

It's tempting to think that
     - LocalIds have only (a)
     - non-LocalIds have only (b)

but that isn't quite right:

     - PrimOps and ClassOps are born with a bunch of rules inside the Id,
       even when they are imported

     - The rules in PrelRules.builtinRules should be active even
       in the module defining the Id (when it's a LocalId), but
       the rules are kept in the global RuleBase

# RuleBase


# Matching


### Note: Extra args in rule matching

If we find a matching rule, we return (Just (rule, rhs)),
but the rule firing has only consumed as many of the input args
as the ruleArity says.  It's up to the caller to keep track
of any left-over args.  E.g. if you call
        lookupRule ... f [e1, e2, e3]
and it returns Just (r, rhs), where r has ruleArity 2
then the real rewrite is
        f e1 e2 e3 ==> rhs e3

You might think it'd be cleaner for lookupRule to deal with the
leftover arguments, by applying 'rhs' to them, but the main call
in the Simplifier works better as it is.  Reason: the 'args' passed
to lookupRule are the result of a lazy substitution


### Note: Unbound RULE binders

It can be the case that the binder in a rule is not actually
bound on the LHS:

* Type variables.  Type synonyms with phantom args can give rise to
  unbound template type variables.  Consider this (Trac #10689,
  simplCore/should_compile/T10689):

    type Foo a b = b

    f :: Eq a => a -> Bool
    f x = x==x

### Note: Matching cases

### Note: Expanding variables

 NOTE: This idea is currently disabled.  It really only works if
         the primops involved are OkForSpeculation, and, since
         they have side effects readIntOfAddr and touch are not.
         Maybe we'll get back to this later .  