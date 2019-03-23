Note [Overall plumbing for rules]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
* After the desugarer:
   - The ModGuts initially contains mg_rules :: [CoreRule] of
     locally-declared rules for imported Ids.
   - Locally-declared rules for locally-declared Ids are attached to
     the IdInfo for that Id.  See Note [Attach rules to local ids] in
     DsBinds

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




Note [Care with roughTopName]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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


Note [Where rules are found]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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




Note [Extra args in rule matching]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
----------------------------------


Note [Unbound RULE binders]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It can be the case that the binder in a rule is not actually
bound on the LHS:

* Type variables.  Type synonyms with phantom args can give rise to
  unbound template type variables.  Consider this (#10689,
  simplCore/should_compile/T10689):

    type Foo a b = b

    f :: Eq a => a -> Bool
    f x = x==x

    {-# RULES "foo" forall (x :: Foo a Char). f x = True #-}
    finkle = f 'c'

  The rule looks like
    forall (a::*) (d::Eq Char) (x :: Foo a Char).
         f (Foo a Char) d x = True

  Matching the rule won't bind 'a', and legitimately so.  We fudge by
  pretending that 'a' is bound to (Any :: *).

* Coercion variables.  On the LHS of a RULE for a local binder
  we might have
    RULE forall (c :: a~b). f (x |> c) = e
  Now, if that binding is inlined, so that a=b=Int, we'd get
    RULE forall (c :: Int~Int). f (x |> c) = e
  and now when we simplify the LHS (Simplify.simplRule) we
  optCoercion will turn that 'c' into Refl:
    RULE forall (c :: Int~Int). f (x |> <Int>) = e
  and then perhaps drop it altogether.  Now 'c' is unbound.

  It's tricky to be sure this never happens, so instead I
  say it's OK to have an unbound coercion binder in a RULE
  provided its type is (c :: t~t).  Then, when the RULE
  fires we can substitute <t> for c.

  This actually happened (in a RULE for a local function)
  in #13410, and also in test T10602.




Note [Cloning the template binders]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider the following match (example 1):
        Template:  forall x.  f x
        Target:               f (x+1)
This should succeed, because the template variable 'x' has nothing to
do with the 'x' in the target.

Likewise this one (example 2):
        Template:  forall x. f (\x.x)
        Target:              f (\y.y)

We achieve this simply by using rnBndrL to clone the template
binders if they are already in scope.

------ Historical note -------
At one point I tried simply adding the template binders to the
in-scope set /without/ cloning them, but that failed in a horribly
obscure way in #14777.  Problem was that during matching we look
up target-term variables in the in-scope set (see Note [Lookup
in-scope]).  If a target-term variable happens to name-clash with a
template variable, that lookup will find the template variable, which
is /utterly/ bogus.  In #14777, this transformed a term variable
into a type variable, and then crashed when we wanted its idInfo.
------ End of historical note -------




Note [Expanding variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~
Here is another Very Important rule: if the term being matched is a
variable, we expand it so long as its unfolding is "expandable". (Its
occurrence information is not necessarily up to date, so we don't use
it.)  By "expandable" we mean a WHNF or a "constructor-like" application.
This is the key reason for "constructor-like" Ids.  If we have
     {-# NOINLINE [1] CONLIKE g #-}
     {-# RULE f (g x) = h x #-}
then in the term
   let v = g 3 in ....(f v)....
we want to make the rule fire, to replace (f v) with (h 3).



Note [Do not expand locally-bound variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Do *not* expand locally-bound variables, else there's a worry that the
unfolding might mention variables that are themselves renamed.
Example
          case x of y { (p,q) -> ...y... }
Don't expand 'y' to (p,q) because p,q might themselves have been
renamed.  Essentially we only expand unfoldings that are "outside"
the entire match.

Hence, (a) the guard (not (isLocallyBoundR v2))
       (b) when we expand we nuke the renaming envt (nukeRnEnvR).



Note [Tick annotations in RULE matching]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We used to unconditionally look through Notes in both template and
expression being matched. This is actually illegal for counting or
cost-centre-scoped ticks, because we have no place to put them without
changing entry counts and/or costs. So now we just fail the match in
these cases.

On the other hand, where we are allowed to insert new cost into the
tick scope, we can float them upwards to the rule application site.

cf Note [Notes in call patterns] in SpecConstr



Note [Matching lets]
~~~~~~~~~~~~~~~~~~~~
Matching a let-expression.  Consider
        RULE forall x.  f (g x) = <rhs>
and target expression
        f (let { w=R } in g E))
Then we'd like the rule to match, to generate
        let { w=R } in (\x. <rhs>) E
In effect, we want to float the let-binding outward, to enable
the match to happen.  This is the WHOLE REASON for accumulating
bindings in the RuleSubst

We can only do this if the free variables of R are not bound by the
part of the target expression outside the let binding; e.g.
        f (\v. let w = v+1 in g E)
Here we obviously cannot float the let-binding for w.  Hence the
use of okToFloat.

There are a couple of tricky points.
  (a) What if floating the binding captures a variable?
        f (let v = x+1 in v) v
      --> NOT!
        let v = x+1 in f (x+1) v

  (b) What if two non-nested let bindings bind the same variable?
        f (let v = e1 in b1) (let v = e2 in b2)
      --> NOT!
        let v = e1 in let v = e2 in (f b2 b2)
      See testsuite test "RuleFloatLet".

Our cunning plan is this:
  * Along with the growing substitution for template variables
    we maintain a growing set of floated let-bindings (rs_binds)
    plus the set of variables thus bound.

  * The RnEnv2 in the MatchEnv binds only the local binders
    in the term (lambdas, case)

  * When we encounter a let in the term to be matched, we
    check that does not mention any locally bound (lambda, case)
    variables.  If so we fail

  * We use CoreSubst.substBind to freshen the binding, using an
    in-scope set that is the original in-scope variables plus the
    rs_bndrs (currently floated let-bindings).  So in (a) above
    we'll freshen the 'v' binding; in (b) above we'll freshen
    the *second* 'v' binding.

  * We apply that freshening substitution, in a lexically-scoped
    way to the term, although lazily; this is the rv_fltR field.




Note [Matching cases]
~~~~~~~~~~~~~~~~~~~~~
{- NOTE: This idea is currently disabled.  It really only works if
         the primops involved are OkForSpeculation, and, since
         they have side effects readIntOfAddr and touch are not.
         Maybe we'll get back to this later .  -}

Consider
   f (case readIntOffAddr# p# i# realWorld# of { (# s#, n# #) ->
      case touch# fp s# of { _ ->
      I# n# } } )
This happened in a tight loop generated by stream fusion that
Roman encountered.  We'd like to treat this just like the let
case, because the primops concerned are ok-for-speculation.
That is, we'd like to behave as if it had been
   case readIntOffAddr# p# i# realWorld# of { (# s#, n# #) ->
   case touch# fp s# of { _ ->
   f (I# n# } } )



Note [Lookup in-scope]
~~~~~~~~~~~~~~~~~~~~~~
Consider this example
        foo :: Int -> Maybe Int -> Int
        foo 0 (Just n) = n
        foo m (Just n) = foo (m-n) (Just n)

SpecConstr sees this fragment:

        case w_smT of wild_Xf [Just A] {
          Data.Maybe.Nothing -> lvl_smf;
          Data.Maybe.Just n_acT [Just S(L)] ->
            case n_acT of wild1_ams [Just A] { GHC.Base.I# y_amr [Just L] ->
              $wfoo_smW (GHC.Prim.-# ds_Xmb y_amr) wild_Xf
            }};

and correctly generates the rule

        RULES: "SC:$wfoo1" [0] __forall {y_amr [Just L] :: GHC.Prim.Int#
                                          sc_snn :: GHC.Prim.Int#}
          $wfoo_smW sc_snn (Data.Maybe.Just @ GHC.Base.Int (GHC.Base.I# y_amr))
          = $s$wfoo_sno y_amr sc_snn ;]

BUT we must ensure that this rule matches in the original function!
Note that the call to $wfoo is
            $wfoo_smW (GHC.Prim.-# ds_Xmb y_amr) wild_Xf

During matching we expand wild_Xf to (Just n_acT).  But then we must also
expand n_acT to (I# y_amr).  And we can only do that if we look up n_acT
in the in-scope set, because in wild_Xf's unfolding it won't have an unfolding
at all.

That is why the 'lookupRnInScope' call in the (Var v2) case of 'match'
is so important.


