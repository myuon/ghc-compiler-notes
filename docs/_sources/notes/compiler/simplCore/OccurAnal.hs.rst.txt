Note [Plugin rules]
~~~~~~~~~~~~~~~~~~~~~~
Conal Elliott (#11651) built a GHC plugin that added some
BuiltinRules (for imported Ids) to the mg_rules field of ModGuts, to
do some domain-specific transformations that could not be expressed
with an ordinary pattern-matching CoreRule.  But then we can't extract
the dependencies (in imp_rule_edges) from ru_rhs etc, because a
BuiltinRule doesn't have any of that stuff.

So we simply assume that BuiltinRules have no dependencies, and filter
them out from the imp_rule_edges comprehension.


Note [Recursive bindings: the grand plan]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we come across a binding group
  Rec { x1 = r1; ...; xn = rn }
we treat it like this (occAnalRecBind):

1. Occurrence-analyse each right hand side, and build a
   "Details" for each binding to capture the results.

   Wrap the details in a Node (details, node-id, dep-node-ids),
   where node-id is just the unique of the binder, and
   dep-node-ids lists all binders on which this binding depends.
   We'll call these the "scope edges".
   See Note [Forming the Rec groups].

   All this is done by makeNode.

2. Do SCC-analysis on these Nodes.  Each SCC will become a new Rec or
   NonRec.  The key property is that every free variable of a binding
   is accounted for by the scope edges, so that when we are done
   everything is still in scope.

3. For each Cyclic SCC of the scope-edge SCC-analysis in (2), we
   identify suitable loop-breakers to ensure that inlining terminates.
   This is done by occAnalRec.

4. To do so we form a new set of Nodes, with the same details, but
   different edges, the "loop-breaker nodes". The loop-breaker nodes
   have both more and fewer dependencies than the scope edges
   (see Note [Choosing loop breakers])

   More edges: if f calls g, and g has an active rule that mentions h
               then we add an edge from f -> h

   Fewer edges: we only include dependencies on active rules, on rule
                RHSs (not LHSs) and if there is an INLINE pragma only
                on the stable unfolding (and vice versa).  The scope
                edges must be much more inclusive.

5.  The "weak fvs" of a node are, by definition:
       the scope fvs - the loop-breaker fvs
    See Note [Weak loop breakers], and the nd_weak field of Details

6.  Having formed the loop-breaker nodes



Note [Dead code]
~~~~~~~~~~~~~~~~
Dropping dead code for a cyclic Strongly Connected Component is done
in a very simple way:

        the entire SCC is dropped if none of its binders are mentioned
        in the body; otherwise the whole thing is kept.

The key observation is that dead code elimination happens after
dependency analysis: so 'occAnalBind' processes SCCs instead of the
original term's binding groups.

Thus 'occAnalBind' does indeed drop 'f' in an example like

        letrec f = ...g...
               g = ...(...g...)...
        in
           ...g...

when 'g' no longer uses 'f' at all (eg 'f' does not occur in a RULE in
'g'). 'occAnalBind' first consumes 'CyclicSCC g' and then it consumes
'AcyclicSCC f', where 'body_usage' won't contain 'f'.

------------------------------------------------------------


Note [Forming Rec groups]
~~~~~~~~~~~~~~~~~~~~~~~~~
We put bindings {f = ef; g = eg } in a Rec group if "f uses g"
and "g uses f", no matter how indirectly.  We do a SCC analysis
with an edge f -> g if "f uses g".

More precisely, "f uses g" iff g should be in scope wherever f is.
That is, g is free in:
  a) the rhs 'ef'
  b) or the RHS of a rule for f (Note [Rules are extra RHSs])
  c) or the LHS or a rule for f (Note [Rule dependency info])

These conditions apply regardless of the activation of the RULE (eg it might be
inactive in this phase but become active later).  Once a Rec is broken up
it can never be put back together, so we must be conservative.

The principle is that, regardless of rule firings, every variable is
always in scope.

  * Note [Rules are extra RHSs]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~
    A RULE for 'f' is like an extra RHS for 'f'. That way the "parent"
    keeps the specialised "children" alive.  If the parent dies
    (because it isn't referenced any more), then the children will die
    too (unless they are already referenced directly).

    To that end, we build a Rec group for each cyclic strongly
    connected component,
        *treating f's rules as extra RHSs for 'f'*.
    More concretely, the SCC analysis runs on a graph with an edge
    from f -> g iff g is mentioned in
        (a) f's rhs
        (b) f's RULES
    These are rec_edges.

    Under (b) we include variables free in *either* LHS *or* RHS of
    the rule.  The former might seems silly, but see Note [Rule
    dependency info].  So in Example [eftInt], eftInt and eftIntFB
    will be put in the same Rec, even though their 'main' RHSs are
    both non-recursive.

  * Note [Rule dependency info]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~
    The VarSet in a RuleInfo is used for dependency analysis in the
    occurrence analyser.  We must track free vars in *both* lhs and rhs.
    Hence use of idRuleVars, rather than idRuleRhsVars in occAnalBind.
    Why both? Consider
        x = y
        RULE f x = v+4
    Then if we substitute y for x, we'd better do so in the
    rule's LHS too, so we'd better ensure the RULE appears to mention 'x'
    as well as 'v'

  * Note [Rules are visible in their own rec group]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    We want the rules for 'f' to be visible in f's right-hand side.
    And we'd like them to be visible in other functions in f's Rec
    group.  E.g. in Note [Specialisation rules] we want f' rule
    to be visible in both f's RHS, and fs's RHS.

    This means that we must simplify the RULEs first, before looking
    at any of the definitions.  This is done by Simplify.simplRecBind,
    when it calls addLetIdInfo.

------------------------------------------------------------


Note [Choosing loop breakers]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Loop breaking is surprisingly subtle.  First read the section 4 of
"Secrets of the GHC inliner".  This describes our basic plan.
We avoid infinite inlinings by choosing loop breakers, and
ensuring that a loop breaker cuts each loop.

See also Note [Inlining and hs-boot files] in ToIface, which deals
with a closely related source of infinite loops.

Fundamentally, we do SCC analysis on a graph.  For each recursive
group we choose a loop breaker, delete all edges to that node,
re-analyse the SCC, and iterate.

But what is the graph?  NOT the same graph as was used for Note
[Forming Rec groups]!  In particular, a RULE is like an equation for
'f' that is *always* inlined if it is applicable.  We do *not* disable
rules for loop-breakers.  It's up to whoever makes the rules to make
sure that the rules themselves always terminate.  See Note [Rules for
recursive functions] in Simplify.hs

Hence, if
    f's RHS (or its INLINE template if it has one) mentions g, and
    g has a RULE that mentions h, and
    h has a RULE that mentions f

then we *must* choose f to be a loop breaker.  Example: see Note
[Specialisation rules].

In general, take the free variables of f's RHS, and augment it with
all the variables reachable by RULES from those starting points.  That
is the whole reason for computing rule_fv_env in occAnalBind.  (Of
course we only consider free vars that are also binders in this Rec
group.)  See also Note [Finding rule RHS free vars]

Note that when we compute this rule_fv_env, we only consider variables
free in the *RHS* of the rule, in contrast to the way we build the
Rec group in the first place (Note [Rule dependency info])

Note that if 'g' has RHS that mentions 'w', we should add w to
g's loop-breaker edges.  More concretely there is an edge from f -> g
iff
        (a) g is mentioned in f's RHS `xor` f's INLINE rhs
            (see Note [Inline rules])
        (b) or h is mentioned in f's RHS, and
            g appears in the RHS of an active RULE of h
            or a transitive sequence of active rules starting with h

Why "active rules"?  See Note [Finding rule RHS free vars]

Note that in Example [eftInt], *neither* eftInt *nor* eftIntFB is
chosen as a loop breaker, because their RHSs don't mention each other.
And indeed both can be inlined safely.

Note again that the edges of the graph we use for computing loop breakers
are not the same as the edges we use for computing the Rec blocks.
That's why we compute

- rec_edges          for the Rec block analysis
- loop_breaker_nodes for the loop breaker analysis

  * Note [Finding rule RHS free vars]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    Consider this real example from Data Parallel Haskell
         tagZero :: Array Int -> Array Tag
         {-# INLINE [1] tagZeroes #-}
         tagZero xs = pmap (\x -> fromBool (x==0)) xs

         {-# RULES "tagZero" [~1] forall xs n.
             pmap fromBool <blah blah> = tagZero xs #-}
    So tagZero's RHS mentions pmap, and pmap's RULE mentions tagZero.
    However, tagZero can only be inlined in phase 1 and later, while
    the RULE is only active *before* phase 1.  So there's no problem.

    To make this work, we look for the RHS free vars only for
    *active* rules. That's the reason for the occ_rule_act field
    of the OccEnv.

  * Note [Weak loop breakers]
    ~~~~~~~~~~~~~~~~~~~~~~~~~
    There is a last nasty wrinkle.  Suppose we have

        Rec { f = f_rhs
              RULE f [] = g

              h = h_rhs
              g = h
              ...more...
        }

    Remember that we simplify the RULES before any RHS (see Note
    [Rules are visible in their own rec group] above).

    So we must *not* postInlineUnconditionally 'g', even though
    its RHS turns out to be trivial.  (I'm assuming that 'g' is
    not choosen as a loop breaker.)  Why not?  Because then we
    drop the binding for 'g', which leaves it out of scope in the
    RULE!

    Here's a somewhat different example of the same thing
        Rec { g = h
            ; h = ...f...
            ; f = f_rhs
              RULE f [] = g }
    Here the RULE is "below" g, but we *still* can't postInlineUnconditionally
    g, because the RULE for f is active throughout.  So the RHS of h
    might rewrite to     h = ...g...
    So g must remain in scope in the output program!

    We "solve" this by:

        Make g a "weak" loop breaker (OccInfo = IAmLoopBreaker True)
        iff g is a "missing free variable" of the Rec group

    A "missing free variable" x is one that is mentioned in an RHS or
    INLINE or RULE of a binding in the Rec group, but where the
    dependency on x may not show up in the loop_breaker_nodes (see
    note [Choosing loop breakers} above).

    A normal "strong" loop breaker has IAmLoopBreaker False.  So

                                    Inline  postInlineUnconditionally
   strong   IAmLoopBreaker False    no      no
   weak     IAmLoopBreaker True     yes     no
            other                   yes     yes

    The **sole** reason for this kind of loop breaker is so that
    postInlineUnconditionally does not fire.  Ugh.  (Typically it'll
    inline via the usual callSiteInline stuff, so it'll be dead in the
    next pass, so the main Ugh is the tiresome complication.)



Note [Rules for imported functions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this
   f = /\a. B.g a
   RULE B.g Int = 1 + f Int
Note that
  * The RULE is for an imported function.
  * f is non-recursive
Now we
can get
   f Int --> B.g Int      Inlining f
         --> 1 + f Int    Firing RULE
and so the simplifier goes into an infinite loop. This
would not happen if the RULE was for a local function,
because we keep track of dependencies through rules.  But
that is pretty much impossible to do for imported Ids.  Suppose
f's definition had been
   f = /\a. C.h a
where (by some long and devious process), C.h eventually inlines to
B.g.  We could only spot such loops by exhaustively following
unfoldings of C.h etc, in case we reach B.g, and hence (via the RULE)
f.

Note that RULES for imported functions are important in practice; they
occur a lot in the libraries.

We regard this potential infinite loop as a *programmer* error.
It's up the programmer not to write silly rules like
     RULE f x = f x
and the example above is just a more complicated version.



Note [Preventing loops due to imported functions rules]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider:
  import GHC.Base (foldr)

  {-# RULES "filterList" forall p. foldr (filterFB (:) p) [] = filter p #-}
  filter p xs = build (\c n -> foldr (filterFB c p) n xs)
  filterFB c p = ...

  f = filter p xs

Note that filter is not a loop-breaker, so what happens is:
  f =          filter p xs
    = {inline} build (\c n -> foldr (filterFB c p) n xs)
    = {inline} foldr (filterFB (:) p) [] xs
    = {RULE}   filter p xs

We are in an infinite loop.

A more elaborate example (that I actually saw in practice when I went to
mark GHC.List.filter as INLINABLE) is as follows. Say I have this module:
  {-# LANGUAGE RankNTypes #-}
  module GHCList where

  import Prelude hiding (filter)
  import GHC.Base (build)

  {-# INLINABLE filter #-}
  filter :: (a -> Bool) -> [a] -> [a]
  filter p [] = []
  filter p (x:xs) = if p x then x : filter p xs else filter p xs

  {-# NOINLINE [0] filterFB #-}
  filterFB :: (a -> b -> b) -> (a -> Bool) -> a -> b -> b
  filterFB c p x r | p x       = x `c` r
                   | otherwise = r

  {-# RULES
  "filter"     [~1] forall p xs.  filter p xs = build (\c n -> foldr
  (filterFB c p) n xs)
  "filterList" [1]  forall p.     foldr (filterFB (:) p) [] = filter p
   #-}

Then (because RULES are applied inside INLINABLE unfoldings, but inlinings
are not), the unfolding given to "filter" in the interface file will be:
  filter p []     = []
  filter p (x:xs) = if p x then x : build (\c n -> foldr (filterFB c p) n xs)
                           else     build (\c n -> foldr (filterFB c p) n xs

Note that because this unfolding does not mention "filter", filter is not
marked as a strong loop breaker. Therefore at a use site in another module:
  filter p xs
    = {inline}
      case xs of []     -> []
                 (x:xs) -> if p x then x : build (\c n -> foldr (filterFB c p) n xs)
                                  else     build (\c n -> foldr (filterFB c p) n xs)

  build (\c n -> foldr (filterFB c p) n xs)
    = {inline} foldr (filterFB (:) p) [] xs
    = {RULE}   filter p xs

And we are in an infinite loop again, except that this time the loop is producing an
infinitely large *term* (an unrolling of filter) and so the simplifier finally
dies with "ticks exhausted"

Because of this problem, we make a small change in the occurrence analyser
designed to mark functions like "filter" as strong loop breakers on the basis that:
  1. The RHS of filter mentions the local function "filterFB"
  2. We have a rule which mentions "filterFB" on the LHS and "filter" on the RHS

So for each RULE for an *imported* function we are going to add
dependency edges between the *local* FVS of the rule LHS and the
*local* FVS of the rule RHS. We don't do anything special for RULES on
local functions because the standard occurrence analysis stuff is
pretty good at getting loop-breakerness correct there.

It is important to note that even with this extra hack we aren't always going to get
things right. For example, it might be that the rule LHS mentions an imported Id,
and another module has a RULE that can rewrite that imported Id to one of our local
Ids.

Note [Specialising imported functions] (referred to from Specialise)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
BUT for *automatically-generated* rules, the programmer can't be
responsible for the "programmer error" in Note [Rules for imported
functions].  In paricular, consider specialising a recursive function
defined in another module.  If we specialise a recursive function B.g,
we get
         g_spec = .....(B.g Int).....
         RULE B.g Int = g_spec
Here, g_spec doesn't look recursive, but when the rule fires, it
becomes so.  And if B.g was mutually recursive, the loop might
not be as obvious as it is here.

To avoid this,
 * When specialising a function that is a loop breaker,
   give a NOINLINE pragma to the specialised function



Note [Glomming]
~~~~~~~~~~~~~~~
RULES for imported Ids can make something at the top refer to something at the bottom:
        f = \x -> B.g (q x)
        h = \y -> 3

        RULE:  B.g (q x) = h x

Applying this rule makes f refer to h, although f doesn't appear to
depend on h.  (And, as in Note [Rules for imported functions], the
dependency might be more indirect. For example, f might mention C.t
rather than B.g, where C.t eventually inlines to B.g.)

NOTICE that this cannot happen for rules whose head is a
locally-defined function, because we accurately track dependencies
through RULES.  It only happens for rules whose head is an imported
function (B.g in the example above).

Solution:
  - When simplifying, bring all top level identifiers into
    scope at the start, ignoring the Rec/NonRec structure, so
    that when 'h' pops up in f's rhs, we find it in the in-scope set
    (as the simplifier generally expects). This happens in simplTopBinds.

  - In the occurrence analyser, if there are any out-of-scope
    occurrences that pop out of the top, which will happen after
    firing the rule:      f = \x -> h x
                          h = \y -> 3
    then just glom all the bindings into a single Rec, so that
    the *next* iteration of the occurrence analyser will sort
    them all out.   This part happens in occurAnalysePgm.

------------------------------------------------------------


Note [Inline rules]
~~~~~~~~~~~~~~~~~~~
None of the above stuff about RULES applies to Inline Rules,
stored in a CoreUnfolding.  The unfolding, if any, is simplified
at the same time as the regular RHS of the function (ie *not* like
Note [Rules are visible in their own rec group]), so it should be
treated *exactly* like an extra RHS.

Or, rather, when computing loop-breaker edges,
  * If f has an INLINE pragma, and it is active, we treat the
    INLINE rhs as f's rhs
  * If it's inactive, we treat f as having no rhs
  * If it has no INLINE pragma, we look at f's actual rhs


There is a danger that we'll be sub-optimal if we see this
     f = ...f...
     [INLINE f = ..no f...]
where f is recursive, but the INLINE is not. This can just about
happen with a sufficiently odd set of rules; eg

        foo :: Int -> Int
        {-# INLINE [1] foo #-}
        foo x = x+1

        bar :: Int -> Int
        {-# INLINE [1] bar #-}
        bar x = foo x + 1

        {-# RULES "foo" [~1] forall x. foo x = bar x #-}

Here the RULE makes bar recursive; but it's INLINE pragma remains
non-recursive. It's tempting to then say that 'bar' should not be
a loop breaker, but an attempt to do so goes wrong in two ways:
   a) We may get
         $df = ...$cfoo...
         $cfoo = ...$df....
         [INLINE $cfoo = ...no-$df...]
      But we want $cfoo to depend on $df explicitly so that we
      put the bindings in the right order to inline $df in $cfoo
      and perhaps break the loop altogether.  (Maybe this
   b)


Example [eftInt]
~~~~~~~~~~~~~~~
Example (from GHC.Enum):

  eftInt :: Int# -> Int# -> [Int]
  eftInt x y = ...(non-recursive)...

  {-# INLINE [0] eftIntFB #-}
  eftIntFB :: (Int -> r -> r) -> r -> Int# -> Int# -> r
  eftIntFB c n x y = ...(non-recursive)...

  {-# RULES
  "eftInt"  [~1] forall x y. eftInt x y = build (\ c n -> eftIntFB c n x y)
  "eftIntList"  [1] eftIntFB  (:) [] = eftInt
   #-}



Note [Specialisation rules]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this group, which is typical of what SpecConstr builds:

   fs a = ....f (C a)....
   f  x = ....f (C a)....
   {-# RULE f (C a) = fs a #-}

So 'f' and 'fs' are in the same Rec group (since f refers to fs via its RULE).

But watch out!  If 'fs' is not chosen as a loop breaker, we may get an infinite loop:
  - the RULE is applied in f's RHS (see Note [Self-recursive rules] in Simplify
  - fs is inlined (say it's small)
  - now there's another opportunity to apply the RULE

This showed up when compiling Control.Concurrent.Chan.getChanContents.

------------------------------------------------------------


Note [Finding join points]
~~~~~~~~~~~~~~~~~~~~~~~~~~
It's the occurrence analyser's job to find bindings that we can turn into join
points, but it doesn't perform that transformation right away. Rather, it marks
the eligible bindings as part of their occurrence data, leaving it to the
simplifier (or to simpleOptPgm) to actually change the binder's 'IdDetails'.
The simplifier then eta-expands the RHS if needed and then updates the
occurrence sites. Dividing the work this way means that the occurrence analyser
still only takes one pass, yet one can always tell the difference between a
function call and a jump by looking at the occurrence (because the same pass
changes the 'IdDetails' and propagates the binders to their occurrence sites).

To track potential join points, we use the 'occ_tail' field of OccInfo. A value
of `AlwaysTailCalled n` indicates that every occurrence of the variable is a
tail call with `n` arguments (counting both value and type arguments). Otherwise
'occ_tail' will be 'NoTailCallInfo'. The tail call info flows bottom-up with the
rest of 'OccInfo' until it goes on the binder.



Note [Rules and join points]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Things get fiddly with rules. Suppose we have:

  let j :: Int -> Int
      j y = 2 * y
      k :: Int -> Int -> Int
      {-# RULES "SPEC k 0" k 0 = j #-}
      k x y = x + 2 * y
  in ...

Now suppose that both j and k appear only as saturated tail calls in the body.
Thus we would like to make them both join points. The rule complicates matters,
though, as its RHS has an unapplied occurrence of j. *However*, if we were to
eta-expand the rule, all would be well:

  {-# RULES "SPEC k 0" forall a. k 0 a = j a #-}

So conceivably we could notice that a potential join point would have an
"undersaturated" rule and account for it. This would mean we could make
something that's been specialised a join point, for instance. But local bindings
are rarely specialised, and being overly cautious about rules only
costs us anything when, for some `j`:

  * Before specialisation, `j` has non-tail calls, so it can't be a join point.
  * During specialisation, `j` gets specialised and thus acquires rules.
  * Sometime afterward, the non-tail calls to `j` disappear (as dead code, say),
    and so now `j` *could* become a join point.

This appears to be very rare in practice. TODO Perhaps we should gather
statistics to be sure.

------------------------------------------------------------


Note [Adjusting right-hand sides]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
There's a bit of a dance we need to do after analysing a lambda expression or
a right-hand side. In particular, we need to

  a) call 'markAllInsideLam' *unless* the binding is for a thunk, a one-shot
     lambda, or a non-recursive join point; and
  b) call 'markAllNonTailCalled' *unless* the binding is for a join point.

Some examples, with how the free occurrences in e (assumed not to be a value
lambda) get marked:

                             inside lam    non-tail-called
  ------------------------------------------------------------
  let x = e                  No            Yes
  let f = \x -> e            Yes           Yes
  let f = \x{OneShot} -> e   No            Yes
  \x -> e                    Yes           Yes
  join j x = e               No            No
  joinrec j x = e            Yes           No

There are a few other caveats; most importantly, if we're marking a binding as
'AlwaysTailCalled', it's *going* to be a join point, so we treat it as one so
that the effect cascades properly. Consequently, at the time the RHS is
analysed, we won't know what adjustments to make; thus 'occAnalLamOrRhs' must
return the unadjusted 'UsageDetails', to be adjusted by 'adjustRhsUsage' once
join-point-hood has been decided.

Thus the overall sequence taking place in 'occAnalNonRecBind' and
'occAnalRecBind' is as follows:

  1. Call 'occAnalLamOrRhs' to find usage information for the RHS.
  2. Call 'tagNonRecBinder' or 'tagRecBinders', which decides whether to make
     the binding a join point.
  3. Call 'adjustRhsUsage' accordingly. (Done as part of 'tagRecBinders' when
     recursive.)

(In the recursive case, this logic is spread between 'makeNode' and
'occAnalRec'.)
----------------------------------------------------------------
                 occAnalBind
----------------------------------------------------------------


Note [Unfoldings and join points]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We assume that anything in an unfolding occurs multiple times, since unfoldings
are often copied (that's the whole point!). But we still need to track tail
calls for the purpose of finding join points.
---------------------------


Note [Complexity of loop breaking]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The loop-breaking algorithm knocks out one binder at a time, and
performs a new SCC analysis on the remaining binders.  That can
behave very badly in tightly-coupled groups of bindings; in the
worst case it can be (N**2)*log N, because it does a full SCC
on N, then N-1, then N-2 and so on.

To avoid this, we switch plans after 2 (or whatever) attempts:
  Plan A: pick one binder with the lowest score, make it
          a loop breaker, and try again
  Plan B: pick *all* binders with the lowest score, make them
          all loop breakers, and try again
Since there are only a small finite number of scores, this will
terminate in a constant number of iterations, rather than O(N)
iterations.

You might thing that it's very unlikely, but RULES make it much
more likely.  Here's a real example from #1969:
  Rec { $dm = \d.\x. op d
        {-# RULES forall d. $dm Int d  = $s$dm1
                  forall d. $dm Bool d = $s$dm2 #-}

        dInt = MkD .... opInt ...
        dInt = MkD .... opBool ...
        opInt  = $dm dInt
        opBool = $dm dBool

        $s$dm1 = \x. op dInt
        $s$dm2 = \x. op dBool }
The RULES stuff means that we can't choose $dm as a loop breaker
(Note [Choosing loop breakers]), so we must choose at least (say)
opInt *and* opBool, and so on.  The number of loop breakders is
linear in the number of instance declarations.



Note [Loop breakers and INLINE/INLINABLE pragmas]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Avoid choosing a function with an INLINE pramga as the loop breaker!
If such a function is mutually-recursive with a non-INLINE thing,
then the latter should be the loop-breaker.

It's vital to distinguish between INLINE and INLINABLE (the
Bool returned by hasStableCoreUnfolding_maybe).  If we start with
   Rec { {-# INLINABLE f #-}
         f x = ...f... }
and then worker/wrapper it through strictness analysis, we'll get
   Rec { {-# INLINABLE $wf #-}
         $wf p q = let x = (p,q) in ...f...

         {-# INLINE f #-}
         f x = case x of (p,q) -> $wf p q }

Now it is vital that we choose $wf as the loop breaker, so we can
inline 'f' in '$wf'.



Note [DFuns should not be loop breakers]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It's particularly bad to make a DFun into a loop breaker.  See
Note [How instance declarations are translated] in TcInstDcls

We give DFuns a higher score than ordinary CONLIKE things because
if there's a choice we want the DFun to be the non-loop breaker. Eg

rec { sc = /\ a \$dC. $fBWrap (T a) ($fCT @ a $dC)

      $fCT :: forall a_afE. (Roman.C a_afE) => Roman.C (Roman.T a_afE)
      {-# DFUN #-}
      $fCT = /\a \$dC. MkD (T a) ((sc @ a $dC) |> blah) ($ctoF @ a $dC)
    }

Here 'sc' (the superclass) looks CONLIKE, but we'll never get to it
if we can't unravel the DFun first.



Note [Constructor applications]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It's really really important to inline dictionaries.  Real
example (the Enum Ordering instance from GHC.Base):

     rec     f = \ x -> case d of (p,q,r) -> p x
             g = \ x -> case d of (p,q,r) -> q x
             d = (v, f, g)

Here, f and g occur just once; but we can't inline them into d.
On the other hand we *could* simplify those case expressions if
we didn't stupidly choose d as the loop breaker.
But we won't because constructor args are marked "Many".
Inlining dictionaries is really essential to unravelling
the loops in static numeric dictionaries, see GHC.Float.



Note [Closure conversion]
~~~~~~~~~~~~~~~~~~~~~~~~~
We treat (\x. C p q) as a high-score candidate in the letrec scoring algorithm.
The immediate motivation came from the result of a closure-conversion transformation
which generated code like this:

    data Clo a b = forall c. Clo (c -> a -> b) c

    ($:) :: Clo a b -> a -> b
    Clo f env $: x = f env x

    rec { plus = Clo plus1 ()

        ; plus1 _ n = Clo plus2 n

        ; plus2 Zero     n = n
        ; plus2 (Succ m) n = Succ (plus $: m $: n) }

If we inline 'plus' and 'plus1', everything unravels nicely.  But if
we choose 'plus1' as the loop breaker (which is entirely possible
otherwise), the loop does not unravel nicely.


@occAnalUnfolding@ deals with the question of bindings where the Id is marked
by an INLINE pragma.  For these we record that anything which occurs
in its RHS occurs many times.  This pessimistically assumes that this
inlined binder also occurs many times in its scope, but if it doesn't
we'll catch it next time round.  At worst this costs an extra simplifier pass.
ToDo: try using the occurrence info for the inline'd binder.

[March 97] We do the same for atomic RHSs.  Reason: see notes with loopBreakSCC.
[June 98, SLPJ]  I've undone this change; I don't understand it.  See notes with loopBreakSCC.




Note [Self-recursion and loop breakers]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If we have
   rec { f = ...f...g...
       ; g = .....f...   }
then 'f' has to be a loop breaker anyway, so we may as well choose it
right away, so that g can inline freely.

This is really just a cheap hack. Consider
   rec { f = ...g...
       ; g = ..f..h...
      ;  h = ...f....}
Here f or g are better loop breakers than h; but we might accidentally
choose h.  Finding the minimal set of loop breakers is hard.



Note [Loop breakers, node scoring, and stability]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
To choose a loop breaker, we give a NodeScore to each node in the SCC,
and pick the one with the best score (according to 'betterLB').

We need to be jolly careful (#12425, #12234) about the stability
of this choice. Suppose we have

    let rec { f = ...g...g...
            ; g = ...f...f... }
    in
    case x of
      True  -> ...f..
      False -> ..f...

In each iteration of the simplifier the occurrence analyser OccAnal
chooses a loop breaker. Suppose in iteration 1 it choose g as the loop
breaker. That means it is free to inline f.

Suppose that GHC decides to inline f in the branches of the case, but
(for some reason; eg it is not saturated) in the rhs of g. So we get

    let rec { f = ...g...g...
            ; g = ...f...f... }
    in
    case x of
      True  -> ...g...g.....
      False -> ..g..g....

Now suppose that, for some reason, in the next iteration the occurrence
analyser chooses f as the loop breaker, so it can freely inline g. And
again for some reason the simplifier inlines g at its calls in the case
branches, but not in the RHS of f. Then we get

    let rec { f = ...g...g...
            ; g = ...f...f... }
    in
    case x of
      True  -> ...(...f...f...)...(...f..f..).....
      False -> ..(...f...f...)...(..f..f...)....

You can see where this is going! Each iteration of the simplifier
doubles the number of calls to f or g. No wonder GHC is slow!

(In the particular example in comment:3 of #12425, f and g are the two
mutually recursive fmap instances for CondT and Result. They are both
marked INLINE which, oddly, is why they don't inline in each other's
RHS, because the call there is not saturated.)

The root cause is that we flip-flop on our choice of loop breaker. I
always thought it didn't matter, and indeed for any single iteration
to terminate, it doesn't matter. But when we iterate, it matters a
lot!!

So The Plan is this:
   If there is a tie, choose the node that
   was a loop breaker last time round

Hence the is_lb field of NodeScore



Note [Join point RHSs]
~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
   x = e
   join j = Just x

We want to inline x into j right away, so we don't want to give
the join point a RhsCtxt (#14137).  It's not a huge deal, because
the FloatIn pass knows to float into join point RHSs; and the simplifier
does not float things out of join point RHSs.  But it's a simple, cheap
thing to do.  See #14137.



Note [Cascading inlines]
~~~~~~~~~~~~~~~~~~~~~~~~
By default we use an rhsCtxt for the RHS of a binding.  This tells the
occ anal n that it's looking at an RHS, which has an effect in
occAnalApp.  In particular, for constructor applications, it makes
the arguments appear to have NoOccInfo, so that we don't inline into
them. Thus    x = f y
              k = Just x
we do not want to inline x.

But there's a problem.  Consider
     x1 = a0 : []
     x2 = a1 : x1
     x3 = a2 : x2
     g  = f x3
First time round, it looks as if x1 and x2 occur as an arg of a
let-bound constructor ==> give them a many-occurrence.
But then x3 is inlined (unconditionally as it happens) and
next time round, x2 will be, and the next time round x1 will be
Result: multiple simplifier iterations.  Sigh.

So, when analysing the RHS of x3 we notice that x3 will itself
definitely inline the next time round, and so we analyse x3's rhs in
an ordinary context, not rhsCtxt.  Hence the "certainly_inline" stuff.

Annoyingly, we have to approximate SimplUtils.preInlineUnconditionally.
If (a) the RHS is expandable (see isExpandableApp in occAnalApp), and
   (b) certainly_inline says "yes" when preInlineUnconditionally says "no"
then the simplifier iterates indefinitely:
        x = f y
        k = Just x   -- We decide that k is 'certainly_inline'
        v = ...k...  -- but preInlineUnconditionally doesn't inline it
inline ==>
        k = Just (f y)
        v = ...k...
float ==>
        x1 = f y
        k = Just x1
        v = ...k...

This is worse than the slow cascade, so we only want to say "certainly_inline"
if it really is certain.  Look at the note with preInlineUnconditionally
for the various clauses.




Note [Gather occurrences of coercion variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We need to gather info about what coercion variables appear, so that
we can sort them into the right place when doing dependency analysis.


Note [Arguments of let-bound constructors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
    f x = let y = expensive x in
          let z = (True,y) in
          (case z of {(p,q)->q}, case z of {(p,q)->q})
We feel free to duplicate the WHNF (True,y), but that means
that y may be duplicated thereby.

If we aren't careful we duplicate the (expensive x) call!
Constructors are rather like lambdas in this way.


Note [Sources of one-shot information]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The occurrence analyser obtains one-shot-lambda information from two sources:

A:  Saturated applications:  eg   f e1 .. en

    In general, given a call (f e1 .. en) we can propagate one-shot info from
    f's strictness signature into e1 .. en, but /only/ if n is enough to
    saturate the strictness signature. A strictness signature like

          f :: C1(C1(L))LS

    means that *if f is applied to three arguments* then it will guarantee to
    call its first argument at most once, and to call the result of that at
    most once. But if f has fewer than three arguments, all bets are off; e.g.

          map (f (\x y. expensive) e2) xs

    Here the \x y abstraction may be called many times (once for each element of
    xs) so we should not mark x and y as one-shot. But if it was

          map (f (\x y. expensive) 3 2) xs

    then the first argument of f will be called at most once.

    The one-shot info, derived from f's strictness signature, is
    computed by 'argsOneShots', called in occAnalApp.

A': Non-obviously saturated applications: eg    build (f (\x y -> expensive))
    where f is as above.

    In this case, f is only manifestly applied to one argument, so it does not
    look saturated. So by the previous point, we should not use its strictness
    signature to learn about the one-shotness of \x y. But in this case we can:
    build is fully applied, so we may use its strictness signature; and from
    that we learn that build calls its argument with two arguments *at most once*.

    So there is really only one call to f, and it will have three arguments. In
    that sense, f is saturated, and we may proceed as described above.

    Hence the computation of 'guaranteed_val_args' in occAnalApp, using
    '(occ_one_shots env)'.  See also #13227, comment:9

B:  Let-bindings:  eg   let f = \c. let ... in \n -> blah
                        in (build f, build f)

    Propagate one-shot info from the demanand-info on 'f' to the
    lambdas in its RHS (which may not be syntactically at the top)

    This information must have come from a previous run of the demanand
    analyser.

Previously, the demand analyser would *also* set the one-shot information, but
that code was buggy (see #11770), so doing it only in on place, namely here, is
saner.



Note [OneShots]
~~~~~~~~~~~~~~~
When analysing an expression, the occ_one_shots argument contains information
about how the function is being used. The length of the list indicates
how many arguments will eventually be passed to the analysed expression,
and the OneShotInfo indicates whether this application is once or multiple times.

Example:

 Context of f                occ_one_shots when analysing f

 f 1 2                       [OneShot, OneShot]
 map (f 1)                   [OneShot, NoOneShotInfo]
 build f                     [OneShot, OneShot]
 f 1 2 `seq` f 2 1           [NoOneShotInfo, OneShot]



Note [Binders in case alternatives]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
    case x of y { (a,b) -> f y }
We treat 'a', 'b' as dead, because they don't physically occur in the
case alternative.  (Indeed, a variable is dead iff it doesn't occur in
its scope in the output of OccAnal.)  It really helps to know when
binders are unused.  See esp the call to isDeadBinder in
Simplify.mkDupableAlt

In this example, though, the Simplifier will bring 'a' and 'b' back to
life, beause it binds 'y' to (a,b) (imagine got inlined and
scrutinised y).


Note [Binder swap]
~~~~~~~~~~~~~~~~~~
The "binder swap" tranformation swaps occurence of the
scrutinee of a case for occurrences of the case-binder:

 (1)  case x of b { pi -> ri }
         ==>
      case x of b { pi -> let x=b in ri }

 (2)  case (x |> co) of b { pi -> ri }
        ==>
      case (x |> co) of b { pi -> let x = b |> sym co in ri }

In both cases, the trivial 'let' can be eliminated by the
immediately following simplifier pass.

There are two reasons for making this swap:

(A) It reduces the number of occurrences of the scrutinee, x.
    That in turn might reduce its occurrences to one, so we
    can inline it and save an allocation.  E.g.
      let x = factorial y in case x of b { I# v -> ...x... }
    If we replace 'x' by 'b' in the alternative we get
      let x = factorial y in case x of b { I# v -> ...b... }
    and now we can inline 'x', thus
      case (factorial y) of b { I# v -> ...b... }

(B) The case-binder b has unfolding information; in the
    example above we know that b = I# v. That in turn allows
    nested cases to simplify.  Consider
       case x of b { I# v ->
       ...(case x of b2 { I# v2 -> rhs })...
    If we replace 'x' by 'b' in the alternative we get
       case x of b { I# v ->
       ...(case b of b2 { I# v2 -> rhs })...
    and now it is trivial to simplify the inner case:
       case x of b { I# v ->
       ...(let b2 = b in rhs)...

    The same can happen even if the scrutinee is a variable
    with a cast: see Note [Case of cast]

In both cases, in a particular alternative (pi -> ri), we only
add the binding if
  (a) x occurs free in (pi -> ri)
        (ie it occurs in ri, but is not bound in pi)
  (b) the pi does not bind b (or the free vars of co)
We need (a) and (b) for the inserted binding to be correct.

For the alternatives where we inject the binding, we can transfer
all x's OccInfo to b.  And that is the point.

Notice that
  * The deliberate shadowing of 'x'.
  * That (a) rapidly becomes false, so no bindings are injected.

The reason for doing these transformations /here in the occurrence
analyser/ is because it allows us to adjust the OccInfo for 'x' and
'b' as we go.

  * Suppose the only occurrences of 'x' are the scrutinee and in the
    ri; then this transformation makes it occur just once, and hence
    get inlined right away.

  * If instead we do this in the Simplifier, we don't know whether 'x'
    is used in ri, so we are forced to pessimistically zap b's OccInfo
    even though it is typically dead (ie neither it nor x appear in
    the ri).  There's nothing actually wrong with zapping it, except
    that it's kind of nice to know which variables are dead.  My nose
    tells me to keep this information as robustly as possible.

The Maybe (Id,CoreExpr) passed to occAnalAlt is the extra let-binding
{x=b}; it's Nothing if the binder-swap doesn't happen.

There is a danger though.  Consider
      let v = x +# y
      in case (f v) of w -> ...v...v...
And suppose that (f v) expands to just v.  Then we'd like to
use 'w' instead of 'v' in the alternative.  But it may be too
late; we may have substituted the (cheap) x+#y for v in the
same simplifier pass that reduced (f v) to v.

I think this is just too bad.  CSE will recover some of it.



Note [Case of cast]
~~~~~~~~~~~~~~~~~~~
Consider        case (x `cast` co) of b { I# ->
                ... (case (x `cast` co) of {...}) ...
We'd like to eliminate the inner case.  That is the motivation for
equation (2) in Note [Binder swap].  When we get to the inner case, we
inline x, cancel the casts, and away we go.



Note [Binder swap on GlobalId scrutinees]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When the scrutinee is a GlobalId we must take care in two ways

 i) In order to *know* whether 'x' occurs free in the RHS, we need its
    occurrence info. BUT, we don't gather occurrence info for
    GlobalIds.  That's the reason for the (small) occ_gbl_scrut env in
    OccEnv is for: it says "gather occurrence info for these".

 ii) We must call localiseId on 'x' first, in case it's a GlobalId, or
     has an External Name. See, for example, SimplEnv Note [Global Ids in
     the substitution].



Note [Zap case binders in proxy bindings]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
From the original
     case x of cb(dead) { p -> ...x... }
we will get
     case x of cb(live) { p -> let x = cb in ...x... }

Core Lint never expects to find an *occurrence* of an Id marked
as Dead, so we must zap the OccInfo on cb before making the
binding x = cb.  See #5028.

NB: the OccInfo on /occurrences/ really doesn't matter much; the simplifier
doesn't use it. So this is only to satisfy the perhpas-over-picky Lint.

Historical note [no-case-of-case]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We *used* to suppress the binder-swap in case expressions when
-fno-case-of-case is on.  Old remarks:
    "This happens in the first simplifier pass,
    and enhances full laziness.  Here's the bad case:
            f = \ y -> ...(case x of I# v -> ...(case x of ...) ... )
    If we eliminate the inner case, we trap it inside the I# v -> arm,
    which might prevent some full laziness happening.  I've seen this
    in action in spectral/cichelli/Prog.hs:
             [(m,n) | m <- [1..max], n <- [1..max]]
    Hence the check for NoCaseOfCase."
However, now the full-laziness pass itself reverses the binder-swap, so this
check is no longer necessary.

Historical note [Suppressing the case binder-swap]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This old note describes a problem that is also fixed by doing the
binder-swap in OccAnal:

    There is another situation when it might make sense to suppress the
    case-expression binde-swap. If we have

        case x of w1 { DEFAULT -> case x of w2 { A -> e1; B -> e2 }
                       ...other cases .... }

    We'll perform the binder-swap for the outer case, giving

        case x of w1 { DEFAULT -> case w1 of w2 { A -> e1; B -> e2 }
                       ...other cases .... }

    But there is no point in doing it for the inner case, because w1 can't
    be inlined anyway.  Furthermore, doing the case-swapping involves
    zapping w2's occurrence info (see paragraphs that follow), and that
    forces us to bind w2 when doing case merging.  So we get

        case x of w1 { A -> let w2 = w1 in e1
                       B -> let w2 = w1 in e2
                       ...other cases .... }

    This is plain silly in the common case where w2 is dead.

    Even so, I can't see a good way to implement this idea.  I tried
    not doing the binder-swap if the scrutinee was already evaluated
    but that failed big-time:

            data T = MkT !Int

            case v of w  { MkT x ->
            case x of x1 { I# y1 ->
            case x of x2 { I# y2 -> ...

    Notice that because MkT is strict, x is marked "evaluated".  But to
    eliminate the last case, we must either make sure that x (as well as
    x1) has unfolding MkT y1.  The straightforward thing to do is to do
    the binder-swap.  So this whole note is a no-op.

It's fixed by doing the binder-swap in OccAnal because we can do the
binder-swap unconditionally and still get occurrence analysis
information right.


Note [UsageDetails and zapping]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

On many occasions, we must modify all gathered occurrence data at once. For
instance, all occurrences underneath a (non-one-shot) lambda set the
'occ_in_lam' flag to become 'True'. We could use 'mapVarEnv' to do this, but
that takes O(n) time and we will do this often---in particular, there are many
places where tail calls are not allowed, and each of these causes all variables
to get marked with 'NoTailCallInfo'.

Instead of relying on `mapVarEnv`, then, we carry three 'IdEnv's around along
with the 'OccInfoEnv'. Each of these extra environments is a "zapped set"
recording which variables have been zapped in some way. Zapping all occurrence
info then simply means setting the corresponding zapped set to the whole
'OccInfoEnv', a fast O(1) operation.


Note [Do not mark CoVars as dead]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It's obviously wrong to mark CoVars as dead if they are used.
Currently we don't traverse types to gather usase info for CoVars,
so we had better treat them as having noOccInfo.

This showed up in #15696 we had something like
  case eq_sel d of co -> ...(typeError @(...co...) "urk")...

Then 'd' was substitued by a dictionary, so the expression
simpified to
  case (Coercion <blah>) of co -> ...(typeError @(...co...) "urk")...

But then the "drop the case altogether" equation of rebuildCase
thought that 'co' was dead, and discarded the entire case. Urk!

I have no idea how we managed to avoid this pitfall for so long!
-----------------
 Auxiliary functions for UsageDetails implementation


Note [Join points and INLINE pragmas]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
   f x = let g = \x. not  -- Arity 1
             {-# INLINE g #-}
         in case x of
              A -> g True True
              B -> g True False
              C -> blah2

Here 'g' is always tail-called applied to 2 args, but the stable
unfolding captured by the INLINE pragma has arity 1.  If we try to
convert g to be a join point, its unfolding will still have arity 1
(since it is stable, and we don't meddle with stable unfoldings), and
Lint will complain (see Note [Invariants on join points], (2a), in
CoreSyn.  #13413.

Moreover, since g is going to be inlined anyway, there is no benefit
from making it a join point.

If it is recursive, and uselessly marked INLINE, this will stop us
making it a join point, which is annoying.  But occasionally
(notably in class methods; see Note [Instances and loop breakers] in
TcInstDcls) we mark recursive things as INLINE but the recursion
unravels; so ignoring INLINE pragmas on recursive things isn't good
either.

See Invariant 2a of Note [Invariants on join points] in CoreSyn


