[[src]](https://github.com/ghc/ghc/tree/master/compiler/simplCore/OccurAnal.hs)

(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

# Occurrence analysis pass

The occurrence analyser re-typechecks a core expression, returning a new
core expression with (hopefully) improved usage information.


# occurAnalysePgm, occurAnalyseExpr, occurAnalyseExpr_NoBinderSwap


Here's the externally-callable interface:


### Note: Plugin rules

Conal Elliott (Trac #11651) built a GHC plugin that added some
BuiltinRules (for imported Ids) to the mg_rules field of ModGuts, to
do some domain-specific transformations that could not be expressed
with an ordinary pattern-matching CoreRule.  But then we can't extract
the dependencies (in imp_rule_edges) from ru_rhs etc, because a
BuiltinRule doesn't have any of that stuff.

So we simply assume that BuiltinRules have no dependencies, and filter
them out from the imp_rule_edges comprehension.


# Bindings


### Note: Recursive bindings: the grand plan

When we come across a binding group
  Rec { x1 = r1; ...; xn = rn }
we treat it like this (occAnalRecBind):

1. Occurrence-analyse each right hand side, and build a
   "Details" for each binding to capture the results.

### Note: Forming the Rec groups

   All this is done by makeNode.

2. Do SCC-analysis on these Nodes.  Each SCC will become a new Rec or
   NonRec.  The key property is that every free variable of a binding
   is accounted for by the scope edges, so that when we are done
   everything is still in scope.

3. For each Cyclic SCC of the scope-edge SCC-analysis in (2), we
   identify suitable loop-breakers to ensure that inlining terminates.
   This is done by occAnalRec.

### Note: Choosing loop breakers

   More edges: if f calls g, and g has an active rule that mentions h
               then we add an edge from f -> h

   Fewer edges: we only include dependencies on active rules, on rule
                RHSs (not LHSs) and if there is an INLINE pragma only
                on the stable unfolding (and vice versa).  The scope
                edges must be much more inclusive.

### Note: Weak loop breakers

6.  Having formed the loop-breaker nodes

### Note: Dead code

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

### Note: Forming Rec groups

We put bindings {f = ef; g = eg } in a Rec group if "f uses g"
and "g uses f", no matter how indirectly.  We do a SCC analysis
with an edge f -> g if "f uses g".

### Note: Rules are extra RHSs

These conditions apply regardless of the activation of the RULE (eg it might be
inactive in this phase but become active later).  Once a Rec is broken up
it can never be put back together, so we must be conservative.

The principle is that, regardless of rule firings, every variable is
always in scope.

### Note: Rules are extra RHSs

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

### Note: Rule dependency info

    The VarSet in a RuleInfo is used for dependency analysis in the
    occurrence analyser.  We must track free vars in *both* lhs and rhs.
    Hence use of idRuleVars, rather than idRuleRhsVars in occAnalBind.
    Why both? Consider
        x = y
        RULE f x = v+4
    Then if we substitute y for x, we'd better do so in the
    rule's LHS too, so we'd better ensure the RULE appears to mention 'x'
    as well as 'v'

### Note: Rules are visible in their own rec group

### Note: Specialisation rules

    This means that we must simplify the RULEs first, before looking
    at any of the definitions.  This is done by Simplify.simplRecBind,
    when it calls addLetIdInfo.

### Note: Choosing loop breakers

Loop breaking is surprisingly subtle.  First read the section 4 of
"Secrets of the GHC inliner".  This describes our basic plan.
We avoid infinite inlinings by choosing loop breakers, and
ensuring that a loop breaker cuts each loop.

### Note: Inlining and hs-boot files

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

### Note: Finding rule RHS free vars

### Note: Rule dependency info

### Note: Inline rules

### Note: Finding rule RHS free vars

Note that in Example [eftInt], *neither* eftInt *nor* eftIntFB is
chosen as a loop breaker, because their RHSs don't mention each other.
And indeed both can be inlined safely.

Note again that the edges of the graph we use for computing loop breakers
are not the same as the edges we use for computing the Rec blocks.
That's why we compute

- rec_edges          for the Rec block analysis
- loop_breaker_nodes for the loop breaker analysis

### Note: Finding rule RHS free vars

# RULES "tagZero" [~1] forall xs n.
             pmap fromBool <blah blah> = tagZero xs #

# RULES
  "filter"     [~1] forall p xs.  filter p xs = build (\c n -> foldr
  (filterFB c p) n xs)
  "filterList" [1]  forall p.     foldr (filterFB (:) p) [] = filter p
   #

# RULES
  "eftInt"  [~1] forall x y. eftInt x y = build (\ c n -> eftIntFB c n x y)
  "eftIntList"  [1] eftIntFB  (:) [] = eftInt
   #

### Note: Unfoldings and join points


We assume that anything in an unfolding occurs multiple times, since unfoldings
are often copied (that's the whole point!). But we still need to track tail
calls for the purpose of finding join points.



loopBreakNodes is applied to the list of nodes for a cyclic strongly
connected component (there's guaranteed to be a cycle).  It returns
the same nodes, but
        a) in a better order,
        b) with some of the Ids having a IAmALoopBreaker pragma

The "loop-breaker" Ids are sufficient to break all cycles in the SCC.  This means
that the simplifier can guarantee not to loop provided it never records an inlining
for these no-inline guys.

Furthermore, the order of the binds is such that if we neglect dependencies
on the no-inline Ids then the binds are topologically sorted.  This means
that the simplifier will generally do a good job if it works from top bottom,
recording inlinings for any Ids which aren't marked as "no-inline" as it goes.


### Note: Complexity of loop breaking

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
more likely.  Here's a real example from Trac #1969:
  Rec { $dm = \d.\x. op d
        {-# RULES forall d. $dm Int d  = $s$dm1
                  forall d. $dm Bool d = $s$dm2 #

### Note: Self-recursion and loop breakers

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

### Note: Loop breakers, node scoring, and stability

To choose a loop breaker, we give a NodeScore to each node in the SCC,
and pick the one with the best score (according to 'betterLB').

We need to be jolly careful (Trac #12425, #12234) about the stability
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

# Right hand sides


### Note: Join point RHSs

Consider
   x = e
   join j = Just x

We want to inline x into j right away, so we don't want to give
the join point a RhsCtxt (Trac #14137).  It's not a huge deal, because
the FloatIn pass knows to float into join point RHSs; and the simplifier
does not float things out of join point RHSs.  But it's a simple, cheap
thing to do.  See Trac #14137.

### Note: Cascading inlines

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

# Expressions


### Note: Gather occurrences of coercion variables

We need to gather info about what coercion variables appear, so that
we can sort them into the right place when doing dependency analysis.



Applications are dealt with specially because we want
the "build hack" to work.

### Note: Arguments of let-bound constructors

Consider
    f x = let y = expensive x in
          let z = (True,y) in
          (case z of {(p,q)->q}, case z of {(p,q)->q})
We feel free to duplicate the WHNF (True,y), but that means
that y may be duplicated thereby.

If we aren't careful we duplicate the (expensive x) call!
Constructors are rather like lambdas in this way.


### Note: Sources of one-shot information

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
    '(occ_one_shots env)'.  See also Trac #13227, comment:9

B:  Let-bindings:  eg   let f = \c. let ... in \n -> blah
                        in (build f, build f)

    Propagate one-shot info from the demanand-info on 'f' to the
    lambdas in its RHS (which may not be syntactically at the top)

    This information must have come from a previous run of the demanand
    analyser.

Previously, the demand analyser would *also* set the one-shot information, but
that code was buggy (see #11770), so doing it only in on place, namely here, is
saner.

### Note: OneShots

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

### Note: Binders in case alternatives

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


# OccEnv


# Binder swap


### Note: Binder swap

We do these two transformations right here:

 (1)   case x of b { pi -> ri }
    ==>
      case x of b { pi -> let x=b in ri }

 (2)  case (x |> co) of b { pi -> ri }
    ==>
      case (x |> co) of b { pi -> let x = b |> sym co in ri }

### Note: Case of cast

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

The reason for doing these transformations here is because it allows
us to adjust the OccInfo for 'x' and 'b' as we go.

  * Suppose the only occurrences of 'x' are the scrutinee and in the
    ri; then this transformation makes it occur just once, and hence
    get inlined right away.

  * If we do this in the Simplifier, we don't know whether 'x' is used
    in ri, so we are forced to pessimistically zap b's OccInfo even
    though it is typically dead (ie neither it nor x appear in the
    ri).  There's nothing actually wrong with zapping it, except that
    it's kind of nice to know which variables are dead.  My nose
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

### Note: Case of cast

### Note: Binder swap

### Note: Binder swap on GlobalId scrutinees

When the scrutinee is a GlobalId we must take care in two ways

 i) In order to *know* whether 'x' occurs free in the RHS, we need its
    occurrence info. BUT, we don't gather occurrence info for
    GlobalIds.  That's the reason for the (small) occ_gbl_scrut env in
    OccEnv is for: it says "gather occurrence info for these".

 ii) We must call localiseId on 'x' first, in case it's a GlobalId, or
     has an External Name. See, for example, SimplEnv Note [Global Ids in
     the substitution].

### Note: Zap case binders in proxy bindings

From the original
     case x of cb(dead) { p -> ...x... }
we will get
     case x of cb(live) { p -> let x = cb in ...x... }

Core Lint never expects to find an *occurrence* of an Id marked
as Dead, so we must zap the OccInfo on cb before making the
binding x = cb.  See Trac #5028.

# note [no-case-of-case]

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

# note [Suppressing the case binder-swap]

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


# \subsection[OccurAnal-types]{OccEnv}


### Note: UsageDetails and zapping


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


### Note: Join points and INLINE pragmas