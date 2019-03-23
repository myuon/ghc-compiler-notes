Note [WorkList priorities]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
A WorkList contains canonical and non-canonical items (of all flavors).
Notice that each Ct now has a simplification depth. We may
consider using this depth for prioritization as well in the future.

As a simple form of priority queue, our worklist separates out

* equalities (wl_eqs); see Note [Prioritise equalities]
* type-function equalities (wl_funeqs)
* all the rest (wl_rest)



Note [Prioritise equalities]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It's very important to process equalities /first/:

* (Efficiency)  The general reason to do so is that if we process a
  class constraint first, we may end up putting it into the inert set
  and then kicking it out later.  That's extra work compared to just
  doing the equality first.

* (Avoiding fundep iteration) As #14723 showed, it's possible to
  get non-termination if we
      - Emit the Derived fundep equalities for a class constraint,
        generating some fresh unification variables.
      - That leads to some unification
      - Which kicks out the class constraint
      - Which isn't solved (because there are still some more Derived
        equalities in the work-list), but generates yet more fundeps
  Solution: prioritise derived equalities over class constraints

* (Class equalities) We need to prioritise equalities even if they
  are hidden inside a class constraint;
  see Note [Prioritise class equalities]

* (Kick-out) We want to apply this priority scheme to kicked-out
  constraints too (see the call to extendWorkListCt in kick_out_rewritable
  E.g. a CIrredCan can be a hetero-kinded (t1 ~ t2), which may become
  homo-kinded when kicked out, and hence we want to priotitise it.

* (Derived equalities) Originally we tried to postpone processing
  Derived equalities, in the hope that we might never need to deal
  with them at all; but in fact we must process Derived equalities
  eagerly, partly for the (Efficiency) reason, and more importantly
  for (Avoiding fundep iteration).



Note [Prioritise class equalities]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We prioritise equalities in the solver (see selectWorkItem). But class
constraints like (a ~ b) and (a ~~ b) are actually equalities too;
see Note [The equality types story] in TysPrim.

Failing to prioritise these is inefficient (more kick-outs etc).
But, worse, it can prevent us spotting a "recursive knot" among
Wanted constraints.  See comment:10 of #12734 for a worked-out
example.

So we arrange to put these particular class constraints in the wl_eqs.

  NB: since we do not currently apply the substitution to the
  inert_solved_dicts, the knot-tying still seems a bit fragile.
  But this makes it better.
See Note [WorkList priorities]


Note [Solved dictionaries]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we apply a top-level instance declaration, we add the "solved"
dictionary to the inert_solved_dicts.  In general, we use it to avoid
creating a new EvVar when we have a new goal that we have solved in
the past.

But in particular, we can use it to create *recursive* dictionaries.
The simplest, degnerate case is
    instance C [a] => C [a] where ...
If we have
    [W] d1 :: C [x]
then we can apply the instance to get
    d1 = $dfCList d
    [W] d2 :: C [x]
Now 'd1' goes in inert_solved_dicts, and we can solve d2 directly from d1.
    d1 = $dfCList d
    d2 = d1

See Note [Example of recursive dictionaries]
Other notes about solved dictionaries

* See also Note [Do not add superclasses of solved dictionaries]

* The inert_solved_dicts field is not rewritten by equalities,
  so it may get out of date.

* THe inert_solved_dicts are all Wanteds, never givens

* We only cache dictionaries from top-level instances, not from
  local quantified constraints.  Reason: if we cached the latter
  we'd need to purge the cache when bringing new quantified
  constraints into scope, because quantified constraints "shadow"
  top-level instances.



Note [Do not add superclasses of solved dictionaries]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Every member of inert_solved_dicts is the result of applying a dictionary
function, NOT of applying superclass selection to anything.
Consider

        class Ord a => C a where
        instance Ord [a] => C [a] where ...

Suppose we are trying to solve
  [G] d1 : Ord a
  [W] d2 : C [a]

Then we'll use the instance decl to give

  [G] d1 : Ord a     Solved: d2 : C [a] = $dfCList d3
  [W] d3 : Ord [a]

We must not add d4 : Ord [a] to the 'solved' set (by taking the
superclass of d2), otherwise we'll use it to solve d3, without ever
using d1, which would be a catastrophe.

Solution: when extending the solved dictionaries, do not add superclasses.
That's why each element of the inert_solved_dicts is the result of applying
a dictionary function.



Note [Example of recursive dictionaries]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--- Example 1

    data D r = ZeroD | SuccD (r (D r));

    instance (Eq (r (D r))) => Eq (D r) where
        ZeroD     == ZeroD     = True
        (SuccD a) == (SuccD b) = a == b
        _         == _         = False;

    equalDC :: D [] -> D [] -> Bool;
    equalDC = (==);

We need to prove (Eq (D [])). Here's how we go:

   [W] d1 : Eq (D [])
By instance decl of Eq (D r):
   [W] d2 : Eq [D []]      where   d1 = dfEqD d2
By instance decl of Eq [a]:
   [W] d3 : Eq (D [])      where   d2 = dfEqList d3
                                   d1 = dfEqD d2
Now this wanted can interact with our "solved" d1 to get:
    d3 = d1

-- Example 2:
This code arises in the context of "Scrap Your Boilerplate with Class"

    class Sat a
    class Data ctx a
    instance  Sat (ctx Char)             => Data ctx Char       -- dfunData1
    instance (Sat (ctx [a]), Data ctx a) => Data ctx [a]        -- dfunData2

    class Data Maybe a => Foo a

    instance Foo t => Sat (Maybe t)                             -- dfunSat

    instance Data Maybe a => Foo a                              -- dfunFoo1
    instance Foo a        => Foo [a]                            -- dfunFoo2
    instance                 Foo [Char]                         -- dfunFoo3

Consider generating the superclasses of the instance declaration
         instance Foo a => Foo [a]

So our problem is this
    [G] d0 : Foo t
    [W] d1 : Data Maybe [t]   -- Desired superclass

We may add the given in the inert set, along with its superclasses
  Inert:
    [G] d0 : Foo t
    [G] d01 : Data Maybe t   -- Superclass of d0
  WorkList
    [W] d1 : Data Maybe [t]

Solve d1 using instance dfunData2; d1 := dfunData2 d2 d3
  Inert:
    [G] d0 : Foo t
    [G] d01 : Data Maybe t   -- Superclass of d0
  Solved:
        d1 : Data Maybe [t]
  WorkList:
    [W] d2 : Sat (Maybe [t])
    [W] d3 : Data Maybe t

Now, we may simplify d2 using dfunSat; d2 := dfunSat d4
  Inert:
    [G] d0 : Foo t
    [G] d01 : Data Maybe t   -- Superclass of d0
  Solved:
        d1 : Data Maybe [t]
        d2 : Sat (Maybe [t])
  WorkList:
    [W] d3 : Data Maybe t
    [W] d4 : Foo [t]

Now, we can just solve d3 from d01; d3 := d01
  Inert
    [G] d0 : Foo t
    [G] d01 : Data Maybe t   -- Superclass of d0
  Solved:
        d1 : Data Maybe [t]
        d2 : Sat (Maybe [t])
  WorkList
    [W] d4 : Foo [t]

Now, solve d4 using dfunFoo2;  d4 := dfunFoo2 d5
  Inert
    [G] d0  : Foo t
    [G] d01 : Data Maybe t   -- Superclass of d0
  Solved:
        d1 : Data Maybe [t]
        d2 : Sat (Maybe [t])
        d4 : Foo [t]
  WorkList:
    [W] d5 : Foo t

Now, d5 can be solved! d5 := d0

Result
   d1 := dfunData2 d2 d3
   d2 := dfunSat d4
   d3 := d01
   d4 := dfunFoo2 d5
   d5 := d0


Note [Detailed InertCans Invariants]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The InertCans represents a collection of constraints with the following properties:

  * All canonical

  * No two dictionaries with the same head
  * No two CIrreds with the same type

  * Family equations inert wrt top-level family axioms

  * Dictionaries have no matching top-level instance

  * Given family or dictionary constraints don't mention touchable
    unification variables

  * Non-CTyEqCan constraints are fully rewritten with respect
    to the CTyEqCan equalities (modulo canRewrite of course;
    eg a wanted cannot rewrite a given)

  * CTyEqCan equalities: see Note [Applying the inert substitution]
                         in TcFlatten



Note [EqualCtList invariants]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    * All are equalities
    * All these equalities have the same LHS
    * The list is never empty
    * No element of the list can rewrite any other
    * Derived before Wanted

From the fourth invariant it follows that the list is
   - A single [G], or
   - Zero or one [D] or [WD], followd by any number of [W]

The Wanteds can't rewrite anything which is why we put them last



Note [Type family equations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Type-family equations, CFunEqCans, of form (ev : F tys ~ ty),
live in three places

  * The work-list, of course

  * The inert_funeqs are un-solved but fully processed, and in
    the InertCans. They can be [G], [W], [WD], or [D].

  * The inert_flat_cache.  This is used when flattening, to get maximal
    sharing. Everthing in the inert_flat_cache is [G] or [WD]

    It contains lots of things that are still in the work-list.
    E.g Suppose we have (w1: F (G a) ~ Int), and (w2: H (G a) ~ Int) in the
        work list.  Then we flatten w1, dumping (w3: G a ~ f1) in the work
        list.  Now if we flatten w2 before we get to w3, we still want to
        share that (G a).
    Because it contains work-list things, DO NOT use the flat cache to solve
    a top-level goal.  Eg in the above example we don't want to solve w3
    using w3 itself!

The CFunEqCan Ownership Invariant:

  * Each [G/W/WD] CFunEqCan has a distinct fsk or fmv
    It "owns" that fsk/fmv, in the sense that:
      - reducing a [W/WD] CFunEqCan fills in the fmv
      - unflattening a [W/WD] CFunEqCan fills in the fmv
      (in both cases unless an occurs-check would result)

  * In contrast a [D] CFunEqCan does not "own" its fmv:
      - reducing a [D] CFunEqCan does not fill in the fmv;
        it just generates an equality
      - unflattening ignores [D] CFunEqCans altogether




Note [inert_eqs: the inert equalities]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Definition [Can-rewrite relation]
A "can-rewrite" relation between flavours, written f1 >= f2, is a
binary relation with the following properties

  (R1) >= is transitive
  (R2) If f1 >= f, and f2 >= f,
       then either f1 >= f2 or f2 >= f1

Lemma.  If f1 >= f then f1 >= f1
Proof.  By property (R2), with f1=f2

Definition [Generalised substitution]
A "generalised substitution" S is a set of triples (a -f-> t), where
  a is a type variable
  t is a type
  f is a flavour
such that
  (WF1) if (a -f1-> t1) in S
           (a -f2-> t2) in S
        then neither (f1 >= f2) nor (f2 >= f1) hold
  (WF2) if (a -f-> t) is in S, then t /= a

Definition [Applying a generalised substitution]
If S is a generalised substitution
   S(f,a) = t,  if (a -fs-> t) in S, and fs >= f
          = a,  otherwise
Application extends naturally to types S(f,t), modulo roles.
See Note [Flavours with roles].

Theorem: S(f,a) is well defined as a function.
Proof: Suppose (a -f1-> t1) and (a -f2-> t2) are both in S,
               and  f1 >= f and f2 >= f
       Then by (R2) f1 >= f2 or f2 >= f1, which contradicts (WF1)

Notation: repeated application.
  S^0(f,t)     = t
  S^(n+1)(f,t) = S(f, S^n(t))

Definition: inert generalised substitution
A generalised substitution S is "inert" iff

  (IG1) there is an n such that
        for every f,t, S^n(f,t) = S^(n+1)(f,t)

By (IG1) we define S*(f,t) to be the result of exahaustively
applying S(f,_) to t.

----------------------------------------------------------------
Our main invariant:
   the inert CTyEqCans should be an inert generalised substitution
----------------------------------------------------------------

Note that inertness is not the same as idempotence.  To apply S to a
type, you may have to apply it recursive.  But inertness does
guarantee that this recursive use will terminate.



Note [Extending the inert equalities]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Main Theorem [Stability under extension]
   Suppose we have a "work item"
       a -fw-> t
   and an inert generalised substitution S,
   THEN the extended substitution T = S+(a -fw-> t)
        is an inert generalised substitution
   PROVIDED
      (T1) S(fw,a) = a     -- LHS of work-item is a fixpoint of S(fw,_)
      (T2) S(fw,t) = t     -- RHS of work-item is a fixpoint of S(fw,_)
      (T3) a not in t      -- No occurs check in the work item

      AND, for every (b -fs-> s) in S:
           (K0) not (fw >= fs)
                Reason: suppose we kick out (a -fs-> s),
                        and add (a -fw-> t) to the inert set.
                        The latter can't rewrite the former,
                        so the kick-out achieved nothing

           OR { (K1) not (a = b)
                     Reason: if fw >= fs, WF1 says we can't have both
                             a -fw-> t  and  a -fs-> s

                AND (K2): guarantees inertness of the new substitution
                    {  (K2a) not (fs >= fs)
                    OR (K2b) fs >= fw
                    OR (K2d) a not in s }

                AND (K3) See Note [K3: completeness of solving]
                    { (K3a) If the role of fs is nominal: s /= a
                      (K3b) If the role of fs is representational:
                            s is not of form (a t1 .. tn) } }


Conditions (T1-T3) are established by the canonicaliser
Conditions (K1-K3) are established by TcSMonad.kickOutRewritable

The idea is that
* (T1-2) are guaranteed by exhaustively rewriting the work-item
  with S(fw,_).

* T3 is guaranteed by a simple occurs-check on the work item.
  This is done during canonicalisation, in canEqTyVar;
  (invariant: a CTyEqCan never has an occurs check).

* (K1-3) are the "kick-out" criteria.  (As stated, they are really the
  "keep" criteria.) If the current inert S contains a triple that does
  not satisfy (K1-3), then we remove it from S by "kicking it out",
  and re-processing it.

* Note that kicking out is a Bad Thing, because it means we have to
  re-process a constraint.  The less we kick out, the better.
  TODO: Make sure that kicking out really *is* a Bad Thing. We've assumed
  this but haven't done the empirical study to check.

* Assume we have  G>=G, G>=W and that's all.  Then, when performing
  a unification we add a new given  a -G-> ty.  But doing so does NOT require
  us to kick out an inert wanted that mentions a, because of (K2a).  This
  is a common case, hence good not to kick out.

* Lemma (L2): if not (fw >= fw), then K0 holds and we kick out nothing
  Proof: using Definition [Can-rewrite relation], fw can't rewrite anything
         and so K0 holds.  Intuitively, since fw can't rewrite anything,
         adding it cannot cause any loops
  This is a common case, because Wanteds cannot rewrite Wanteds.
  It's used to avoid even looking for constraint to kick out.

* Lemma (L1): The conditions of the Main Theorem imply that there is no
              (a -fs-> t) in S, s.t.  (fs >= fw).
  Proof. Suppose the contrary (fs >= fw).  Then because of (T1),
  S(fw,a)=a.  But since fs>=fw, S(fw,a) = s, hence s=a.  But now we
  have (a -fs-> a) in S, which contradicts (WF2).

* The extended substitution satisfies (WF1) and (WF2)
  - (K1) plus (L1) guarantee that the extended substitution satisfies (WF1).
  - (T3) guarantees (WF2).

* (K2) is about inertness.  Intuitively, any infinite chain T^0(f,t),
  T^1(f,t), T^2(f,T).... must pass through the new work item infinitely
  often, since the substitution without the work item is inert; and must
  pass through at least one of the triples in S infinitely often.

  - (K2a): if not(fs>=fs) then there is no f that fs can rewrite (fs>=f),
    and hence this triple never plays a role in application S(f,a).
    It is always safe to extend S with such a triple.

    (NB: we could strengten K1) in this way too, but see K3.

  - (K2b): If this holds then, by (T2), b is not in t.  So applying the
    work item does not generate any new opportunities for applying S

  - (K2c): If this holds, we can't pass through this triple infinitely
    often, because if we did then fs>=f, fw>=f, hence by (R2)
      * either fw>=fs, contradicting K2c
      * or fs>=fw; so by the argument in K2b we can't have a loop

  - (K2d): if a not in s, we hae no further opportunity to apply the
    work item, similar to (K2b)

  NB: Dimitrios has a PDF that does this in more detail

Key lemma to make it watertight.
  Under the conditions of the Main Theorem,
  forall f st fw >= f, a is not in S^k(f,t), for any k

Also, consider roles more carefully. See Note [Flavours with roles]



Note [K3: completeness of solving]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(K3) is not necessary for the extended substitution
to be inert.  In fact K1 could be made stronger by saying
   ... then (not (fw >= fs) or not (fs >= fs))
But it's not enough for S to be inert; we also want completeness.
That is, we want to be able to solve all soluble wanted equalities.
Suppose we have

   work-item   b -G-> a
   inert-item  a -W-> b

Assuming (G >= W) but not (W >= W), this fulfills all the conditions,
so we could extend the inerts, thus:

   inert-items   b -G-> a
                 a -W-> b

But if we kicked-out the inert item, we'd get

   work-item     a -W-> b
   inert-item    b -G-> a

Then rewrite the work-item gives us (a -W-> a), which is soluble via Refl.
So we add one more clause to the kick-out criteria

Another way to understand (K3) is that we treat an inert item
        a -f-> b
in the same way as
        b -f-> a
So if we kick out one, we should kick out the other.  The orientation
is somewhat accidental.

When considering roles, we also need the second clause (K3b). Consider

  work-item    c -G/N-> a
  inert-item   a -W/R-> b c

The work-item doesn't get rewritten by the inert, because (>=) doesn't hold.
But we don't kick out the inert item because not (W/R >= W/R).  So we just
add the work item. But then, consider if we hit the following:

  work-item    b -G/N-> Id
  inert-items  a -W/R-> b c
               c -G/N-> a
where
  newtype Id x = Id x

For similar reasons, if we only had (K3a), we wouldn't kick the
representational inert out. And then, we'd miss solving the inert, which
now reduced to reflexivity.

The solution here is to kick out representational inerts whenever the
tyvar of a work item is "exposed", where exposed means being at the
head of the top-level application chain (a t1 .. tn).  See
TcType.isTyVarHead. This is encoded in (K3b).

Beware: if we make this test succeed too often, we kick out too much,
and the solver might loop.  Consider (#14363)
  work item:   [G] a ~R f b
  inert item:  [G] b ~R f a
In GHC 8.2 the completeness tests more aggressive, and kicked out
the inert item; but no rewriting happened and there was an infinite
loop.  All we need is to have the tyvar at the head.



Note [Flavours with roles]
~~~~~~~~~~~~~~~~~~~~~~~~~~
The system described in Note [inert_eqs: the inert equalities]
discusses an abstract
set of flavours. In GHC, flavours have two components: the flavour proper,
taken from {Wanted, Derived, Given} and the equality relation (often called
role), taken from {NomEq, ReprEq}.
When substituting w.r.t. the inert set,
as described in Note [inert_eqs: the inert equalities],
we must be careful to respect all components of a flavour.
For example, if we have

  inert set: a -G/R-> Int
             b -G/R-> Bool

  type role T nominal representational

and we wish to compute S(W/R, T a b), the correct answer is T a Bool, NOT
T Int Bool. The reason is that T's first parameter has a nominal role, and
thus rewriting a to Int in T a b is wrong. Indeed, this non-congruence of
substitution means that the proof in Note [The inert equalities] may need
to be revisited, but we don't think that the end conclusion is wrong.


Note [lookupFlattenTyVar]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have an injective function F and
  inert_funeqs:   F t1 ~ fsk1
                  F t2 ~ fsk2
  inert_eqs:      fsk1 ~ fsk2

We never rewrite the RHS (cc_fsk) of a CFunEqCan.  But we /do/ want to
get the [D] t1 ~ t2 from the injectiveness of F.  So we look up the
cc_fsk of CFunEqCans in the inert_eqs when trying to find derived
equalities arising from injectivity.


Note [Do not add duplicate quantified instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this (#15244):

  f :: (C g, D g) => ....
  class S g => C g where ...
  class S g => D g where ...
  class (forall a. Eq a => Eq (g a)) => S g where ...

Then in f's RHS there are two identical quantified constraints
available, one via the superclasses of C and one via the superclasses
of D.  The two are identical, and it seems wrong to reject the program
because of that. But without doing duplicate-elimination we will have
two matching QCInsts when we try to solve constraints arising from f's
RHS.

The simplest thing is simply to eliminate duplicattes, which we do here.


Note [kickOutRewritable]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
See also Note [inert_eqs: the inert equalities].

When we add a new inert equality (a ~N ty) to the inert set,
we must kick out any inert items that could be rewritten by the
new equality, to maintain the inert-set invariants.

  - We want to kick out an existing inert constraint if
    a) the new constraint can rewrite the inert one
    b) 'a' is free in the inert constraint (so that it *will*)
       rewrite it if we kick it out.

    For (b) we use tyCoVarsOfCt, which returns the type variables /and
    the kind variables/ that are directly visible in the type. Hence
    we will have exposed all the rewriting we care about to make the
    most precise kinds visible for matching classes etc. No need to
    kick out constraints that mention type variables whose kinds
    contain this variable!

  - A Derived equality can kick out [D] constraints in inert_eqs,
    inert_dicts, inert_irreds etc.

  - We don't kick out constraints from inert_solved_dicts, and
    inert_solved_funeqs optimistically. But when we lookup we have to
    take the substitution into account




Note [Rewrite insolubles]
~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have an insoluble alpha ~ [alpha], which is insoluble
because an occurs check.  And then we unify alpha := [Int].  Then we
really want to rewrite the insoluble to [Int] ~ [[Int]].  Now it can
be decomposed.  Otherwise we end up with a "Can't match [Int] ~
[[Int]]" which is true, but a bit confusing because the outer type
constructors match.

Similarly, if we have a CHoleCan, we'd like to rewrite it with any
Givens, to give as informative an error messasge as possible
(#12468, #11325).

Hence:
 * In the main simlifier loops in TcSimplify (solveWanteds,
   simpl_loop), we feed the insolubles in solveSimpleWanteds,
   so that they get rewritten (albeit not solved).

 * We kick insolubles out of the inert set, if they can be
   rewritten (see TcSMonad.kick_out_rewritable)

 * We rewrite those insolubles in TcCanonical.
   See Note [Make sure that insolubles are fully rewritten]
------------


Note [Unsolved Derived equalities]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In getUnsolvedInerts, we return a derived equality from the inert_eqs
because it is a candidate for floating out of this implication.  We
only float equalities with a meta-tyvar on the left, so we only pull
those out here.



Note [When does an implication have given equalities?]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider an implication
   beta => alpha ~ Int
where beta is a unification variable that has already been unified
to () in an outer scope.  Then we can float the (alpha ~ Int) out
just fine. So when deciding whether the givens contain an equality,
we should canonicalise first, rather than just looking at the original
givens (#8644).

So we simply look at the inert, canonical Givens and see if there are
any equalities among them, the calculation of has_given_eqs.  There
are some wrinkles:

 * We must know which ones are bound in *this* implication and which
   are bound further out.  We can find that out from the TcLevel
   of the Given, which is itself recorded in the tcl_tclvl field
   of the TcLclEnv stored in the Given (ev_given_here).

   What about interactions between inner and outer givens?
      - Outer given is rewritten by an inner given, then there must
        have been an inner given equality, hence the “given-eq” flag
        will be true anyway.

      - Inner given rewritten by outer, retains its level (ie. The inner one)

 * We must take account of *potential* equalities, like the one above:
      beta => ...blah...
   If we still don't know what beta is, we conservatively treat it as potentially
   becoming an equality. Hence including 'irreds' in the calculation or has_given_eqs.

 * When flattening givens, we generate Given equalities like
     <F [a]> : F [a] ~ f,
   with Refl evidence, and we *don't* want those to count as an equality
   in the givens!  After all, the entire flattening business is just an
   internal matter, and the evidence does not mention any of the 'givens'
   of this implication.  So we do not treat inert_funeqs as a 'given equality'.

 * See Note [Let-bound skolems] for another wrinkle

 * We do *not* need to worry about representational equalities, because
   these do not affect the ability to float constraints.



Note [Let-bound skolems]
~~~~~~~~~~~~~~~~~~~~~~~~
If   * the inert set contains a canonical Given CTyEqCan (a ~ ty)
and  * 'a' is a skolem bound in this very implication,

then:
a) The Given is pretty much a let-binding, like
      f :: (a ~ b->c) => a -> a
   Here the equality constraint is like saying
      let a = b->c in ...
   It is not adding any new, local equality  information,
   and hence can be ignored by has_given_eqs

b) 'a' will have been completely substituted out in the inert set,
   so we can safely discard it.  Notably, it doesn't need to be
   returned as part of 'fsks'

For an example, see #9211.

See also TcUnify Note [Deeper level on the left] for how we ensure
that the right variable is on the left of the equality when both are
tyvars.

You might wonder whether the skokem really needs to be bound "in the
very same implication" as the equuality constraint.
(c.f. #15009) Consider this:

  data S a where
    MkS :: (a ~ Int) => S a

  g :: forall a. S a -> a -> blah
  g x y = let h = \z. ( z :: Int
                      , case x of
                           MkS -> [y,z])
          in ...

From the type signature for `g`, we get `y::a` .  Then when when we
encounter the `\z`, we'll assign `z :: alpha[1]`, say.  Next, from the
body of the lambda we'll get

  [W] alpha[1] ~ Int                             -- From z::Int
  [W] forall[2]. (a ~ Int) => [W] alpha[1] ~ a   -- From [y,z]

Now, suppose we decide to float `alpha ~ a` out of the implication
and then unify `alpha := a`.  Now we are stuck!  But if treat
`alpha ~ Int` first, and unify `alpha := Int`, all is fine.
But we absolutely cannot float that equality or we will get stuck.


Note [Tuples hiding implicit parameters]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
   f,g :: (?x::Int, C a) => a -> a
   f v = let ?x = 4 in g v

The call to 'g' gives rise to a Wanted constraint (?x::Int, C a).
We must /not/ solve this from the Given (?x::Int, C a), because of
the intervening binding for (?x::Int).  #14218.

We deal with this by arranging that we always fail when looking up a
tuple constraint that hides an implicit parameter. Not that this applies
  * both to the inert_dicts (lookupInertDict)
  * and to the solved_dicts (looukpSolvedDict)
An alternative would be not to extend these sets with such tuple
constraints, but it seemed more direct to deal with the lookup.



Note [Solving CallStack constraints]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose f :: HasCallStack => blah.  Then

* Each call to 'f' gives rise to
    [W] s1 :: IP "callStack" CallStack    -- CtOrigin = OccurrenceOf f
  with a CtOrigin that says "OccurrenceOf f".
  Remember that HasCallStack is just shorthand for
    IP "callStack CallStack
  See Note [Overview of implicit CallStacks] in TcEvidence

* We cannonicalise such constraints, in TcCanonical.canClassNC, by
  pushing the call-site info on the stack, and changing the CtOrigin
  to record that has been done.
   Bind:  s1 = pushCallStack <site-info> s2
   [W] s2 :: IP "callStack" CallStack   -- CtOrigin = IPOccOrigin

* Then, and only then, we can solve the constraint from an enclosing
  Given.

So we must be careful /not/ to solve 's1' from the Givens.  Again,
we ensure this by arranging that findDict always misses when looking
up souch constraints.


Note [The TcS monad]
~~~~~~~~~~~~~~~~~~~~
The TcS monad is a weak form of the main Tc monad

All you can do is
    * fail
    * allocate new variables
    * fill in evidence variables

Filling in a dictionary evidence variable means to create a binding
for it, so TcS carries a mutable location where the binding can be
added.  This is initialised from the innermost implication constraint.


Note [Do not inherit the flat cache]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We do not want to inherit the flat cache when processing nested
implications.  Consider
   a ~ F b, forall c. b~Int => blah
If we have F b ~ fsk in the flat-cache, and we push that into the
nested implication, we might miss that F b can be rewritten to F Int,
and hence perhpas solve it.  Moreover, the fsk from outside is
flattened out after solving the outer level, but and we don't
do that flattening recursively.


Note [Propagate the solved dictionaries]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It's really quite important that nestTcS does not discard the solved
dictionaries from the thing_inside.
Consider
   Eq [a]
   forall b. empty =>  Eq [a]
We solve the simple (Eq [a]), under nestTcS, and then turn our attention to
the implications.  It's definitely fine to use the solved dictionaries on
the inner implications, and it can make a signficant performance difference
if you do so.
Getters and setters of TcEnv fields
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Getter of inerts and worklist


Note [Residual implications]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The wl_implics in the WorkList are the residual implication
constraints that are generated while solving or canonicalising the
current worklist.  Specifically, when canonicalising
   (forall a. t1 ~ forall a. t2)
from which we get the implication
   (forall a. t1 ~ t2)
See TcSMonad.deferTcSForAllEq
