`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/simplCore/SimplEnv.hs>`_

====================
compiler/simplCore/SimplEnv.hs.rst
====================

Note [SimplEnv invariants]
~~~~~~~~~~~~~~~~~~~~~~~~~~
seInScope:
        The in-scope part of Subst includes *all* in-scope TyVars and Ids
        The elements of the set may have better IdInfo than the
        occurrences of in-scope Ids, and (more important) they will
        have a correctly-substituted type.  So we use a lookup in this
        set to replace occurrences

.. code-block:: haskell

        The Ids in the InScopeSet are replete with their Rules,
        and as we gather info about the unfolding of an Id, we replace
        it in the in-scope set.

.. code-block:: haskell

        The in-scope set is actually a mapping OutVar -> OutVar, and
        in case expressions we sometimes bind

seIdSubst:
        The substitution is *apply-once* only, because InIds and OutIds
        can overlap.
        For example, we generally omit mappings
                a77 -> a77
        from the substitution, when we decide not to clone a77, but it's quite
        legitimate to put the mapping in the substitution anyway.

.. code-block:: haskell

        Furthermore, consider
                let x = case k of I# x77 -> ... in
                let y = case k of I# x77 -> ... in ...
        and suppose the body is strict in both x and y.  Then the simplifier
        will pull the first (case k) to the top; so the second (case k) will
        cancel out, mapping x77 to, well, x77!  But one is an in-Id and the
        other is an out-Id.

.. code-block:: haskell

        Of course, the substitution *must* applied! Things in its domain
        simply aren't necessarily bound in the result.

* substId adds a binding (DoneId new_id) to the substitution if
        the Id's unique has changed

.. code-block:: haskell

  Note, though that the substitution isn't necessarily extended
  if the type of the Id changes.  Why not?  Because of the next point:

* We *always, always* finish by looking up in the in-scope set
  any variable that doesn't get a DoneEx or DoneVar hit in the substitution.
  Reason: so that we never finish up with a "old" Id in the result.
  An old Id might point to an old unfolding and so on... which gives a space
  leak.

.. code-block:: haskell

  [The DoneEx and DoneVar hits map to "new" stuff.]

* It follows that substExpr must not do a no-op if the substitution is empty.
  substType is free to do so, however.

* When we come to a let-binding (say) we generate new IdInfo, including an
  unfolding, attach it to the binder, and add this newly adorned binder to
  the in-scope set.  So all subsequent occurrences of the binder will get
  mapped to the full-adorned binder, which is also the one put in the
  binding site.

* The in-scope "set" usually maps x->x; we use it simply for its domain.
  But sometimes we have two in-scope Ids that are synomyms, and should
  map to the same target:  x->x, y->x.  Notably:
        case y of x { ... }
  That's why the "set" is actually a VarEnv Var



Note [Join arity in SimplIdSubst]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We have to remember which incoming variables are join points: the occurrences
may not be marked correctly yet, and we're in change of propagating the change if
OccurAnal makes something a join point).

Normally the in-scope set is where we keep the latest information, but
the in-scope set tracks only OutVars; if a binding is unconditionally
inlined (via DoneEx), it never makes it into the in-scope set, and we
need to know at the occurrence site that the variable is a join point
so that we know to drop the context. Thus we remember which join
points we're substituting. 


Note [WildCard binders]
~~~~~~~~~~~~~~~~~~~~~~~
The program to be simplified may have wild binders
    case e of wild { p -> ... }
We want to *rename* them away, so that there are no
occurrences of 'wild-id' (with wildCardKey).  The easy
way to do that is to start of with a representative
Id in the in-scope set

There can be *occurrences* of wild-id.  For example,
MkCore.mkCoreApp transforms
   e (a /# b)   -->   case (a /# b) of wild { DEFAULT -> e wild }
This is ok provided 'wild' isn't free in 'e', and that's the delicate
thing. Generally, you want to run the simplifier to get rid of the
wild-ids before doing much else.

It's a very dark corner of GHC.  Maybe it should be cleaned up.


Note [Setting the right in-scope set]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
  \x. (let x = e in b) arg[x]
where the let shadows the lambda.  Really this means something like
  \x1. (let x2 = e in b) arg[x1]

- When we capture the 'arg' in an ApplyToVal continuation, we capture
  the environment, which says what 'x' is bound to, namely x1

- Then that continuation gets pushed under the let

- Finally we simplify 'arg'.  We want
     - the static, lexical environment bindig x :-> x1
     - the in-scopeset from "here", under the 'let' which includes
       both x1 and x2

It's important to have the right in-scope set, else we may rename a
variable to one that is already in scope.  So we must pick up the
in-scope set from "here", but otherwise use the environment we
captured along with 'arg'.  This transfer of in-scope set is done by
setInScopeFromE.
-------------------


Note [LetFloats]
~~~~~~~~~~~~~~~~
The LetFloats is a bunch of bindings, classified by a FloatFlag.

* All of them satisfy the let/app invariant

Examples

.. code-block:: haskell

  NonRec x (y:ys)       FltLifted
  Rec [(x,rhs)]         FltLifted

.. code-block:: haskell

  NonRec x* (p:q)       FltOKSpec   -- RHS is WHNF.  Question: why not FltLifted?
  NonRec x# (y +# 3)    FltOkSpec   -- Unboxed, but ok-for-spec'n

.. code-block:: haskell

  NonRec x* (f y)       FltCareful  -- Strict binding; might fail or diverge

Can't happen:
  NonRec x# (a /# b)    -- Might fail; does not satisfy let/app
  NonRec x# (f y)       -- Might diverge; does not satisfy let/app


Note [Float when cheap or expandable]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We want to float a let from a let if the residual RHS is
   a) cheap, such as (\x. blah)
   b) expandable, such as (f b) if f is CONLIKE
But there are
  - cheap things that are not expandable (eg \x. expensive)
  - expandable things that are not cheap (eg (f b) where b is CONLIKE)
so we must take the 'or' of the two.


Note [Global Ids in the substitution]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We look up even a global (eg imported) Id in the substitution. Consider
   case X.g_34 of b { (a,b) ->  ... case X.g_34 of { (p,q) -> ...} ... }
The binder-swap in the occurrence analyser will add a binding
for a LocalId version of g (with the same unique though):
   case X.g_34 of b { (a,b) ->  let g_34 = b in
                                ... case X.g_34 of { (p,q) -> ...} ... }
So we want to look up the inner X.g_34 in the substitution, where we'll
find that it has been substituted by b.  (Or conceivably cloned.)


Note [Return type for join points]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider

.. code-block:: haskell

   (join j :: Char -> Int -> Int) 77
   (     j x = \y. y + ord x    )
   (in case v of                )
   (     A -> j 'x'             )
   (     B -> j 'y'             )
   (     C -> <blah>            )

The simplifier pushes the "apply to 77" continuation inwards to give

.. code-block:: haskell

   join j :: Char -> Int
        j x = (\y. y + ord x) 77
   in case v of
        A -> j 'x'
        B -> j 'y'
        C -> <blah> 77

Notice that the "apply to 77" continuation went into the RHS of the
join point.  And that meant that the return type of the join point
changed!!

That's why we pass res_ty into simplNonRecJoinBndr, and substIdBndr
takes a (Just res_ty) argument so that it knows to do the type-changing
thing.


Note [Arity robustness]
~~~~~~~~~~~~~~~~~~~~~~~
We *do* transfer the arity from from the in_id of a let binding to the
out_id.  This is important, so that the arity of an Id is visible in
its own RHS.  For example:
        f = \x. ....g (\y. f y)....
We can eta-reduce the arg to g, because f is a value.  But that
needs to be visible.

This interacts with the 'state hack' too:
        f :: Bool -> IO Int
        f = \x. case x of
                  True  -> f y
                  False -> \s -> ...
Can we eta-expand f?  Only if we see that f has arity 1, and then we
take advantage of the 'state hack' on the result of
(f y) :: State# -> (State#, Int) to expand the arity one more.

There is a disadvantage though.  Making the arity visible in the RHS
allows us to eta-reduce
        f = \x -> f x
to
        f = f
which technically is not sound.   This is very much a corner case, so
I'm not worried about it.  Another idea is to ensure that f's arity
never decreases; its arity started as 1, and we should never eta-reduce
below that.




Note [Robust OccInfo]
~~~~~~~~~~~~~~~~~~~~~
It's important that we *do* retain the loop-breaker OccInfo, because
that's what stops the Id getting inlined infinitely, in the body of
the letrec.

