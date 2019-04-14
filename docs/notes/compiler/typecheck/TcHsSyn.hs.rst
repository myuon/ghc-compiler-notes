`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcHsSyn.hs>`_

compiler/typecheck/TcHsSyn.hs
=============================


Note [The ZonkEnv]
~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcHsSyn.hs#L208>`__

* ze_flexi :: ZonkFlexi says what to do with a
  unification variable that is still un-unified.
  See Note [Un-unified unification variables]

* ze_tv_env :: TyCoVarEnv TyCoVar promotes sharing. At a binding site
  of a tyvar or covar, we zonk the kind right away and add a mapping
  to the env. This prevents re-zonking the kind at every
  occurrence. But this is *just* an optimisation.

* ze_id_env : IdEnv Id promotes sharing among Ids, by making all
  occurrences of the Id point to a single zonked copy, built at the
  binding site.

  Unlike ze_tv_env, it is knot-tied: see extendIdZonkEnvRec.
  In a mutually recusive group
     rec { f = ...g...; g = ...f... }
  we want the occurrence of g to point to the one zonked Id for g,
  and the same for f.

  Because it is knot-tied, we must be careful to consult it lazily.
  Specifically, zonkIdOcc is not monadic.

* ze_meta_tv_env: see Note [Sharing when zonking to Type]


Notes:
  * We must be careful never to put coercion variables (which are Ids,
    after all) in the knot-tied ze_id_env, because coercions can
    appear in types, and we sometimes inspect a zonked type in this
    module.  [Question: where, precisely?]

  * In zonkTyVarOcc we consult ze_tv_env in a monadic context,
    a second reason that ze_tv_env can't be monadic.

  * An obvious suggestion would be to have one VarEnv Var to
    replace both ze_id_env and ze_tv_env, but that doesn't work
    because of the knot-tying stuff mentioned above.



Note [Un-unified unification variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcHsSyn.hs#L248>`__

What should we do if we find a Flexi unification variable?
There are three possibilities:

* DefaultFlexi: this is the common case, in situations like
     length @alpha ([] @alpha)
  It really doesn't matter what type we choose for alpha.  But
  we must choose a type!  We can't leae mutable unification
  variables floating around: after typecheck is complete, every
  type variable occurrence must have a bindign site.

::

  So we default it to 'Any' of the right kind.

..

::

  All this works for both type and kind variables (indeed
  the two are the same thign).

..

* SkolemiseFlexi: is a special case for the LHS of RULES.
  See Note [Zonking the LHS of a RULE]

* RuntimeUnkFlexi: is a special case for the GHCi debugger.
  It's a way to have a variable that is not a mutuable
  unification variable, but doesn't have a binding site
  either.



Note [Skolems in zonkSyntaxExpr]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcHsSyn.hs#L968>`__

Consider rebindable syntax with something like

::

  (>>=) :: (forall x. blah) -> (forall y. blah') -> blah''

..

The x and y become skolems that are in scope when type-checking the
arguments to the bind. This means that we must extend the ZonkEnv with
these skolems when zonking the arguments to the bind. But the skolems
are different between the two arguments, and so we should theoretically
carry around different environments to use for the different arguments.

However, this becomes a logistical nightmare, especially in dealing with
the more exotic Stmt forms. So, we simplify by making the critical
assumption that the uniques of the skolems are different. (This assumption
is justified by the use of newUnique in TcMType.instSkolTyCoVarX.)
Now, we can safely just extend one environment.

::

 See Note [Skolems in zonkSyntaxExpr]

..



Note [Optimise coercion zonking]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcHsSyn.hs#L1698>`__

When optimising evidence binds we may come across situations where
a coercion looks like
      cv = ReflCo ty
or    cv1 = cv2
where the type 'ty' is big.  In such cases it is a waste of time to zonk both
  * The variable on the LHS
  * The coercion on the RHS
Rather, we can zonk the variable, and if its type is (ty ~ ty), we can just
use Refl on the right, ignoring the actual coercion on the RHS.

This can have a very big effect, because the constraint solver sometimes does go
to a lot of effort to prove Refl!  (Eg when solving  10+3 = 10+3; cf #5030)



Note [Sharing when zonking to Type]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcHsSyn.hs#L1721>`__

Problem:

    In TcMType.zonkTcTyVar, we short-circuit (Indirect ty) to
    (Indirect zty), see Note [Sharing in zonking] in TcMType. But we
    /can't/ do this when zonking a TcType to a Type (#15552, esp
    comment:3).  Suppose we have

       alpha -> alpha
         where
            alpha is already unified:
             alpha := T{tc-tycon} Int -> Int
         and T is knot-tied

    By "knot-tied" I mean that the occurrence of T is currently a TcTyCon,
    but the global env contains a mapping "T" :-> T{knot-tied-tc}. See
    Note [Type checking recursive type and class declarations] in
    TcTyClsDecls.

    Now we call zonkTcTypeToType on that (alpha -> alpha). If we follow
    the same path as Note [Sharing in zonking] in TcMType, we'll
    update alpha to
       alpha := T{knot-tied-tc} Int -> Int

::

    But alas, if we encounter alpha for a /second/ time, we end up
    looking at T{knot-tied-tc} and fall into a black hole. The whole
    point of zonkTcTypeToType is that it produces a type full of
    knot-tied tycons, and you must not look at the result!!

..

::

    To put it another way (zonkTcTypeToType . zonkTcTypeToType) is not
    the same as zonkTcTypeToType. (If we distinguished TcType from
    Type, this issue would have been a type error!)

..

Solution: (see #15552 for other variants)

::

    One possible solution is simply not to do the short-circuiting.
    That has less sharing, but maybe sharing is rare. And indeed,
    that turns out to be viable from a perf point of view

..

::

    But the code implements something a bit better

..

    * ZonkEnv contains ze_meta_tv_env, which maps
          from a MetaTyVar (unificaion variable)
          to a Type (not a TcType)

    * In zonkTyVarOcc, we check this map to see if we have zonked
      this variable before. If so, use the previous answer; if not
      zonk it, and extend the map.

    * The map is of course stateful, held in a TcRef. (That is unlike
      the treatment of lexically-scoped variables in ze_tv_env and
      ze_id_env.)

::

    Is the extra work worth it?  Some non-sytematic perf measurements
    suggest that compiler allocation is reduced overall (by 0.5% or so)
    but compile time really doesn't change.

..



Note [Zonking the LHS of a RULE]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcHsSyn.hs#L1923>`__

See also DsBinds Note [Free tyvars on rule LHS]

We need to gather the type variables mentioned on the LHS so we can
quantify over them.  Example:
  data T a = C

::

  foo :: T a -> Int
  foo C = 1

..

::

  {-# RULES "myrule"  foo C = 1 #-}

..

After type checking the LHS becomes (foo alpha (C alpha)) and we do
not want to zap the unbound meta-tyvar 'alpha' to Any, because that
limits the applicability of the rule.  Instead, we want to quantify
over it!

We do this in two stages.

* During zonking, we skolemise the TcTyVar 'alpha' to TyVar 'a'.  We
  do this by using zonkTvSkolemising as the UnboundTyVarZonker in the
  ZonkEnv.  (This is in fact the whole reason that the ZonkEnv has a
  UnboundTyVarZonker.)

* In DsBinds, we quantify over it.  See DsBinds
  Note [Free tyvars on rule LHS]

Quantifying here is awkward because (a) the data type is big and (b)
finding the free type vars of an expression is necessarily monadic
operation. (consider /\a -> f @ b, where b is side-effected to a)

