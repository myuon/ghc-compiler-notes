`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcRnDriver.hs>`_

compiler/typecheck/TcRnDriver.hs
================================


Note [DFun impedance matching]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcRnDriver.hs#L730>`__

We return a list of "impedance-matching" bindings for the dfuns
defined in the hs-boot file, such as
          $fxEqT = $fEqT
We need these because the module and hi-boot file might differ in
the name it chose for the dfun: the name of a dfun is not
uniquely determined by its type; there might be multiple dfuns
which, individually, would map to the same name (in which case
we have to disambiguate them.)  There's no way for the hi file
to know exactly what disambiguation to use... without looking
at the hi-boot file itself.

In fact, the names will always differ because we always pick names
prefixed with "$fx" for boot dfuns, and "$f" for real dfuns
(so that this impedance matching is always possible).



Note [DFun knot-tying]
~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcRnDriver.hs#L747>`__

The 'SelfBootInfo' that is fed into 'checkHiBootIface' comes from
typechecking the hi-boot file that we are presently implementing.
Suppose we are typechecking the module A: when we typecheck the
hi-boot file, whenever we see an identifier A.T, we knot-tie this
identifier to the *local* type environment (via if_rec_types.)  The
contract then is that we don't *look* at 'SelfBootInfo' until we've
finished typechecking the module and updated the type environment with
the new tycons and ids.

This most works well, but there is one problem: DFuns!  We do not want
to look at the mb_insts of the ModDetails in SelfBootInfo, because a
dfun in one of those ClsInsts is gotten (in TcIface.tcIfaceInst) by a
(lazily evaluated) lookup in the if_rec_types.  We could extend the
type env, do a setGloblaTypeEnv etc; but that all seems very indirect.
It is much more directly simply to extract the DFunIds from the
md_types of the SelfBootInfo.

See #4003, #16038 for why we need to take care here.



Note [Root-main Id]
~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcRnDriver.hs#L1820>`__

The function that the RTS invokes is always :Main.main, which we call
root_main_id.  (Because GHC allows the user to have a module not
called Main as the main module, we can't rely on the main function
being called "Main.main".  That's why root_main_id has a fixed module
":Main".)

This is unusual: it's a LocalId whose Name has a Module from another
module.  Tiresomely, we must filter it out again in MkIface, les we
get two defns for 'main' in the interface file!



Note [Initialising the type environment for GHCi]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcRnDriver.hs#L1921>`__

Most of the Ids in ic_things, defined by the user in 'let' stmts,
have closed types. E.g.
   ghci> let foo x y = x && not y

However the GHCi debugger creates top-level bindings for Ids whose
types have free RuntimeUnk skolem variables, standing for unknown
types.  If we don't register these free TyVars as global TyVars then
the typechecker will try to quantify over them and fall over in
skolemiseQuantifiedTyVar. so we must add any free TyVars to the
typechecker's global TyVar set.  That is most conveniently by using
tcExtendLocalTypeEnv, which automatically extends the global TyVar
set.

We do this by splitting out the Ids with open types, using 'is_closed'
to do the partition.  The top-level things go in the global TypeEnv;
the open, NotTopLevel, Ids, with free RuntimeUnk tyvars, go in the
local TypeEnv.

Note that we don't extend the local RdrEnv (tcl_rdr); all the in-scope
things are already in the interactive context's GlobalRdrEnv.
Extending the local RdrEnv isn't terrible, but it means there is an
entry for the same Name in both global and local RdrEnvs, and that
lead to duplicate "perhaps you meant..." suggestions (e.g. T5564).

We don't bother with the tcl_th_bndrs environment either.



Note [Deferred type errors in GHCi]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcRnDriver.hs#L2150>`__

In GHCi, we ensure that type errors don't get deferred when type checking the
naked expressions. Deferring type errors here is unhelpful because the
expression gets evaluated right away anyway. It also would potentially emit
two redundant type-error warnings, one from each plan.

#14963 reveals another bug that when deferred type errors is enabled
in GHCi, any reference of imported/loaded variables (directly or indirectly)
in interactively issued naked expressions will cause ghc panic. See more
detailed dicussion in #14963.

The interactively issued declarations, statements, as well as the modules
loaded into GHCi, are not affected. That means, for declaration, you could
have

::

    Prelude> :set -fdefer-type-errors
    Prelude> x :: IO (); x = putStrLn True
    <interactive>:14:26: warning: [-Wdeferred-type-errors]
        ? Couldn't match type ‘Bool’ with ‘[Char]’
          Expected type: String
            Actual type: Bool
        ? In the first argument of ‘putStrLn’, namely ‘True’
          In the expression: putStrLn True
          In an equation for ‘x’: x = putStrLn True

But for naked expressions, you will have

::

    Prelude> :set -fdefer-type-errors
    Prelude> putStrLn True
    <interactive>:2:10: error:
        ? Couldn't match type ‘Bool’ with ‘[Char]’
          Expected type: String
            Actual type: Bool
        ? In the first argument of ‘putStrLn’, namely ‘True’
          In the expression: putStrLn True
          In an equation for ‘it’: it = putStrLn True

::

    Prelude> let x = putStrLn True
    <interactive>:2:18: warning: [-Wdeferred-type-errors]
        ? Couldn't match type ‘Bool’ with ‘[Char]’
          Expected type: String
            Actual type: Bool
        ? In the first argument of ‘putStrLn’, namely ‘True’
          In the expression: putStrLn True
          In an equation for ‘x’: x = putStrLn True



Note [GHCi Plans]
~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcRnDriver.hs#L2237>`__

When a user types an expression in the repl we try to print it in three different
ways. Also, depending on whether -fno-it is set, we bind a variable called `it`
which can be used to refer to the result of the expression subsequently in the repl.

The normal plans are :
  A. [it <- e; print e]     but not if it::()
  B. [it <- e]
  C. [let it = e; print it]

When -fno-it is set, the plans are:
  A. [e >>= print]
  B. [e]
  C. [let it = e in print it]

The reason for -fno-it is explained in #14336. `it` can lead to the repl
leaking memory as it is repeatedly queried.



Note [TcRnExprMode]
~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcRnDriver.hs#L2461>`__

How should we infer a type when a user asks for the type of an expression e
at the GHCi prompt? We offer 3 different possibilities, described below. Each
considers this example, with -fprint-explicit-foralls enabled:

::

  foo :: forall a f b. (Show a, Num b, Foldable f) => a -> f b -> String
  :type{,-spec,-def} foo @Int

:type / TM_Inst

::

  In this mode, we report the type that would be inferred if a variable
  were assigned to expression e, without applying the monomorphism restriction.
  This means we deeply instantiate the type and then regeneralize, as discussed
  in #11376.

::

  > :type foo @Int
  forall {b} {f :: * -> *}. (Foldable f, Num b) => Int -> f b -> String

::

  Note that the variables and constraints are reordered here, because this
  is possible during regeneralization. Also note that the variables are
  reported as Inferred instead of Specified.

:type +v / TM_NoInst

::

  This mode is for the benefit of users using TypeApplications. It does no
  instantiation whatsoever, sometimes meaning that class constraints are not
  solved.

::

  > :type +v foo @Int
  forall f b. (Show Int, Num b, Foldable f) => Int -> f b -> String

::

  Note that Show Int is still reported, because the solver never got a chance
  to see it.

:type +d / TM_Default

::

  This mode is for the benefit of users who wish to see instantiations of
  generalized types, and in particular to instantiate Foldable and Traversable.
  In this mode, any type variable that can be defaulted is defaulted. Because
  GHCi uses -XExtendedDefaultRules, this means that Foldable and Traversable are
  defaulted.

::

  > :type +d foo @Int
  Int -> [Integer] -> String

::

  Note that this mode can sometimes lead to a type error, if a type variable is
  used with a defaultable class but cannot actually be defaulted:

::

  bar :: (Num a, Monoid a) => a -> a
  > :type +d bar
  ** error **

::

  The error arises because GHC tries to default a but cannot find a concrete
  type in the defaulting list that is both Num and Monoid. (If this list is
  modified to include an element that is both Num and Monoid, the defaulting
  would succeed, of course.)



Note [Kind-generalise in tcRnType]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcRnDriver.hs#L2519>`__

We switch on PolyKinds when kind-checking a user type, so that we will
kind-generalise the type, even when PolyKinds is not otherwise on.
This gives the right default behaviour at the GHCi prompt, where if
you say ":k T", and T has a polymorphic kind, you'd like to see that
polymorphism. Of course.  If T isn't kind-polymorphic you won't get
anything unexpected, but the apparent *loss* of polymorphism, for
types that you know are polymorphic, is quite surprising.  See Trac
#7688 for a discussion.

Note that the goal is to generalise the *kind of the type*, not
the type itself! Example:
  ghci> data SameKind :: k -> k -> Type
  ghci> :k SameKind _

We want to get `k -> Type`, not `Any -> Type`, which is what we would
get without kind-generalisation. Note that `:k SameKind` is OK, as
GHC will not instantiate SameKind here, and so we see its full kind
of `forall k. k -> k -> Type`.

