[[src]](https://github.com/ghc/ghc/tree/master/compiler/stranal/WorkWrap.hs)

(c) The GRASP/AQUA Project, Glasgow University, 1993-1998

# Worker/wrapper-generating back-end of strictness analyser


We take Core bindings whose binders have:

\begin{enumerate}

\item Strictness attached (by the front-end of the strictness
analyser), and / or

\item Constructed Product Result information attached by the CPR
analysis pass.

\end{enumerate}

and we return some ``plain'' bindings which have been
worker/wrapper-ified, meaning:

\begin{enumerate}

\item Functions have been split into workers and wrappers where
appropriate.  If a function has both strictness and CPR properties
then only one worker/wrapper doing both transformations is produced;

\item Binders' @IdInfos@ have been updated to reflect the existence of
these workers/wrappers (this is where we get STRICTNESS and CPR pragma
info for exported values).
\end{enumerate}


# \subsection[wwBind-wwExpr]{@wwBind@ and @wwExpr@}


@wwBind@ works on a binding, trying each \tr{(binder, expr)} pair in
turn.  Non-recursive case first, then recursive...



@wwExpr@ basically just walks the tree, looking for appropriate
annotations that can be used. Remember it is @wwBind@ that does the
matching by looking for strict arguments of the correct type.
@wwExpr@ is a version that just returns the ``Plain'' Tree.


# \subsection[tryWW]{@tryWW@: attempt a worker/wrapper pair}


@tryWW@ just accumulates arguments, converts strictness info from the
front-end into the proper form, then calls @mkWwBodies@ to do
the business.

The only reason this is monadised is for the unique supply.

### Note: Don't w/w INLINE things

It's very important to refrain from w/w-ing an INLINE function (ie one
with a stable unfolding) because the wrapper will then overwrite the
old stable unfolding with the wrapper code.

Furthermore, if the programmer has marked something as INLINE,
we may lose by w/w'ing it.

If the strictness analyser is run twice, this test also prevents
wrappers (which are INLINEd) from being re-done.  (You can end up with
several liked-named Ids bouncing around at the same time---absolute
mischief.)

Notice that we refrain from w/w'ing an INLINE function even if it is
in a recursive group.  It might not be the loop breaker.  (We could
test for loop-breaker-hood, but I'm not sure that ever matters.)

### Note: Worker-wrapper for INLINABLE functions

 InlineStable, Template = g val 

### Note: Zapping DmdEnv after Demand Analyzer

In the worker-wrapper pass we zap the DmdEnv.  Why?
 (a) it is never used again
 (b) it wastes space
 (c) it becomes incorrect as things are cloned, because
     we don't push the substitution into it

Why here?
 * Because we don’t want to do it in the Demand Analyzer, as we never know
   there when we are doing the last pass.
 * We want them to be still there at the end of DmdAnal, so that
   -ddump-str-anal contains them.
 * We don’t want a second pass just for that.
 * WorkWrap looks at all bindings anyway.

### Note: Final Demand Analyser run

### Note: Zapping Used Once info in WorkWrap

In the worker-wrapper pass we zap the used once info in demands and in
strictness signatures.

Why?
 * The simplifier may happen to transform code in a way that invalidates the
   data (see #11731 for an example).
 * It is not used in later passes, up to code generation.

So as the data is useless and possibly wrong, we want to remove it. The most
convenient place to do that is the worker wrapper phase, as it runs after every
run of the demand analyser besides the very last one (which is the one where we
want to _keep_ the info for the code generator).

### Note: Zapping DmdEnv after Demand Analyzer

### Note: Worker-wrapper for INLINABLE functions

                        `setInlinePragma` work_prag

### Note: Worker-wrapper for INLINABLE functions

                        `setIdStrictness` mkClosedStrictSig work_demands work_res_info
                                -- Even though we may not be at top level,
                                -- it's ok to give it an empty DmdEnv

                        `setIdDemandInfo` worker_demand

                        `setIdArity` work_arity
                                -- Set the arity so that the Core Lint check that the
                                -- arity is consistent with the demand type goes
                                -- through
                        `asJoinId_maybe` work_join_arity

            work_arity = length work_demands

### Note: Demand on the Worker

### Note: Wrapper activation

            wrap_id   = fn_id `setIdUnfolding`  mkWwInlineRule wrap_rhs arity
                              `setInlinePragma` wrap_prag
                              `setIdOccInfo`    noOccInfo
                                -- Zap any loop-breaker-ness, to avoid bleating from Lint
                                -- about a loop breaker with an INLINE rule



        return $ [(work_id, work_rhs), (wrap_id, wrap_rhs)]
            -- Worker first, because wrapper mentions it

      Nothing -> return [(fn_id, rhs)]
  where
    mb_join_arity   = isJoinId_maybe fn_id
    rhs_fvs         = exprFreeVars rhs
    fun_ty          = idType fn_id
    inl_prag        = inlinePragInfo fn_info
    rule_match_info = inlinePragmaRuleMatchInfo inl_prag
    arity           = arityInfo fn_info
                    -- The arity is set by the simplifier using exprEtaExpandArity
                    -- So it may be more than the number of top-level-visible lambdas

### Note: Don't CPR join points

### Note: Demand on the worker


If the original function is called once, according to its demand info, then
so is the worker. This is important so that the occurrence analyser can
attach OneShot annotations to the worker’s lambda binders.


Example:

  -- Original function
  f [Demand=<L,1*C1(U)>] :: (a,a) -> a
  f = \p -> ...

  -- Wrapper
  f [Demand=<L,1*C1(U)>] :: a -> a -> a
  f = \p -> case p of (a,b) -> $wf a b

  -- Worker
  $wf [Demand=<L,1*C1(C1(U))>] :: Int -> Int
  $wf = \a b -> ...

We need to check whether the original function is called once, with
sufficiently many arguments. This is done using saturatedByOneShots, which
takes the arity of the original function (resp. the wrapper) and the demand on
the original function.

The demand on the worker is then calculated using mkWorkerDemand, and always of
the form [Demand=<L,1*(C1(...(C1(U))))>]

### Note: Do not split void functions

Consider this rather common form of binding:
        $j = \x:Void# -> ...no use of x...

Since x is not used it'll be marked as absent.  But there is no point
in w/w-ing because we'll simply add (\y:Void#), see WwLib.mkWorerArgs.

If x has a more interesting type (eg Int, or Int#), there *is* a point
in w/w so that we don't pass the argument at all.

### Note: Thunk splitting

Suppose x is used strictly (never mind whether it has the CPR
property).

      let
        x* = x-rhs
      in body

splitThunk transforms like this:

      let
        x* = case x-rhs of { I# a -> I# a }
      in body

Now simplifier will transform to

      case x-rhs of
        I# a -> let x* = I# a
                in body

which is what we want. Now suppose x-rhs is itself a case:

        x-rhs = case e of { T -> I# a; F -> I# b }

The join point will abstract over a, rather than over (which is
what would have happened before) which is fine.

Notice that x certainly has the CPR property now!

In fact, splitThunk uses the function argument w/w splitting
function, so that if x's demand is deeper (say U(U(L,L),L))
then the splitting will go deeper too.
