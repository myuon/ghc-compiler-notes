`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/simplStg/StgLiftLams/Analysis.hs>`_

compiler/simplStg/StgLiftLams/Analysis.hs
=========================================


Note [When to lift]
~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/simplStg/StgLiftLams/Analysis.hs#L40>`__

::

 The analysis proceeds in two steps:

..

   1. It tags the syntax tree with analysis information in the form of
      'BinderInfo' at each binder and 'Skeleton's at each let-binding
      by 'tagSkeletonTopBind' and friends.
   2. The resulting syntax tree is treated by the "StgLiftLams.Transformation"
      module, calling out to 'goodToLift' to decide if a binding is worthwhile
      to lift.
      'goodToLift' consults argument occurrence information in 'BinderInfo'
      and estimates 'closureGrowth', for which it needs the 'Skeleton'.

 So the annotations from 'tagSkeletonTopBind' ultimately fuel 'goodToLift',
 which employs a number of heuristics to identify and exclude lambda lifting
 opportunities deemed non-beneficial:

  [Top-level bindings] can't be lifted.
  [Thunks] and data constructors shouldn't be lifted in order not to destroy
    sharing.
  [Argument occurrences] #arg_occs# of binders prohibit them to be lifted.
    Doing the lift would re-introduce the very allocation at call sites that
    we tried to get rid off in the first place. We capture analysis
    information in 'BinderInfo'. Note that we also consider a nullary
    application as argument occurrence, because it would turn into an n-ary
    partial application created by a generic apply function. This occurs in
    CPS-heavy code like the CS benchmark.
  [Join points] should not be lifted, simply because there's no reduction in
    allocation to be had.
  [Abstracting over join points] destroys join points, because they end up as
    arguments to the lifted function.
  [Abstracting over known local functions] turns a known call into an unknown
    call (e.g. some @stg_ap_*@), which is generally slower. Can be turned off
    with @-fstg-lift-lams-known@.
  [Calling convention] Don't lift when the resulting function would have a
    higher arity than available argument registers for the calling convention.
    Can be influenced with @-fstg-lift-(non)rec-args(-any)@.
  [Closure growth] introduced when former free variables have to be available
    at call sites may actually lead to an increase in overall allocations
  resulting from a lift. Estimating closure growth is described in
  "StgLiftLams.Analysis#clogro" and is what most of this module is ultimately
  concerned with.

 There's a <https://gitlab.haskell.org/ghc/ghc/wikis/late-lam-lift wiki page> with
 some more background and history.



Note [Estimating closure growth]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/simplStg/StgLiftLams/Analysis.hs#L87>`__

::

 We estimate closure growth by abstracting the syntax tree into a 'Skeleton',
 capturing only syntactic details relevant to 'closureGrowth', such as

..

   * 'ClosureSk', representing closure allocation.
   * 'RhsSk', representing a RHS of a binding and how many times it's called
     by an appropriate 'DmdShell'.
   * 'AltSk', 'BothSk' and 'NilSk' for choice, sequence and empty element.

::

 This abstraction is mostly so that the main analysis function 'closureGrowth'
 can stay simple and focused. Also, skeletons tend to be much smaller than
 the syntax tree they abstract, so it makes sense to construct them once and
 and operate on them instead of the actual syntax tree.

..

::

 A more detailed treatment of computing closure growth, including examples,
 can be found in the paper referenced from the
 <https://gitlab.haskell.org/ghc/ghc/wikis/late-lam-lift wiki page>.

..

