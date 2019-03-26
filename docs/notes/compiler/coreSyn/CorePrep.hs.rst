`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/coreSyn/CorePrep.hs>`_

compiler/coreSyn/CorePrep.hs
============================


Note [CorePrep invariants]
~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/coreSyn/CorePrep.hs#L140>`__

Here is the syntax of the Core produced by CorePrep:

::

    Trivial expressions
       arg ::= lit |  var
              | arg ty  |  /\a. arg
              | truv co  |  /\c. arg  |  arg |> co

::

    Applications
       app ::= lit  |  var  |  app arg  |  app ty  | app co | app |> co

::

    Expressions
       body ::= app
              | let(rec) x = rhs in body     -- Boxed only
              | case body of pat -> body
              | /\a. body | /\c. body
              | body |> co

::

    Right hand sides (only place where value lambdas can occur)
       rhs ::= /\a.rhs  |  \x.rhs  |  body

We define a synonym for each of these non-terminals.  Functions
with the corresponding name produce a result in that syntax.



Note [Floating out of top level bindings]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/coreSyn/CorePrep.hs#L253>`__

NB: we do need to float out of top-level bindings
Consider        x = length [True,False]
We want to get
                s1 = False : []
                s2 = True  : s1
                x  = length s2

We return a *list* of bindings, because we may start with
        x* = f (g y)
where x is demanded, in which case we want to finish with
        a = g y
        x* = f a
And then x will actually end up case-bound



Note [CafInfo and floating]
~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/coreSyn/CorePrep.hs#L269>`__

What happens when we try to float bindings to the top level?  At this
point all the CafInfo is supposed to be correct, and we must make certain
that is true of the new top-level bindings.  There are two cases
to consider

a) The top-level binding is marked asCafRefs.  In that case we are
   basically fine.  The floated bindings had better all be lazy lets,
   so they can float to top level, but they'll all have HasCafRefs
   (the default) which is safe.

b) The top-level binding is marked NoCafRefs.  This really happens
   Example.  CoreTidy produces
      $fApplicativeSTM [NoCafRefs] = D:Alternative retry# ...blah...
   Now CorePrep has to eta-expand to
      $fApplicativeSTM = let sat = \xy. retry x y
                         in D:Alternative sat ...blah...
   So what we *want* is
      sat [NoCafRefs] = \xy. retry x y
      $fApplicativeSTM [NoCafRefs] = D:Alternative sat ...blah...

::

   So, gruesomely, we must set the NoCafRefs flag on the sat bindings,
   *and* substitute the modified 'sat' into the old RHS.

::

   It should be the case that 'sat' is itself [NoCafRefs] (a value, no
   cafs) else the original top-level binding would not itself have been
   marked [NoCafRefs].  The DEBUG check in CoreToStg for
   consistentCafInfo will find this.

This is all very gruesome and horrible. It would be better to figure
out CafInfo later, after CorePrep.  We'll do that in due course.
Meanwhile this horrible hack works.



Note [Join points and floating]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/coreSyn/CorePrep.hs#L303>`__

Join points can float out of other join points but not out of value bindings:

::

  let z =
    let  w = ... in -- can float
    join k = ... in -- can't float
    ... jump k ...
  join j x1 ... xn =
    let  y = ... in -- can float (but don't want to)
    join h = ... in -- can float (but not much point)
    ... jump h ...
  in ...

Here, the jump to h remains valid if h is floated outward, but the jump to k
does not.

We don't float *out* of join points. It would only be safe to float out of
nullary join points (or ones where the arguments are all either type arguments
or dead binders). Nullary join points aren't ever recursive, so they're always
effectively one-shot functions, which we don't float out of. We *could* float
join points from nullary join points, but there's no clear benefit at this
stage.



Note [Data constructor workers]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/coreSyn/CorePrep.hs#L327>`__

Create any necessary "implicit" bindings for data con workers.  We
create the rather strange (non-recursive!) binding

::

        $wC = \x y -> $wC x y

i.e. a curried constructor that allocates.  This means that we can
treat the worker for a constructor like any other function in the rest
of the compiler.  The point here is that CoreToStg will generate a
StgConApp for the RHS, rather than a call to the worker (which would
give a loop).  As Lennart says: the ice is thin here, but it works.

Hmm.  Should we create bindings for dictionary constructors?  They are
always fully applied, and the bindings are just there to support
partial applications. But it's easier to let them through.



Note [Dead code in CorePrep]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/coreSyn/CorePrep.hs#L345>`__

Imagine that we got an input program like this (see #4962):

::

  f :: Show b => Int -> (Int, b -> Maybe Int -> Int)
  f x = (g True (Just x) + g () (Just x), g)
    where
      g :: Show a => a -> Maybe Int -> Int
      g _ Nothing = x
      g y (Just z) = if z > 100 then g y (Just (z + length (show y))) else g y unknown

After specialisation and SpecConstr, we would get something like this:

::

  f :: Show b => Int -> (Int, b -> Maybe Int -> Int)
  f x = (g$Bool_True_Just x + g$Unit_Unit_Just x, g)
    where
      {-# RULES g $dBool = g$Bool
                g $dUnit = g$Unit #-}
      g = ...
      {-# RULES forall x. g$Bool True (Just x) = g$Bool_True_Just x #-}
      g$Bool = ...
      {-# RULES forall x. g$Unit () (Just x) = g$Unit_Unit_Just x #-}
      g$Unit = ...
      g$Bool_True_Just = ...
      g$Unit_Unit_Just = ...

Note that the g$Bool and g$Unit functions are actually dead code: they
are only kept alive by the occurrence analyser because they are
referred to by the rules of g, which is being kept alive by the fact
that it is used (unspecialised) in the returned pair.

However, at the CorePrep stage there is no way that the rules for g
will ever fire, and it really seems like a shame to produce an output
program that goes to the trouble of allocating a closure for the
unreachable g$Bool and g$Unit functions.

The way we fix this is to:
 * In cloneBndr, drop all unfoldings/rules

 * In deFloatTop, run a simple dead code analyser on each top-level
   RHS to drop the dead local bindings. For that call to OccAnal, we
   disable the binder swap, else the occurrence analyser sometimes
   introduces new let bindings for cased binders, which lead to the bug
   in #5433.

The reason we don't just OccAnal the whole output of CorePrep is that
the tidier ensures that all top-level binders are GlobalIds, so they
don't show up in the free variables any longer. So if you run the
occurrence analyser on the output of CoreTidy (or later) you e.g. turn
this program:

::

  Rec {
  f = ... f ...
  }

Into this one:

::

  f = ... f ...

(Since f is not considered to be free in its own RHS.)



Note [Silly extra arguments]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/coreSyn/CorePrep.hs#L545>`__

Suppose we had this
        f{arity=1} = \x\y. e
We *must* match the arity on the Id, so we have to generate
        f' = \x\y. e
        f  = \x. f' x

It's a bizarre case: why is the arity on the Id wrong?  Reason
(in the days of __inline_me__):
        f{arity=0} = __inline_me__ (let v = expensive in \xy. e)
When InlineMe notes go away this won't happen any more.  But
it seems good for CorePrep to be robust.
-------------



Note [Arity and join points]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/coreSyn/CorePrep.hs#L582>`__

Up to now, we've allowed a join point to have an arity greater than its join
arity (minus type arguments), since this is what's useful for eta expansion.
However, for code gen purposes, its arity must be exactly the number of value
arguments it will be called with, and it must have exactly that many value
lambdas. Hence if there are extra lambdas we must let-bind the body of the RHS:

::

  join j x y z = \w -> ... in ...
    =>
  join j x y z = (let f = \w -> ... in f) in ...

This is also what happens with Note [Silly extra arguments]. Note that it's okay
for us to mess with the arity because a join point is never exported.
---------------------------------------------------------------------------
             CpeRhs: produces a result satisfying CpeRhs
---------------------------------------------------------------------------



Note [runRW arg]
~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/coreSyn/CorePrep.hs#L789>`__

If we got, say
   runRW# (case bot of {})
which happened in #11291, we do /not/ want to turn it into
   (case bot of {}) realWorldPrimId#
because that gives a panic in CoreToStg.myCollectArgs, which expects
only variables in function position.  But if we are sure to make
runRW# strict (which we do in MkId), this can't happen



Note [runRW magic]
~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/coreSyn/CorePrep.hs#L948>`__

Some definitions, for instance @runST@, must have careful control over float out
of the bindings in their body. Consider this use of @runST@,

::

    f x = runST ( \ s -> let (a, s')  = newArray# 100 [] s
                             (_, s'') = fill_in_array_or_something a x s'
                         in freezeArray# a s'' )

If we inline @runST@, we'll get:

::

    f x = let (a, s')  = newArray# 100 [] realWorld#{-NB-}
              (_, s'') = fill_in_array_or_something a x s'
          in freezeArray# a s''

And now if we allow the @newArray#@ binding to float out to become a CAF,
we end up with a result that is totally and utterly wrong:

::

    f = let (a, s')  = newArray# 100 [] realWorld#{-NB-} -- YIKES!!!
        in \ x ->
            let (_, s'') = fill_in_array_or_something a x s'
            in freezeArray# a s''

All calls to @f@ will share a {\em single} array! Clearly this is nonsense and
must be prevented.

This is what @runRW#@ gives us: by being inlined extremely late in the
optimization (right before lowering to STG, in CorePrep), we can ensure that
no further floating will occur. This allows us to safely inline things like
@runST@, which are otherwise needlessly expensive (see #10678 and #5916).

'runRW' is defined (for historical reasons) in GHC.Magic, with a NOINLINE
pragma.  It is levity-polymorphic.

::

    runRW# :: forall (r1 :: RuntimeRep). (o :: TYPE r)
           => (State# RealWorld -> (# State# RealWorld, o #))
                              -> (# State# RealWorld, o #)

It needs no special treatment in GHC except this special inlining here
in CorePrep (and in ByteCodeGen).

-- ---------------------------------------------------------------------------
--      CpeArg: produces a result satisfying CpeArg
-- ---------------------------------------------------------------------------



Note [ANF-ising literal string arguments]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/coreSyn/CorePrep.hs#L993>`__

Consider a program like,

::

    data Foo = Foo Addr#

::

    foo = Foo "turtle"#

When we go to ANFise this we might think that we want to float the string
literal like we do any other non-trivial argument. This would look like,

::

    foo = u\ [] case "turtle"# of s { __DEFAULT__ -> Foo s }

However, this 1) isn't necessary since strings are in a sense "trivial"; and 2)
wreaks havoc on the CAF annotations that we produce here since we the result
above is caffy since it is updateable. Ideally at some point in the future we
would like to just float the literal to the top level as suggested in #11312,

::

    s = "turtle"#
    foo = Foo s

However, until then we simply add a special case excluding literals from the
floating done by cpeArg.



Note [Floating unlifted arguments]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/coreSyn/CorePrep.hs#L1049>`__

Consider    C (let v* = expensive in v)

where the "*" indicates "will be demanded".  Usually v will have been
inlined by now, but let's suppose it hasn't (see #2756).  Then we
do *not* want to get

::

     let v* = expensive in C v

because that has different strictness.  Hence the use of 'allLazy'.
(NB: the let v* turns into a FloatCase, in mkLocalNonRec.)


------------------------------------------------------------------------------
-- Building the saturated syntax
-- ---------------------------------------------------------------------------

maybeSaturate deals with saturating primops and constructors
The type is the type of the entire application



Note [Eta expansion]
~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/coreSyn/CorePrep.hs#L1096>`__

Eta expand to match the arity claimed by the binder Remember,
CorePrep must not change arity

Eta expansion might not have happened already, because it is done by
the simplifier only when there at least one lambda already.

NB1:we could refrain when the RHS is trivial (which can happen
    for exported things).  This would reduce the amount of code
    generated (a little) and make things a little words for
    code compiled without -O.  The case in point is data constructor
    wrappers.

NB2: we have to be careful that the result of etaExpand doesn't
   invalidate any of the assumptions that CorePrep is attempting
   to establish.  One possible cause is eta expanding inside of
   an SCC note - we're now careful in etaExpand to make sure the
   SCC is pushed inside any new lambdas that are generated.



Note [Eta expansion and the CorePrep invariants]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/coreSyn/CorePrep.hs#L1116>`__

It turns out to be much much easier to do eta expansion
*after* the main CorePrep stuff.  But that places constraints
on the eta expander: given a CpeRhs, it must return a CpeRhs.

For example here is what we do not want:
                f = /\a -> g (h 3)      -- h has arity 2
After ANFing we get
                f = /\a -> let s = h 3 in g s
and now we do NOT want eta expansion to give
                f = /\a -> \ y -> (let s = h 3 in g s) y

Instead CoreArity.etaExpand gives
                f = /\a -> \y -> let s = h 3 in g s y



Note [Pin demand info on floats]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/coreSyn/CorePrep.hs#L1199>`__

We pin demand info on floated lets, so that we can see the one-shot thunks.



Note [Inlining in CorePrep]
~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/coreSyn/CorePrep.hs#L1400>`__

There is a subtle but important invariant that must be upheld in the output
of CorePrep: there are no "trivial" updatable thunks.  Thus, this Core
is impermissible:

::

     let x :: ()
         x = y

(where y is a reference to a GLOBAL variable).  Thunks like this are silly:
they can always be profitably replaced by inlining x with y. Consequently,
the code generator/runtime does not bother implementing this properly
(specifically, there is no implementation of stg_ap_0_upd_info, which is the
stack frame that would be used to update this thunk.  The "0" means it has
zero free variables.)

In general, the inliner is good at eliminating these let-bindings.  However,
there is one case where these trivial updatable thunks can arise: when
we are optimizing away 'lazy' (see Note [lazyId magic], and also
'cpeRhsE'.)  Then, we could have started with:

::

     let x :: ()
         x = lazy @ () y

which is a perfectly fine, non-trivial thunk, but then CorePrep will
drop 'lazy', giving us 'x = y' which is trivial and impermissible.
The solution is CorePrep to have a miniature inlining pass which deals
with cases like this.  We can then drop the let-binding altogether.

Why does the removal of 'lazy' have to occur in CorePrep?
The gory details are in Note [lazyId magic] in MkId, but the
main reason is that lazy must appear in unfoldings (optimizer
output) and it must prevent call-by-value for catch# (which
is implemented by CorePrep.)

An alternate strategy for solving this problem is to have the
inliner treat 'lazy e' as a trivial expression if 'e' is trivial.
We decided not to adopt this solution to keep the definition
of 'exprIsTrivial' simple.

There is ONE caveat however: for top-level bindings we have
to preserve the binding so that we float the (hacky) non-recursive
binding for data constructors; see Note [Data constructor workers].



Note [CorePrep inlines trivial CoreExpr not Id]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/coreSyn/CorePrep.hs#L1444>`__

Why does cpe_env need to be an IdEnv CoreExpr, as opposed to an
IdEnv Id?  Naively, we might conjecture that trivial updatable thunks
as per Note [Inlining in CorePrep] always have the form
'lazy @ SomeType gbl_id'.  But this is not true: the following is
perfectly reasonable Core:

::

     let x :: ()
         x = lazy @ (forall a. a) y @ Bool

When we inline 'x' after eliminating 'lazy', we need to replace
occurrences of 'x' with 'y @ bool', not just 'y'.  Situations like
this can easily arise with higher-rank types; thus, cpe_env must
map to CoreExprs, not Ids.



Note [Drop unfoldings and rules]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/coreSyn/CorePrep.hs#L1600>`__

We want to drop the unfolding/rules on every Id:

  - We are now past interface-file generation, and in the
    codegen pipeline, so we really don't need full unfoldings/rules

  - The unfolding/rule may be keeping stuff alive that we'd like
    to discard.  See  Note [Dead code in CorePrep]

  - Getting rid of unnecessary unfoldings reduces heap usage

  - We are changing uniques, so if we didn't discard unfoldings/rules
    we'd have to substitute in them

HOWEVER, we want to preserve evaluated-ness;
see Note [Preserve evaluatedness] in CoreTidy.
----------------------------------------------------------------------------
 Cloning ccall Ids; each must have a unique name,
 to give the code generator a handle to hang it on
 ---------------------------------------------------------------------------

