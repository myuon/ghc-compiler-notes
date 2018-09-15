[[src]](https://github.com/ghc/ghc/tree/master/compiler/coreSyn/CorePrep.hs)

(c) The University of Glasgow, 1994-2006


Core pass to saturate constructors and PrimOps


### Note: CorePrep Overview

The goal of this pass is to prepare for code generation.

1.  Saturate constructor and primop applications.

2.  Convert to A-normal form; that is, function arguments
    are always variables.

    * Use case for strict arguments:
        f E ==> case E of x -> f x
        (where f is strict)

    * Use let for non-trivial lazy arguments
        f E ==> let x = E in f x
        (were f is lazy and x is non-trivial)

3.  Similarly, convert any unboxed lets into cases.
    [I'm experimenting with leaving 'ok-for-speculation'
     rhss in let-form right up to this point.]

4.  Ensure that *value* lambdas only occur as the RHS of a binding
    (The code generator can't deal with anything else.)
    Type lambdas are ok, however, because the code gen discards them.

5.  [Not any more; nuked Jun 2002] Do the seq/par munging.

6.  Clone all local Ids.
    This means that all such Ids are unique, rather than the
    weaker guarantee of no clashes which the simplifier provides.
    And that is what the code generator needs.

    We don't clone TyVars or CoVars. The code gen doesn't need that,
    and doing so would be tiresome because then we'd need
    to substitute in types and coercions.

7.  Give each dynamic CCall occurrence a fresh unique; this is
    rather like the cloning step above.

8.  Inject bindings for the "implicit" Ids:
        * Constructor wrappers
        * Constructor workers
    We want curried definitions for all of these in case they
    aren't inlined by some caller.

### Note: lazyId magic

10. Convert (LitInteger i t) into the core representation
    for the Integer i. Normally this uses mkInteger, but if
    we are using the integer-gmp implementation then there is a
    special case where we use the S# constructor for Integers that
    are in the range of Int.

11. Uphold tick consistency while doing this: We move ticks out of
    (non-type) applications where we can, and make sure that we
    annotate according to scoping rules when floating.

### Note: Drop unfoldings and rules

This is all done modulo type applications and abstractions, so that
when type erasure is done for conversion to STG, we don't end up with
any trivial or useless bindings.

### Note: CorePrep invariants

Here is the syntax of the Core produced by CorePrep:

    Trivial expressions
       arg ::= lit |  var
              | arg ty  |  /\a. arg
              | truv co  |  /\c. arg  |  arg |> co

    Applications
       app ::= lit  |  var  |  app arg  |  app ty  | app co | app |> co

    Expressions
       body ::= app
              | let(rec) x = rhs in body     -- Boxed only
              | case body of pat -> body
              | /\a. body | /\c. body
              | body |> co

    Right hand sides (only place where value lambdas can occur)
       rhs ::= /\a.rhs  |  \x.rhs  |  body

We define a synonym for each of these non-terminals.  Functions
with the corresponding name produce a result in that syntax.


# Top level stuff


### Note: Floating out of top level bindings

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

### Note: CafInfo and floating

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

   So, gruesomely, we must set the NoCafRefs flag on the sat bindings,
   *and* substitute the modified 'sat' into the old RHS.

   It should be the case that 'sat' is itself [NoCafRefs] (a value, no
   cafs) else the original top-level binding would not itself have been
   marked [NoCafRefs].  The DEBUG check in CoreToStg for
   consistentCafInfo will find this.

This is all very gruesome and horrible. It would be better to figure
out CafInfo later, after CorePrep.  We'll do that in due course.
Meanwhile this horrible hack works.

### Note: Join points and floating

Join points can float out of other join points but not out of value bindings:

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

### Note: Data constructor workers

Create any necessary "implicit" bindings for data con workers.  We
create the rather strange (non-recursive!) binding

        $wC = \x y -> $wC x y

i.e. a curried constructor that allocates.  This means that we can
treat the worker for a constructor like any other function in the rest
of the compiler.  The point here is that CoreToStg will generate a
StgConApp for the RHS, rather than a call to the worker (which would
give a loop).  As Lennart says: the ice is thin here, but it works.

Hmm.  Should we create bindings for dictionary constructors?  They are
always fully applied, and the bindings are just there to support
partial applications. But it's easier to let them through.

### Note: Dead code in CorePrep

Imagine that we got an input program like this (see Trac #4962):

  f :: Show b => Int -> (Int, b -> Maybe Int -> Int)
  f x = (g True (Just x) + g () (Just x), g)
    where
      g :: Show a => a -> Maybe Int -> Int
      g _ Nothing = x
      g y (Just z) = if z > 100 then g y (Just (z + length (show y))) else g y unknown

After specialisation and SpecConstr, we would get something like this:

  f :: Show b => Int -> (Int, b -> Maybe Int -> Int)
  f x = (g$Bool_True_Just x + g$Unit_Unit_Just x, g)
    where
      {-# RULES g $dBool = g$Bool
                g $dUnit = g$Unit #

### Note: Silly extra arguments

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


### Note: Arity and join points


Up to now, we've allowed a join point to have an arity greater than its join
arity (minus type arguments), since this is what's useful for eta expansion.
However, for code gen purposes, its arity must be exactly the number of value
arguments it will be called with, and it must have exactly that many value
lambdas. Hence if there are extra lambdas we must let-bind the body of the RHS:

  join j x y z = \w -> ... in ...
    =>
  join j x y z = (let f = \w -> ... in f) in ...

### Note: Silly extra arguments

### Note: runRW arg

If we got, say
   runRW# (case bot of {})
which happened in Trac #11291, we do /not/ want to turn it into
   (case bot of {}) realWorldPrimId#
because that gives a panic in CoreToStg.myCollectArgs, which expects
only variables in function position.  But if we are sure to make
runRW# strict (which we do in MkId), this can't happen


### Note: ANF-ising literal string arguments


Consider a program like,

    data Foo = Foo Addr#

    foo = Foo "turtle"#

When we go to ANFise this we might think that we want to float the string
literal like we do any other non-trivial argument. This would look like,

    foo = u\ [] case "turtle"# of s { __DEFAULT__ -> Foo s }

However, this 1) isn't necessary since strings are in a sense "trivial"; and 2)
wreaks havoc on the CAF annotations that we produce here since we the result
above is caffy since it is updateable. Ideally at some point in the future we
would like to just float the literal to the top level as suggested in #11312,

    s = "turtle"#
    foo = Foo s

However, until then we simply add a special case excluding literals from the
floating done by cpeArg.


### Note: Floating unlifted arguments

Consider    C (let v* = expensive in v)

where the "*" indicates "will be demanded".  Usually v will have been
inlined by now, but let's suppose it hasn't (see Trac #2756).  Then we
do *not* want to get

     let v* = expensive in C v

because that has different strictness.  Hence the use of 'allLazy'.
(NB: the let v* turns into a FloatCase, in mkLocalNonRec.)


------------------------------------------------------------------------------
-- Building the saturated syntax
-- ---------------------------------------------------------------------------

maybeSaturate deals with saturating primops and constructors
The type is the type of the entire application


### Note: dataToTag magic

Horrid: we must ensure that the arg of data2TagOp is evaluated
  (data2tag x) -->  (case x of y -> data2tag y)
(yuk yuk) take into account the lambdas we've now introduced

How might it not be evaluated?  Well, we might have floated it out
of the scope of a `seq`, or dropped the `seq` altogether.

# Simple CoreSyn operations



-- -----------------------------------------------------------------------------
--      Eta reduction
-- -----------------------------------------------------------------------------

### Note: Eta expansion

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

### Note: Eta expansion and the CorePrep invariants

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



-- -----------------------------------------------------------------------------
--      Eta reduction
-- -----------------------------------------------------------------------------

Why try eta reduction?  Hasn't the simplifier already done eta?
But the simplifier only eta reduces if that leaves something
trivial (like f, or f Int).  But for deLam it would be enough to
get to a partial application:
        case x of { p -> \xs. map f xs }
    ==> case x of { p -> map f }


# Floats


### Note: Pin demand info on floats

We pin demand info on floated lets, so that we can see the one-shot thunks.


# Cloning
