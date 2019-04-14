`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/stranal/DmdAnal.hs>`_

compiler/stranal/DmdAnal.hs
===========================


Note [Stamp out space leaks in demand analysis]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/stranal/DmdAnal.hs#L86>`__

The demand analysis pass outputs a new copy of the Core program in
which binders have been annotated with demand and strictness
information. It's tiresome to ensure that this information is fully
evaluated everywhere that we produce it, so we just run a single
seqBinds over the output before returning it, to ensure that there are
no references holding on to the input Core program.

This makes a ~30% reduction in peak memory usage when compiling
DynFlags (cf #9675 and #13426).

This is particularly important when we are doing late demand analysis,
since we don't do a seqBinds at any point thereafter. Hence code
generation would hold on to an extra copy of the Core program, via
unforced thunks in demand or strictness information; and it is the
most memory-intensive part of the compilation process, so this added
seqBinds makes a big difference in peak memory usage.



Note [Ensure demand is strict]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/stranal/DmdAnal.hs#L114>`__

It's important not to analyse e with a lazy demand because
a) When we encounter   case s of (a,b) ->
        we demand s with U(d1d2)... but if the overall demand is lazy
        that is wrong, and we'd need to reduce the demand on s,
        which is inconvenient
b) More important, consider
        f (let x = R in x+x), where f is lazy
   We still want to mark x as demanded, because it will be when we
   enter the let.  If we analyse f's arg with a Lazy demand, we'll
   just mark x as Lazy
c) The application rule wouldn't be right either
   Evaluating (f x) in a L demand does *not* cause
   evaluation of f in a C(L) demand!

If e is complicated enough to become a thunk, its contents will be evaluated
at most once, so oneify it.



Note [IO hack in the demand analyser]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/stranal/DmdAnal.hs#L361>`__

There's a hack here for I/O operations.  Consider

::

     case foo x s of { (# s', r #) -> y }

..

Is this strict in 'y'? Often not! If foo x s performs some observable action
(including raising an exception with raiseIO#, modifying a mutable variable, or
even ending the program normally), then we must not force 'y' (which may fail
to terminate) until we have performed foo x s.

Hackish solution: spot the IO-like situation and add a virtual branch,
as if we had
     case foo x s of
        (# s, r #) -> y
        other      -> return ()
So the 'y' isn't necessarily going to be evaluated

A more complete example (#148, #1592) where this shows up is:
     do { let len = <expensive> ;
        ; when (...) (exitWith ExitSuccess)
        ; print len }

However, consider
  f x s = case getMaskingState# s of
            (# s, r #) ->
          case x of I# x2 -> ...

Here it is terribly sad to make 'f' lazy in 's'.  After all,
getMaskingState# is not going to diverge or throw an exception!  This
situation actually arises in GHC.IO.Handle.Internals.wantReadableHandle
(on an MVar not an Int), and made a material difference.

So if the scrutinee is a primop call, we *don't* apply the
state hack:
  - If it is a simple, terminating one like getMaskingState,
    applying the hack is over-conservative.
  - If the primop is raise# then it returns bottom, so
    the case alternatives are already discarded.
  - If the primop can raise a non-IO exception, like
    divide by zero or seg-fault (eg writing an array
    out of bounds) then we don't mind evaluating 'x' first.



Note [Demand on the scrutinee of a product case]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/stranal/DmdAnal.hs#L404>`__

When figuring out the demand on the scrutinee of a product case,
we use the demands of the case alternative, i.e. id_dmds.
But note that these include the demand on the case binder;
see Note [Demand on case-alternative binders] in Demand.hs.
This is crucial. Example:
   f x = case x of y { (a,b) -> k y a }
If we just take scrut_demand = U(L,A), then we won't pass x to the
worker, so the worker will rebuild
     x = (a, absent-error)
and that'll crash.



Note [Aggregated demand for cardinality]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/stranal/DmdAnal.hs#L417>`__

We use different strategies for strictness and usage/cardinality to
"unleash" demands captured on free variables by bindings. Let us
consider the example:

f1 y = let {-# NOINLINE h #-}
           h = y
       in  (h, h)

We are interested in obtaining cardinality demand U1 on |y|, as it is
used only in a thunk, and, therefore, is not going to be updated any
more. Therefore, the demand on |y|, captured and unleashed by usage of
|h| is U1. However, if we unleash this demand every time |h| is used,
and then sum up the effects, the ultimate demand on |y| will be U1 +
U1 = U. In order to avoid it, we *first* collect the aggregate demand
on |h| in the body of let-expression, and only then apply the demand
transformer:

transf[x](U) = {y |-> U1}

so the resulting demand on |y| is U1.

The situation is, however, different for strictness, where this
aggregating approach exhibits worse results because of the nature of
|both| operation for strictness. Consider the example:

f y c =
  let h x = y |seq| x
   in case of
        True  -> h True
        False -> y

It is clear that |f| is strict in |y|, however, the suggested analysis
will infer from the body of |let| that |h| is used lazily (as it is
used in one branch only), therefore lazy demand will be put on its
free variable |y|. Conversely, if the demand on |h| is unleashed right
on the spot, we will get the desired result, namely, that |f| is
strict in |y|.



Note [Safe abortion in the fixed-point iteration]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/stranal/DmdAnal.hs#L568>`__

Fixed-point iteration may fail to terminate. But we cannot simply give up and
return the environment and code unchanged! We still need to do one additional
round, for two reasons:

 * To get information on used free variables (both lazy and strict!)
   (see Note [Lazy and unleashable free variables])
 * To ensure that all expressions have been traversed at least once, and any left-over
   strictness annotations have been updated.

This final iteration does not add the variables to the strictness signature
environment, which effectively assigns them 'nopSig' (see "getStrictness")


Trivial RHS
See Note [Demand analysis for trivial right-hand sides]



Note [Demand analysis for join points]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/stranal/DmdAnal.hs#L700>`__

Consider
   g :: (Int,Int) -> Int
   g (p,q) = p+q

::

   f :: T -> Int -> Int
   f x p = g (join j y = (p,y)
              in case x of
                   A -> j 3
                   B -> j 4
                   C -> (p,7))

..

If j was a vanilla function definition, we'd analyse its body with
evalDmd, and think that it was lazy in p.  But for join points we can
do better!  We know that j's body will (if called at all) be evaluated
with the demand that consumes the entire join-binding, in this case
the argument demand from g.  Whizzo!  g evaluates both components of
its argument pair, so p will certainly be evaluated if j is called.

For f to be strict in p, we need /all/ paths to evaluate p; in this
case the C branch does so too, so we are fine.  So, as usual, we need
to transport demands on free variables to the call site(s).  Compare
Note [Lazy and unleashable free variables].

The implementation is easy.  When analysing a join point, we can
analyse its body with the demand from the entire join-binding (written
let_dmd here).

Another win for join points!  #13543.



Note [Demand analysis for trivial right-hand sides]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/stranal/DmdAnal.hs#L731>`__

Consider
        foo = plusInt |> co
where plusInt is an arity-2 function with known strictness.  Clearly
we want plusInt's strictness to propagate to foo!  But because it has
no manifest lambdas, it won't do so automatically, and indeed 'co' might
have type (Int->Int->Int) ~ T, so we *can't* eta-expand.  So we have a
special case for right-hand sides that are "trivial", namely variables,
casts, type applications, and the like.

Note that this can mean that 'foo' has an arity that is smaller than that
indicated by its demand info.  e.g. if co :: (Int->Int->Int) ~ T, then
foo's arity will be zero (see Note [exprArity invariant] in CoreArity),
but its demand signature will be that of plusInt. A small example is the
test case of #8963.



Note [Product demands for function body]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/stranal/DmdAnal.hs#L749>`__

This example comes from shootout/binary_trees:

    Main.check' = \ b z ds. case z of z' { I# ip ->
                                case ds_d13s of
                                  Main.Nil -> z'
                                  Main.Node s14k s14l s14m ->
                                    Main.check' (not b)
                                      (Main.check' b
                                         (case b {
                                            False -> I# (-# s14h s14k);
                                            True  -> I# (+# s14h s14k)
                                          })
                                         s14l)
                                     s14m   }   }   }

Here we *really* want to unbox z, even though it appears to be used boxed in
the Nil case.  Partly the Nil case is not a hot path.  But more specifically,
the whole function gets the CPR property if we do.

So for the demand on the body of a RHS we use a product demand if it's
a product type.



Note [Do not strictify the argument dictionaries of a dfun]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/stranal/DmdAnal.hs#L819>`__

The typechecker can tie recursive knots involving dfuns, so we do the
conservative thing and refrain from strictifying a dfun's argument
dictionaries.



Note [CPR for sum types]
~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/stranal/DmdAnal.hs#L880>`__

At the moment we do not do CPR for let-bindings that
   * non-top level
   * bind a sum type
Reason: I found that in some benchmarks we were losing let-no-escapes,
which messed it all up.  Example
   let j = \x. ....
   in case y of
        True  -> j False
        False -> j True
If we w/w this we get
   let j' = \x. ....
   in case y of
        True  -> case j' False of { (# a #) -> Just a }
        False -> case j' True of { (# a #) -> Just a }
Notice that j' is not a let-no-escape any more.

However this means in turn that the *enclosing* function
may be CPR'd (via the returned Justs).  But in the case of
sums, there may be Nothing alternatives; and that messes
up the sum-type CPR.

Conclusion: only do this for products.  It's still not
guaranteed OK for products, but sums definitely lose sometimes.



Note [CPR for thunks]
~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/stranal/DmdAnal.hs#L906>`__

If the rhs is a thunk, we usually forget the CPR info, because
it is presumably shared (else it would have been inlined, and
so we'd lose sharing if w/w'd it into a function).  E.g.

        let r = case expensive of
                  (a,b) -> (b,a)
        in ...

If we marked r as having the CPR property, then we'd w/w into

        let $wr = \() -> case expensive of
                            (a,b) -> (# b, a #)
            r = case $wr () of
                  (# b,a #) -> (b,a)
        in ...

But now r is a thunk, which won't be inlined, so we are no further ahead.
But consider

::

        f x = let r = case expensive of (a,b) -> (b,a)
              in if foo r then r else (x,x)

..

Does f have the CPR property?  Well, no.

However, if the strictness analyser has figured out (in a previous
iteration) that it's strict, then we DON'T need to forget the CPR info.
Instead we can retain the CPR info and do the thunk-splitting transform
(see WorkWrap.splitThunk).

This made a big difference to PrelBase.modInt, which had something like
        modInt = \ x -> let r = ... -> I# v in
                        ...body strict in r...
r's RHS isn't a value yet; but modInt returns r in various branches, so
if r doesn't have the CPR property then neither does modInt
Another case I found in practice (in Complex.magnitude), looks like this:
                let k = if ... then I# a else I# b
                in ... body strict in k ....
(For this example, it doesn't matter whether k is returned as part of
the overall result; but it does matter that k's RHS has the CPR property.)
Left to itself, the simplifier will make a join point thus:
                let $j k = ...body strict in k...
                if ... then $j (I# a) else $j (I# b)
With thunk-splitting, we get instead
                let $j x = let k = I#x in ...body strict in k...
                in if ... then $j a else $j b
This is much better; there's a good chance the I# won't get allocated.

The difficulty with this is that we need the strictness type to
look at the body... but we now need the body to calculate the demand
on the variable, so we can decide whether its strictness type should
have a CPR in it or not.  Simple solution:
        a) use strictness info from the previous iteration
        b) make sure we do at least 2 iterations, by doing a second
           round for top-level non-recs.  Top level recs will get at
           least 2 iterations except for totally-bottom functions
           which aren't very interesting anyway.

NB: strictly_demanded is never true of a top-level Id, or of a recursive Id.



Note [Optimistic CPR in the "virgin" case]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/stranal/DmdAnal.hs#L967>`__

Demand and strictness info are initialized by top elements. However,
this prevents from inferring a CPR property in the first pass of the
analyser, so we keep an explicit flag ae_virgin in the AnalEnv
datatype.

We can't start with 'not-demanded' (i.e., top) because then consider
        f x = let
                  t = ... I# x
              in
              if ... then t else I# y else f x'

In the first iteration we'd have no demand info for x, so assume
not-demanded; then we'd get TopRes for f's CPR info.  Next iteration
we'd see that t was demanded, and so give it the CPR property, but by
now f has TopRes, so it will stay TopRes.  Instead, by checking the
ae_virgin flag at the first time round, we say 'yes t is demanded' the
first time.

However, this does mean that for non-recursive bindings we must
iterate twice to be sure of not getting over-optimistic CPR info,
in the case where t turns out to be not-demanded.  This is handled
by dmdAnalTopBind.



Note [NOINLINE and strictness]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/stranal/DmdAnal.hs#L993>`__

The strictness analyser used to have a HACK which ensured that NOINLNE
things were not strictness-analysed.  The reason was unsafePerformIO.
Left to itself, the strictness analyser would discover this strictness
for unsafePerformIO:
        unsafePerformIO:  C(U(AV))
But then consider this sub-expression
        unsafePerformIO (\s -> let r = f x in
                               case writeIORef v r s of (# s1, _ #) ->
                               (# s1, r #)
The strictness analyser will now find that r is sure to be eval'd,
and may then hoist it out.  This makes tests/lib/should_run/memo002
deadlock.

Solving this by making all NOINLINE things have no strictness info is overkill.
In particular, it's overkill for runST, which is perfectly respectable.
Consider
        f x = runST (return x)
This should be strict in x.

So the new plan is to define unsafePerformIO using the 'lazy' combinator:

::

        unsafePerformIO (IO m) = lazy (case m realWorld# of (# _, r #) -> r)

..

Remember, 'lazy' is a wired-in identity-function Id, of type a->a, which is
magically NON-STRICT, and is inlined after strictness analysis.  So
unsafePerformIO will look non-strict, and that's what we want.

Now we don't need the hack in the strictness analyser.  HOWEVER, this
decision does mean that even a NOINLINE function is not entirely
opaque: some aspect of its implementation leaks out, notably its
strictness.  For example, if you have a function implemented by an
error stub, but which has RULES, you may want it not to be eliminated
in favour of error!



Note [Lazy and unleashable free variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/stranal/DmdAnal.hs#L1029>`__

We put the strict and once-used FVs in the DmdType of the Id, so
that at its call sites we unleash demands on its strict fvs.
An example is 'roll' in imaginary/wheel-sieve2
Something like this:
        roll x = letrec
                     go y = if ... then roll (x-1) else x+1
                 in
                 go ms
We want to see that roll is strict in x, which is because
go is called.   So we put the DmdEnv for x in go's DmdType.

Another example:

        f :: Int -> Int -> Int
        f x y = let t = x+1
            h z = if z==0 then t else
                  if z==1 then x+1 else
                  x + h (z-1)
        in h y

Calling h does indeed evaluate x, but we can only see
that if we unleash a demand on x at the call site for t.

Incidentally, here's a place where lambda-lifting h would
lose the cigar --- we couldn't see the joint strictness in t/x

        ON THE OTHER HAND

We don't want to put *all* the fv's from the RHS into the
DmdType. Because

 * it makes the strictness signatures larger, and hence slows down fixpointing

and

 * it is useless information at the call site anyways:
   For lazy, used-many times fv's we will never get any better result than
   that, no matter how good the actual demand on the function at the call site
   is (unless it is always absent, but then the whole binder is useless).

Therefore we exclude lazy multiple-used fv's from the environment in the
DmdType.

But now the signature lies! (Missing variables are assumed to be absent.) To
make up for this, the code that analyses the binding keeps the demand on those
variable separate (usually called "lazy_fv") and adds it to the demand of the
whole binding later.

What if we decide _not_ to store a strictness signature for a binding at all, as
we do when aborting a fixed-point iteration? The we risk losing the information
that the strict variables are being used. In that case, we take all free variables
mentioned in the (unsound) strictness signature, conservatively approximate the
demand put on them (topDmd), and add that to the "lazy_fv" returned by "dmdFix".



Note [Lambda-bound unfoldings]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/stranal/DmdAnal.hs#L1086>`__

We allow a lambda-bound variable to carry an unfolding, a facility that is used
exclusively for join points; see Note [Case binders and join points].  If so,
we must be careful to demand-analyse the RHS of the unfolding!  Example
   \x. \y{=Just x}. <body>
Then if <body> uses 'y', then transitively it uses 'x', and we must not
forget that fact, otherwise we might make 'x' absent when it isn't.



Note [CPR in a product case alternative]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/stranal/DmdAnal.hs#L1259>`__

In a case alternative for a product type, we want to give some of the
binders the CPR property.  Specifically

 * The case binder; inside the alternative, the case binder always has
   the CPR property, meaning that a case on it will successfully cancel.
   Example:
        f True  x = case x of y { I# x' -> if x' ==# 3
                                           then y
                                           else I# 8 }
        f False x = I# 3

::

   By giving 'y' the CPR property, we ensure that 'f' does too, so we get
        f b x = case fw b x of { r -> I# r }
        fw True  x = case x of y { I# x' -> if x' ==# 3 then x' else 8 }
        fw False x = 3

..

::

   Of course there is the usual risk of re-boxing: we have 'x' available
   boxed and unboxed, but we return the unboxed version for the wrapper to
   box.  If the wrapper doesn't cancel with its caller, we'll end up
   re-boxing something that we did have available in boxed form.

..

 * Any strict binders with product type, can use
   Note [Initial CPR for strict binders].  But we can go a little
   further. Consider

::

      data T = MkT !Int Int

..

::

      f2 (MkT x y) | y>0       = f2 (MkT x (y-1))
                   | otherwise = x

..

::

   For $wf2 we are going to unbox the MkT *and*, since it is strict, the
   first argument of the MkT; see Note [Add demands for strict constructors]
   in WwLib. But then we don't want box it up again when returning it! We want
   'f2' to have the CPR property, so we give 'x' the CPR property.

..

 * It's a bit delicate because if this case is scrutinising something other
   than an argument the original function, we really don't have the unboxed
   version available.  E.g
      g v = case foo v of
              MkT x y | y>0       -> ...
                      | otherwise -> x
   Here we don't have the unboxed 'x' available.  Hence the
   is_var_scrut test when making use of the strictness annotation.
   Slightly ad-hoc, because even if the scrutinee *is* a variable it
   might not be a onre of the arguments to the original function, or a
   sub-component thereof.  But it's simple, and nothing terrible
   happens if we get it wrong.  e.g. #10694.



Note [Initial CPR for strict binders]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/stranal/DmdAnal.hs#L1310>`__

CPR is initialized for a lambda binder in an optimistic manner, i.e,
if the binder is used strictly and at least some of its components as
a product are used, which is checked by the value of the absence
demand.

If the binder is marked demanded with a strict demand, then give it a
CPR signature. Here's a concrete example ('f1' in test T10482a),
assuming h is strict:

::

  f1 :: Int -> Int
  f1 x = case h x of
          A -> x
          B -> f1 (x-1)
          C -> x+1

..

If we notice that 'x' is used strictly, we can give it the CPR
property; and hence f1 gets the CPR property too.  It's sound (doesn't
change strictness) to give it the CPR property because by the time 'x'
is returned (case A above), it'll have been evaluated (by the wrapper
of 'h' in the example).

Moreover, if f itself is strict in x, then we'll pass x unboxed to
f1, and so the boxed version *won't* be available; in that case it's
very helpful to give 'x' the CPR property.

Note that

  * We only want to do this for something that definitely
    has product type, else we may get over-optimistic CPR results
    (e.g. from \x -> x!).

  * See Note [CPR examples]



Note [CPR examples]
~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/stranal/DmdAnal.hs#L1345>`__

Here are some examples (stranal/should_compile/T10482a) of the
usefulness of Note [CPR in a product case alternative].  The main
point: all of these functions can have the CPR property.

    ------- f1 -----------
    -- x is used strictly by h, so it'll be available
    -- unboxed before it is returned in the True branch

::

    f1 :: Int -> Int
    f1 x = case h x x of
            True  -> x
            False -> f1 (x-1)

..


    ------- f2 -----------
    -- x is a strict field of MkT2, so we'll pass it unboxed
    -- to $wf2, so it's available unboxed.  This depends on
    -- the case expression analysing (a subcomponent of) one
    -- of the original arguments to the function, so it's
    -- a bit more delicate.

::

    data T2 = MkT2 !Int Int

..

::

    f2 :: T2 -> Int
    f2 (MkT2 x y) | y>0       = f2 (MkT2 x (y-1))
                  | otherwise = x

..


    ------- f3 -----------
    -- h is strict in x, so x will be unboxed before it
    -- is rerturned in the otherwise case.

::

    data T3 = MkT3 Int Int

..

::

    f1 :: T3 -> Int
    f1 (MkT3 x y) | h x y     = f3 (MkT3 x (y-1))
                  | otherwise = x

..


    ------- f4 -----------
    -- Just like f2, but MkT4 can't unbox its strict
    -- argument automatically, as f2 can

::

    data family Foo a
    newtype instance Foo Int = Foo Int

..

::

    data T4 a = MkT4 !(Foo a) Int

..

::

    f4 :: T4 Int -> Int
    f4 (MkT4 x@(Foo v) y) | y>0       = f4 (MkT4 x (y-1))
                          | otherwise = v

..



Note [Initialising strictness]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/stranal/DmdAnal.hs#L1400>`__

See section 9.2 (Finding fixpoints) of the paper.

Our basic plan is to initialise the strictness of each Id in a
recursive group to "bottom", and find a fixpoint from there.  However,
this group B might be inside an *enclosing* recursive group A, in
which case we'll do the entire fixpoint shebang on for each iteration
of A. This can be illustrated by the following example:

Example:

::

  f [] = []
  f (x:xs) = let g []     = f xs
                 g (y:ys) = y+1 : g ys
              in g (h x)

..

At each iteration of the fixpoint for f, the analyser has to find a
fixpoint for the enclosed function g. In the meantime, the demand
values for g at each iteration for f are *greater* than those we
encountered in the previous iteration for f. Therefore, we can begin
the fixpoint for g not with the bottom value but rather with the
result of the previous analysis. I.e., when beginning the fixpoint
process for g, we can start from the demand signature computed for g
previously and attached to the binding occurrence of g.

To speed things up, we initialise each iteration of A (the enclosing
one) from the result of the last one, which is neatly recorded in each
binder.  That way we make use of earlier iterations of the fixpoint
algorithm. (Cunning plan.)

But on the *first* iteration we want to *ignore* the current strictness
of the Id, and start from "bottom".  Nowadays the Id can have a current
strictness, because interface files record strictness for nested bindings.
To know when we are in the first iteration, we look at the ae_virgin
field of the AnalEnv.



Note [Final Demand Analyser run]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/stranal/DmdAnal.hs#L1438>`__

Some of the information that the demand analyser determines is not always
preserved by the simplifier.  For example, the simplifier will happily rewrite
  \y [Demand=1*U] let x = y in x + x
to
  \y [Demand=1*U] y + y
which is quite a lie.

The once-used information is (currently) only used by the code
generator, though.  So:

 * We zap the used-once info in the worker-wrapper;
   see Note [Zapping Used Once info in WorkWrap] in WorkWrap. If it's
   not reliable, it's better not to have it at all.

 * Just before TidyCore, we add a pass of the demand analyser,
      but WITHOUT subsequent worker/wrapper and simplifier,
   right before TidyCore.  See SimplCore.getCoreToDo.

::

   This way, correct information finds its way into the module interface
   (strictness signatures!) and the code generator (single-entry thunks!)

..

Note that, in contrast, the single-call information (C1(..)) /can/ be
relied upon, as the simplifier tends to be very careful about not
duplicating actual function calls.

Also see #11731.

