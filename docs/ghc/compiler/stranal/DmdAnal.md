[[src]](https://github.com/ghc/ghc/tree/master/compiler/stranal/DmdAnal.hs)

(c) The GRASP/AQUA Project, Glasgow University, 1993-1998


                        -----------------
                        A demand analysis
                        -----------------


# \subsection{Top level stuff}


### Note: Stamp out space leaks in demand analysis

The demand analysis pass outputs a new copy of the Core program in
which binders have been annotated with demand and strictness
information. It's tiresome to ensure that this information is fully
evaluated everywhere that we produce it, so we just run a single
seqBinds over the output before returning it, to ensure that there are
no references holding on to the input Core program.

This makes a ~30% reduction in peak memory usage when compiling
DynFlags (cf Trac #9675 and #13426).

This is particularly important when we are doing late demand analysis,
since we don't do a seqBinds at any point thereafter. Hence code
generation would hold on to an extra copy of the Core program, via
unforced thunks in demand or strictness information; and it is the
most memory-intensive part of the compilation process, so this added
seqBinds makes a big difference in peak memory usage.


# \subsection{The analyser itself}


### Note: Ensure demand is strict

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


       ----- I don't get this, so commenting out -------
    to_co        = pSnd (coercionKind co)
    dmd'
      | Just tc <- tyConAppTyCon_maybe to_co
      , isRecursiveTyCon tc = cleanEvalDmd
      | otherwise           = dmd
        -- This coerce usually arises from a recursive
        -- newtype, and we don't want to look inside them
        -- for exactly the same reason that we don't look
        -- inside recursive products -- we might not reach
        -- a fixpoint.  So revert to a vanilla Eval demand


### Note: IO hack in the demand analyser

There's a hack here for I/O operations.  Consider

Is this strict in 'y'? Often not! If foo x s performs some observable action
(including raising an exception with raiseIO#, modifying a mutable variable, or
even ending the program normally), then we must not force 'y' (which may fail
to terminate) until we have performed foo x s.

A more complete example (Trac #148, #1592) where this shows up is:
     do { let len = <expensive> ;
        ; when (...) (exitWith ExitSuccess)
        ; print len }

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

### Note: Demand on the scrutinee of a product case

### Note: Demand on case-alternative binders

### Note: Aggregated demand for cardinality

We use different strategies for strictness and usage/cardinality to
"unleash" demands captured on free variables by bindings. Let us
consider the example:

# \subsection{Bindings}


### Note: Safe abortion in the fixed-point iteration


Fixed-point iteration may fail to terminate. But we cannot simply give up and
return the environment and code unchanged! We still need to do one additional
round, for two reasons:

### Note: Lazy and unleashable free variables

This final iteration does not add the variables to the strictness signature
environment, which effectively assigns them 'nopSig' (see "getStrictness")



### Note: Demand analysis for join points

Consider
   g :: (Int,Int) -> Int
   g (p,q) = p+q

   f :: T -> Int -> Int
   f x p = g (join j y = (p,y)
              in case x of
                   A -> j 3
                   B -> j 4
                   C -> (p,7))

If j was a vanilla function definition, we'd analyse its body with
evalDmd, and think that it was lazy in p.  But for join points we can
do better!  We know that j's body will (if called at all) be evaluated
with the demand that consumes the entire join-binding, in this case
the argument demand from g.  Whizzo!  g evaluates both components of
its argument pair, so p will certainly be evaluated if j is called.

### Note: Lazy and unleashable free variables

The implementation is easy.  When analysing a join point, we can
analyse its body with the demand from the entire join-binding (written
let_dmd here).

Another win for join points!  Trac #13543.

### Note: Demand analysis for trivial right-hand sides

Consider
        foo = plusInt |> co
where plusInt is an arity-2 function with known strictness.  Clearly
we want plusInt's strictness to propagate to foo!  But because it has
no manifest lambdas, it won't do so automatically, and indeed 'co' might
have type (Int->Int->Int) ~ T, so we *can't* eta-expand.  So we have a
special case for right-hand sides that are "trivial", namely variables,
casts, type applications, and the like.

### Note: exprArity invariant

### Note: Product demands for function body

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

# \subsection{Strictness signatures and types}


### Note: Do not strictify the argument dictionaries of a dfun

The typechecker can tie recursive knots involving dfuns, so we do the
conservative thing and refrain from strictifying a dfun's argument
dictionaries.


### Note: CPR for sum types

However this means in turn that the *enclosing* function
may be CPR'd (via the returned Justs).  But in the case of
sums, there may be Nothing alternatives; and that messes
up the sum-type CPR.

Conclusion: only do this for products.  It's still not
guaranteed OK for products, but sums definitely lose sometimes.

### Note: CPR for thunks

If the rhs is a thunk, we usually forget the CPR info, because
it is presumably shared (else it would have been inlined, and
so we'd lose sharing if w/w'd it into a function).  E.g.

        let r = case expensive of
                  (a,b) -> (b,a)
        in ...

If we marked r as having the CPR property, then we'd w/w into

But now r is a thunk, which won't be inlined, so we are no further ahead.
But consider

        f x = let r = case expensive of (a,b) -> (b,a)
              in if foo r then r else (x,x)

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

### Note: Optimistic CPR in the "virgin" case

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

### Note: NOINLINE and strictness

Solving this by making all NOINLINE things have no strictness info is overkill.
In particular, it's overkill for runST, which is perfectly respectable.
Consider
        f x = runST (return x)
This should be strict in x.

So the new plan is to define unsafePerformIO using the 'lazy' combinator:

Remember, 'lazy' is a wired-in identity-function Id, of type a->a, which is
magically NON-STRICT, and is inlined after strictness analysis.  So
unsafePerformIO will look non-strict, and that's what we want.

Now we don't need the hack in the strictness analyser.  HOWEVER, this
decision does mean that even a NOINLINE function is not entirely
opaque: some aspect of its implementation leaks out, notably its
strictness.  For example, if you have a function implemented by an
error stub, but which has RULES, you may want it not to be eliminated
in favour of error!

### Note: Lazy and unleashable free variables

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

### Note: Lambda-bound unfoldings

### Note: Case binders and join points

# \subsection{Strictness signatures}


### Note: CPR in a product case alternative

In a case alternative for a product type, we want to give some of the
binders the CPR property.  Specifically

 * The case binder; inside the alternative, the case binder always has
   the CPR property, meaning that a case on it will successfully cancel.
   Example:
        f True  x = case x of y { I# x' -> if x' ==# 3
                                           then y
                                           else I# 8 }
        f False x = I# 3

   By giving 'y' the CPR property, we ensure that 'f' does too, so we get
        f b x = case fw b x of { r -> I# r }
        fw True  x = case x of y { I# x' -> if x' ==# 3 then x' else 8 }
        fw False x = 3

   Of course there is the usual risk of re-boxing: we have 'x' available
   boxed and unboxed, but we return the unboxed version for the wrapper to
   box.  If the wrapper doesn't cancel with its caller, we'll end up
   re-boxing something that we did have available in boxed form.

### Note: Initial CPR for strict binders

      data T = MkT !Int Int

      f2 (MkT x y) | y>0       = f2 (MkT x (y-1))
                   | otherwise = x

### Note: Add demands for strict constructors

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
   happens if we get it wrong.  e.g. Trac #10694.

### Note: Add demands for strict constructors

Consider this program (due to Roman):

    data X a = X !a

    foo :: X Int -> Int -> Int
    foo (X a) n = go 0
     where
       go i | i < n     = a + go (i+1)
            | otherwise = 0

We want the worker for 'foo' too look like this:

    $wfoo :: Int# -> Int# -> Int#

with the first argument unboxed, so that it is not eval'd each time
around the 'go' loop (which would otherwise happen, since 'foo' is not
strict in 'a').  It is sound for the wrapper to pass an unboxed arg
because X is strict, so its argument must be evaluated.  And if we
*don't* pass an unboxed argument, we can't even repair it by adding a
`seq` thus:

    foo (X a) n = a `seq` go 0

because the seq is discarded (very early) since X is strict!

We achieve the effect using addDataConStrictness.  It is called at a
case expression, such as the pattern match on (X a) in the example
above.  After computing how 'a' is used in the alternatives, we add an
extra 'seqDmd' to it.  The case alternative isn't itself strict in the
sub-components, but simply evaluating the scrutinee to HNF does force
those sub-components.

If the argument is not used at all in the alternative (i.e. it is
Absent), then *don't* add a 'seqDmd'.  If we do, it makes it look used
and hence it'll be passed to the worker when it doesn't need to be.
Hence the isAbsDmd test in addDataConStrictness.

There is the usual danger of reboxing, which as usual we ignore. But
if X is monomorphic, and has an UNPACK pragma, then this optimisation
is even more important.  We don't want the wrapper to rebox an unboxed
argument, and pass an Int to $wfoo!

### Note: Initial CPR for strict binders

CPR is initialized for a lambda binder in an optimistic manner, i.e,
if the binder is used strictly and at least some of its components as
a product are used, which is checked by the value of the absence
demand.

If the binder is marked demanded with a strict demand, then give it a
CPR signature. Here's a concrete example ('f1' in test T10482a),
assuming h is strict:

  f1 :: Int -> Int
  f1 x = case h x of
          A -> x
          B -> f1 (x-1)
          C -> x+1

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

### Note: CPR examples

### Note: CPR examples

### Note: CPR in a product case alternative

    ------- f1 -----------
    -- x is used strictly by h, so it'll be available
    -- unboxed before it is returned in the True branch

    f1 :: Int -> Int
    f1 x = case h x x of
            True  -> x
            False -> f1 (x-1)


    ------- f2 -----------
    -- x is a strict field of MkT2, so we'll pass it unboxed
    -- to $wf2, so it's available unboxed.  This depends on
    -- the case expression analysing (a subcomponent of) one
    -- of the original arguments to the function, so it's
    -- a bit more delicate.

    data T2 = MkT2 !Int Int

    f2 :: T2 -> Int
    f2 (MkT2 x y) | y>0       = f2 (MkT2 x (y-1))
                  | otherwise = x


    ------- f3 -----------
    -- h is strict in x, so x will be unboxed before it
    -- is rerturned in the otherwise case.

    data T3 = MkT3 Int Int

    f1 :: T3 -> Int
    f1 (MkT3 x y) | h x y     = f3 (MkT3 x (y-1))
                  | otherwise = x


    ------- f4 -----------
    -- Just like f2, but MkT4 can't unbox its strict
    -- argument automatically, as f2 can

    data family Foo a
    newtype instance Foo Int = Foo Int

    data T4 a = MkT4 !(Foo a) Int

    f4 :: T4 Int -> Int
    f4 (MkT4 x@(Foo v) y) | y>0       = f4 (MkT4 x (y-1))
                          | otherwise = v

### Note: Initialising strictness

See section 9.2 (Finding fixpoints) of the paper.

Our basic plan is to initialise the strictness of each Id in a
recursive group to "bottom", and find a fixpoint from there.  However,
this group B might be inside an *enclosing* recursive group A, in
which case we'll do the entire fixpoint shebang on for each iteration
of A. This can be illustrated by the following example:

Example:

  f [] = []
  f (x:xs) = let g []     = f xs
                 g (y:ys) = y+1 : g ys
              in g (h x)

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

### Note: Final Demand Analyser run

Some of the information that the demand analyser determines is not always
preserved by the simplifier.  For example, the simplifier will happily rewrite
  \y [Demand=1*U] let x = y in x + x
to
  \y [Demand=1*U] y + y
which is quite a lie.

The once-used information is (currently) only used by the code
generator, though.  So:

### Note: Zapping Used Once info in WorkWrap

 * Just before TidyCore, we add a pass of the demand analyser,
      but WITHOUT subsequent worker/wrapper and simplifier,
   right before TidyCore.  See SimplCore.getCoreToDo.

   This way, correct information finds its way into the module interface
   (strictness signatures!) and the code generator (single-entry thunks!)

Note that, in contrast, the single-call information (C1(..)) /can/ be
relied upon, as the simplifier tends to be very careful about not
duplicating actual function calls.

Also see #11731.
