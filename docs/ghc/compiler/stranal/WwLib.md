[[src]](https://github.com/ghc/ghc/tree/master/compiler/stranal/WwLib.hs)

(c) The GRASP/AQUA Project, Glasgow University, 1993-1998

# A library for the ``worker\/wrapper'' back-end to the strictness analyser

# \subsection[mkWrapperAndWorker]{@mkWrapperAndWorker@}


Here's an example.  The original function is:

\begin{verbatim}
g :: forall a . Int -> [a] -> a

g = \/\ a -> \ x ys ->
        case x of
          0 -> head ys
          _ -> head (tail ys)
\end{verbatim}

From this, we want to produce:
\begin{verbatim}
-- wrapper (an unfolding)
g :: forall a . Int -> [a] -> a

g = \/\ a -> \ x ys ->
        case x of
          I# x# -> $wg a x# ys
            -- call the worker; don't forget the type args!

-- worker
$wg :: forall a . Int# -> [a] -> a

$wg = \/\ a -> \ x# ys ->
        let
            x = I# x#
        in
            case x of               -- note: body of g moved intact
              0 -> head ys
              _ -> head (tail ys)
\end{verbatim}

Something we have to be careful about:  Here's an example:

\begin{verbatim}
-- "f" strictness: U(P)U(P)
f (I# a) (I# b) = a +# b

g = f   -- "g" strictness same as "f"
\end{verbatim}

\tr{f} will get a worker all nice and friendly-like; that's good.
{\em But we don't want a worker for \tr{g}}, even though it has the
same strictness as \tr{f}.  Doing so could break laziness, at best.

Consequently, we insist that the number of strictness-info items is
exactly the same as the number of lambda-bound arguments.  (This is
probably slightly paranoid, but OK in practice.)  If it isn't the
same, we ``revise'' the strictness info, so that we won't propagate
the unusable strictness-info into the interfaces.

# \subsection{The worker wrapper core}


@mkWwBodies@ is called when doing the worker\/wrapper split inside a module.


### Note: Always do CPR w/w

### Note: CPR for thunks

And if something *has* been given the CPR property and we don't w/w, it's
a disaster, because then the enclosing function might say it has the CPR
property, but now doesn't and there a cascade of disaster.  A good example
is Trac #5920.

### Note: Limit w/w arity

Guard against high worker arity as it generates a lot of stack traffic.
A simplified example is Trac #11565#comment:6

Current strategy is very simple: don't perform w/w transformation at all
if the result produces a wrapper with arity higher than -fmax-worker-args=.

It is a bit all or nothing, consider

        f (x,y) (a,b,c,d,e ... , z) = rhs

Currently we will remove all w/w ness entirely. But actually we could
w/w on the (x,y) pair... it's the huge product that is the problem.

Could we instead refrain from w/w on an arg-by-arg basis? Yes, that'd
solve f. But we can get a lot of args from deeply-nested products:

        g (a, (b, (c, (d, ...)))) = rhs

This is harder to spot on an arg-by-arg basis. Previously mkWwStr was
given some "fuel" saying how many arguments it could add; when we ran
out of fuel it would stop w/wing.
Still not very clever because it had a left-right bias.

# \subsection{Making wrapper args}


During worker-wrapper stuff we may end up with an unlifted thing
which we want to let-bind without losing laziness.  So we
add a void argument.  E.g.

        f = /\a -> \x y z -> E::Int#    -- E does not mention x,y,z
==>
        fw = /\ a -> \void -> E
        f  = /\ a -> \x y z -> fw realworld

We use the state-token type which generates no code.


### Note: Protecting the last value argument

If the user writes (\_ -> E), they might be intentionally disallowing
the sharing of E. Since absence analysis and worker-wrapper are keen
to remove such unused arguments, we add in a void argument to prevent
the function from becoming a thunk.

The user can avoid adding the void argument with the -ffun-to-thunk
flag. However, this can create sharing, which may be bad in two ways. 1) It can
create a space leak. 2) It can prevent inlining *under a lambda*. If w/w
removes the last argument from a function f, then f now looks like a thunk, and
so f can't be inlined *under a lambda*.

### Note: Join points and beta-redexes


Originally, the worker would invoke the original function by calling it with
arguments, thus producing a beta-redex for the simplifier to munch away:

  \x y z -> e => (\x y z -> e) wx wy wz

Now that we have special rules about join points, however, this is Not Good if
the original function is itself a join point, as then it may contain invocations
of other join points:

  join j1 x = ...
  join j2 y = if y == 0 then 0 else j1 y

  =>

  join j1 x = ...
  join $wj2 y# = let wy = I# y# in (\y -> if y == 0 then 0 else jump j1 y) wy
  join j2 y = case y of I# y# -> jump $wj2 y#

There can't be an intervening lambda between a join point's declaration and its
occurrences, so $wj2 here is wrong. But of course, this is easy enough to fix:

  ...
  let join $wj2 y# = let wy = I# y# in let y = wy in if y == 0 then 0 else j1 y
  ...

Hence we simply do the beta-reduction here. (This would be harder if we had to
worry about hygiene, but luckily wy is freshly generated.)

### Note: Join points returning functions


It is crucial that the arity of a join point depends on its *callers,* not its
own syntax. What this means is that a join point can have "extra lambdas":

f :: Int -> Int -> (Int, Int) -> Int
f x y = join j (z, w) = \(u, v) -> ...
        in jump j (x, y)

Typically this happens with functions that are seen as computing functions,
rather than being curried. (The real-life example was GraphOps.addConflicts.)

When we create the wrapper, it *must* be in "eta-contracted" form so that the
jump has the right number of arguments:

f x y = join $wj z' w' = \u' v' -> let {z = z'; w = w'; u = u'; v = v'} in ...
             j (z, w)  = jump $wj z w

### Note: Join points and beta-redexes

f x y = let $wj = \z' w' u' v' -> let {z = z'; w = w'; u = u'; v = v'} in ...
            j (z, w) (u, v) = $wj z w u v

Notice that the worker ends up with the same lambdas; it's only the wrapper we
have to be concerned about.

FIXME Currently the functionality to produce "eta-contracted" wrappers is
unimplemented; we simply give up.

# \subsection{Coercion stuff}


We really want to "look through" coerces.
Reason: I've seen this situation:

        let f = coerce T (\s -> E)
        in \x -> case x of
                    p -> coerce T' f
                    q -> \s -> E2
                    r -> coerce T' f

If only we w/w'd f, we'd get
        let f = coerce T (\s -> fw s)
            fw = \s -> E
        in ...

Now we'll inline f to get

        let fw = \s -> E
        in \x -> case x of
                    p -> fw
                    q -> \s -> E2
                    r -> fw

Now we'll see that fw has arity 1, and will arity expand
the \x to get what we want.


### Note: Freshen WW arguments

Wen we do a worker/wrapper split, we must not in-scope names as the arguments
of the worker, else we'll get name capture.  E.g.

   -- y1 is in scope from further out
   f x = ..y1..

If we accidentally choose y1 as a worker argument disaster results:

   fww y1 y2 = let x = (y1,y2) in ...y1...

To avoid this:

  * We use a fresh unique for both type-variable and term-variable binders
    Originally we lacked this freshness for type variables, and that led
    to the very obscure Trac #12562.  (A type variable in the worker shadowed
    an outer term-variable binding.)

  * Because of this cloning we have to substitute in the type/kind of the
    new binders.  That's why we carry the TCvSubst through mkWWargs.

    So we need a decent in-scope set, just in case that type/kind
    itself has foralls.  We get this from the free vars of the RHS of the
    function since those are the only variables that might be captured.
    It's a lazy thunk, which will only be poked if the type/kind has a forall.

    Another tricky case was when f :: forall a. a -> forall a. a->a
    (i.e. with shadowing), and then the worker used the same 'a' twice.

# \subsection{Strictness stuff}


### Note: Unpacking arguments with product and polymorphic demands

The argument is unpacked in a case if it has a product type and has a
strict *and* used demand put on it. I.e., arguments, with demands such
as the following ones:

   <S,U(U, L)>
   <S(L,S),U>

will be unpacked, but

   <S,U> or <B,U>

will not, because the pieces aren't used. This is quite important otherwise
we end up unpacking massive tuples passed to the bottoming function. Example:

        f :: ((Int,Int) -> String) -> (Int,Int) -> a
        f g pr = error (g pr)

        main = print (f fst (1, error "no"))

Does 'main' print "error 1" or "error no"?  We don't really want 'f'
to unbox its second argument.  This actually happened in GHC's onwn
source code, in Packages.applyPackageFlag, which ended up un-boxing
the enormous DynFlags tuple, and being strict in the
as-yet-un-filled-in pkgState files.


### Note: mkWWstr and unsafeCoerce

By using unsafeCoerce, it is possible to make the number of demands fail to
match the number of constructor arguments; this happened in Trac #8037.
If so, the worker/wrapper split doesn't work right and we get a Core Lint
bug.  The fix here is simply to decline to do w/w if that happens.

### Note: Record evaluated-ness in worker/wrapper

Suppose we have

   data T = MkT !Int Int

   f :: T -> T
   f x = e

and f's is strict, and has the CPR property.  The we are going to generate
this w/w split

Note that

* In the worker $wf, inside 'e' we can be sure that x1 will be
  evaluated (it came from unpacking the argument MkT.  But that's no
  immediately apparent in $wf

* In the wrapper 'f', which we'll inline at call sites, we can be sure
  that 'r1' has been evaluated (because it came from unpacking the result
  MkT.  But that is not immediately apparent from the wrapper code.

Missing these facts isn't unsound, but it loses possible future
opportunities for optimisation.

Solution: use setCaseBndrEvald when creating
 (A) The arg binders x1,x2 in mkWstr_one
         See Trac #13077, test T13077
 (B) The result binders r1,r2 in mkWWcpr_help
         See Trace #13077, test T13077a
         And Trac #13027 comment:20, item (4)
to record that the relevant binder is evaluated.

# Type scrutiny that is specific to demand analysis


### Note: Do not unpack class dictionaries

# \subsection{CPR stuff}



@mkWWcpr@ takes the worker/wrapper pair produced from the strictness
info and adds in the CPR transformation.  The worker returns an
unboxed tuple containing non-CPR components.  The wrapper takes this
tuple and re-produces the correct structured output.

The non-CPR results appear ordered in the unboxed tuple as if by a
left-to-right traversal of the result structure.


### Note: non-algebraic or open body type warning


There are a few cases where the W/W transformation is told that something
returns a constructor, but the type at hand doesn't really match this. One
real-world example involves unsafeCoerce:
  foo = IO a
  foo = unsafeCoerce c_exit
  foreign import ccall "c_exit" c_exit :: IO ()
Here CPR will tell you that `foo` returns a () constructor for sure, but trying
to create a worker/wrapper for type `a` obviously fails.
(This was a real example until ee8e792  in libraries/base.)

It does not seem feasible to avoid all such cases already in the analyser (and
after all, the analysis is not really wrong), so we simply do nothing here in
mkWWcpr. But we still want to emit warning with -DDEBUG, to hopefully catch
other cases where something went avoidably wrong.

### Note: Profiling and unpacking