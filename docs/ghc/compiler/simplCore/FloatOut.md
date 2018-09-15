[[src]](https://github.com/ghc/ghc/tree/master/compiler/simplCore/FloatOut.hs)

(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

# Float bindings outwards (towards the top level)

``Long-distance'' floating of bindings towards the top level.



        -----------------
        Overall game plan
        -----------------

The Big Main Idea is:

        To float out sub-expressions that can thereby get outside
        a non-one-shot value lambda, and hence may be shared.


To achieve this we may need to do two thing:

   a) Let-bind the sub-expression:

        f (g x)  ==>  let lvl = f (g x) in lvl

      Now we can float the binding for 'lvl'.

   b) More than that, we may need to abstract wrt a type variable

        \x -> ... /\a -> let v = ...a... in ....

      Here the binding for v mentions 'a' but not 'x'.  So we
      abstract wrt 'a', to give this binding for 'v':

            vp = /\a -> ...a...
            v  = vp a

      Now the binding for vp can float out unimpeded.
      I can't remember why this case seemed important enough to
      deal with, but I certainly found cases where important floats
      didn't happen if we did not abstract wrt tyvars.

With this in mind we can also achieve another goal: lambda lifting.
We can make an arbitrary (function) binding float to top level by
abstracting wrt *all* local variables, not just type variables, leaving
a binding that can be floated right to top level.  Whether or not this
happens is controlled by a flag.

# Random comments


At the moment we never float a binding out to between two adjacent
lambdas.  For example:

@
        \x y -> let t = x+x in ...
===>
        \x -> let t = x+x in \y -> ...
@
Reason: this is less efficient in the case where the original lambda
is never partially applied.

But there's a case I've seen where this might not be true.  Consider:
@
elEm2 x ys
  = elem' x ys
  where
    elem' _ []  = False
    elem' x (y:ys)      = x==y || elem' x ys
@
It turns out that this generates a subexpression of the form
@
        \deq x ys -> let eq = eqFromEqDict deq in ...
@
vwhich might usefully be separated to
@
        \deq -> let eq = eqFromEqDict deq in \xy -> ...
@
Well, maybe.  We don't do this at the moment.

### Note: Join points

### Note: Join ceiling

  (< join j = ... in
     let x = < ... > in
     case < ... > of
       A -> ...
       B -> ...
   >) < ... > < ... >

Here the join ceilings are marked with angle brackets. Either side of an
application is a join ceiling, as is the scrutinee position of a case
expression or the RHS of a let binding (but not a join point).

Why do we *want* do float join points at all? After all, they're never
allocated, so there's no sharing to be gained by floating them. However, the
other benefit of floating is making RHSes small, and this can have a significant
impact. In particular, stream fusion has been known to produce nested loops like
this:

  joinrec j1 x1 =
    joinrec j2 x2 =
      joinrec j3 x3 = ... jump j1 (x3 + 1) ... jump j2 (x3 + 1) ...
      in jump j3 x2
    in jump j2 x1
  in jump j1 x

(Assume x1 and x2 do *not* occur free in j3.)

Here j1 and j2 are wholly superfluous---each of them merely forwards its
argument to j3. Since j3 only refers to x3, we can float j2 and j3 to make
everything one big mutual recursion:

  joinrec j1 x1 = jump j2 x1
          j2 x2 = jump j3 x2
          j3 x3 = ... jump j1 (x3 + 1) ... jump j2 (x3 + 1) ...
  in jump j1 x

Now the simplifier will happily inline the trivial j1 and j2, leaving only j3.
Without floating, we're stuck with three loops instead of one.

# \subsection[floatOutwards]{@floatOutwards@: let-floating interface function}


# \subsection[FloatOut-Bind]{Floating in a binding (the business end)}


### Note: Floating out of Rec rhss

Consider   Rec { f<1,0> = \xy. body }
From the body we may get some floats. The ones with level <1,0> must
stay here, since they may mention f.  Ideally we'd like to make them
part of the Rec block pairs -- but we can't if there are any
FloatCases involved.

Nor is it a good idea to dump them in the rhs, but outside the lambda
    f = case x of I# y -> \xy. body
because now f's arity might get worse, which is Not Good. (And if
there's an SCC around the RHS it might not get better again.
See Trac #5342.)

So, gruesomely, we split the floats into
 * the outer FloatLets, which can join the Rec, and
 * an inner batch starting in a FloatCase, which are then
   pushed *inside* the lambdas.
This loses full-laziness the rare situation where there is a
FloatCase and a Rec interacting.

If there are unlifted FloatLets (that *aren't* join points) among the floats,
we can't add them to the recursive group without angering Core Lint, but since
they must be ok-for-speculation, they can't actually be making any recursive
calls, so we can safely pull them out and keep them non-recursive.

(Why is something getting floated to <1,0> that doesn't make a recursive call?
The case that came up in testing was that f *and* the unlifted binding were
getting floated *to the same place*:

  \x<2,0> ->
    ... <3,0>
    letrec { f<F<2,0>> =
      ... let x'<F<2,0>> = x +# 1# in ...
    } in ...

Everything gets labeled "float to <2,0>" because it all depends on x, but this
makes f and x' look mutually recursive when they're not.

The test was shootout/k-nucleotide, as compiled using commit 47d5dd68 on the
wip/join-points branch.

TODO: This can probably be solved somehow in SetLevels. The difference between
"this *is at* level <2,0>" and "this *depends on* level <2,0>" is very
important.)

### Note: floatBind for top level

We may have a *nested* binding whose destination level is (FloatMe tOP_LEVEL), thus
         letrec { foo <0,0> = .... (let bar<0,0> = .. in ..) .... }
The binding for bar will be in the "tops" part of the floating binds,
and thus not partioned by floatBody.

We could perhaps get rid of the 'tops' component of the floating binds,
but this case works just as well.

# 

# 

### Note: Floating past breakpoints


We used to disallow floating out of breakpoint ticks (see #10052). However, I
think this is too restrictive.

Consider the case of an expression scoped over by a breakpoint tick,

  tick<...> (let x = ... in f x)

In this case it is completely legal to float out x, despite the fact that
breakpoint ticks are scoped,

  let x = ... in (tick<...>  f x)

The reason here is that we know that the breakpoint will still be hit when the
expression is entered since the tick still scopes over the RHS.



### Note: Avoiding unnecessary floating

In general we want to avoid floating a let unnecessarily, because
it might worsen strictness:
    let
       x = ...(let y = e in y+y)....
Here y is demanded.  If we float it outside the lazy 'x=..' then
we'd have to zap its demand info, and it may never be restored.

So at a 'let' we leave the binding right where the are unless
the binding will escape a value lambda, e.g.

(\x -> let y = fac 100 in y)

That's what the partitionByMajorLevel does in the floatExpr (Let ...)
case.

Notice, though, that we must take care to drop any bindings
from the body of the let that depend on the staying-put bindings.

We used instead to do the partitionByMajorLevel on the RHS of an '=',
in floatRhs.  But that was quite tiresome.  We needed to test for
values or trival rhss, because (in particular) we don't want to insert
new bindings between the "=" and the "\".  E.g.
        f = \x -> let <bind> in <body>
We do not want
        f = let <bind> in \x -> <body>
(a) The simplifier will immediately float it further out, so we may
        as well do so right now; in general, keeping rhss as manifest
        values is good
(b) If a float-in pass follows immediately, it might add yet more
        bindings just after the '='.  And some of them might (correctly)
        be strict even though the 'let f' is lazy, because f, being a value,
        gets its demand-info zapped by the simplifier.
And even all that turned out to be very fragile, and broke
altogether when profiling got in the way.

So now we do the partition right at the (Let..) itself.

# \subsection{Utility bits for floating stats}


I didn't implement this with unboxed numbers.  I don't want to be too
strict in this stuff, as it is rarely turned on.  (WDP 95/09)


# \subsection{Utility bits for floating}


### Note: Representation of FloatBinds

The FloatBinds types is somewhat important.  We can get very large numbers
of floating bindings, often all destined for the top level.  A typical example
is     x = [4,2,5,2,5, .... ]
Then we get lots of small expressions like (fromInteger 4), which all get
lifted to top level.

The trouble is that
  (a) we partition these floating bindings *at every binding site*
  (b) SetLevels introduces a new bindings site for every float
So we had better not look at each binding at each binding site!

That is why MajorEnv is represented as a finite map.

We keep the bindings destined for the *top* level separate, because
we float them out even if they don't escape a *value* lambda; see
partitionByMajorLevel.



--       ---- partitionByMajorLevel ----
-- Float it if we escape a value lambda,
--     *or* if we get to the top level
--     *or* if it's a case-float and its minor level is < current
--
-- If we can get to the top level, say "yes" anyway. This means that
--      x = f e
-- transforms to
--    lvl = e
--    x = f lvl
-- which is as it should be

partitionByMajorLevel (Level major _) (FB tops defns)
  = (FB tops outer, heres `unionBags` flattenMajor inner)
  where
    (outer, mb_heres, inner) = M.splitLookup major defns
    heres = case mb_heres of
               Nothing -> emptyBag
               Just h  -> flattenMinor h
