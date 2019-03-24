`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/simplCore/FloatOut.hs>`_

Note [Join points]
~~~~~~~~~~~~~~~~~~
Every occurrence of a join point must be a tail call (see Note [Invariants on
join points] in CoreSyn), so we must be careful with how far we float them. The
mechanism for doing so is the *join ceiling*, detailed in Note [Join ceiling]
in SetLevels. For us, the significance is that a binder might be marked to be
dropped at the nearest boundary between tail calls and non-tail calls. For
example:

.. code-block:: haskell

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

.. code-block:: haskell

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

.. code-block:: haskell

  joinrec j1 x1 = jump j2 x1
          j2 x2 = jump j3 x2
          j3 x3 = ... jump j1 (x3 + 1) ... jump j2 (x3 + 1) ...
  in jump j1 x

Now the simplifier will happily inline the trivial j1 and j2, leaving only j3.
Without floating, we're stuck with three loops instead of one.



Note [Floating out of Rec rhss]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider   Rec { f<1,0> = \xy. body }
From the body we may get some floats. The ones with level <1,0> must
stay here, since they may mention f.  Ideally we'd like to make them
part of the Rec block pairs -- but we can't if there are any
FloatCases involved.

Nor is it a good idea to dump them in the rhs, but outside the lambda
    f = case x of I# y -> \xy. body
because now f's arity might get worse, which is Not Good. (And if
there's an SCC around the RHS it might not get better again.
See #5342.)

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

.. code-block:: haskell

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



Note [floatBind for top level]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We may have a *nested* binding whose destination level is (FloatMe tOP_LEVEL), thus
         letrec { foo <0,0> = .... (let bar<0,0> = .. in ..) .... }
The binding for bar will be in the "tops" part of the floating binds,
and thus not partioned by floatBody.

We could perhaps get rid of the 'tops' component of the floating binds,
but this case works just as well.


************************************************************************

\subsection[FloatOut-Expr]{Floating in expressions}
*                                                                      *
************************************************************************


Note [Floating past breakpoints]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We used to disallow floating out of breakpoint ticks (see #10052). However, I
think this is too restrictive.

Consider the case of an expression scoped over by a breakpoint tick,

.. code-block:: haskell

  tick<...> (let x = ... in f x)

In this case it is completely legal to float out x, despite the fact that
breakpoint ticks are scoped,

.. code-block:: haskell

  let x = ... in (tick<...>  f x)

The reason here is that we know that the breakpoint will still be hit when the
expression is entered since the tick still scopes over the RHS.



Note [Avoiding unnecessary floating]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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



Note [Representation of FloatBinds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

