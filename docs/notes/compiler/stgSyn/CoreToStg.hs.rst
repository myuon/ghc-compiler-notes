Note [Live vs free]
~~~~~~~~~~~~~~~~~~~

The two are not the same. Liveness is an operational property rather
than a semantic one. A variable is live at a particular execution
point if it can be referred to directly again. In particular, a dead
variable's stack slot (if it has one):

          - should be stubbed to avoid space leaks, and
          - may be reused for something else.

There ought to be a better way to say this. Here are some examples:

        let v = [q] \[x] -> e
        in
        ...v...  (but no q's)

Just after the `in', v is live, but q is dead. If the whole of that
let expression was enclosed in a case expression, thus:

        case (let v = [q] \[x] -> e in ...v...) of
                alts[...q...]

(ie `alts' mention `q'), then `q' is live even after the `in'; because
we'll return later to the `alts' and need it.

Let-no-escapes make this a bit more interesting:

        let-no-escape v = [q] \ [x] -> e
        in
        ...v...

Here, `q' is still live at the `in', because `v' is represented not by
a closure but by the current stack state.  In other words, if `v' is
live then so is `q'. Furthermore, if `e' mentions an enclosing
let-no-escaped variable, then its free variables are also live if `v' is.


Note [What are these SRTs all about?]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Consider the Core program,

    fibs = go 1 1
      where go a b = let c = a + c
                     in c : go b c
    add x = map (\y -> x*y) fibs

In this case we have a CAF, 'fibs', which is quite large after evaluation and
has only one possible user, 'add'. Consequently, we want to ensure that when
all references to 'add' die we can garbage collect any bit of 'fibs' that we
have evaluated.

However, how do we know whether there are any references to 'fibs' still
around? Afterall, the only reference to it is buried in the code generated
for 'add'. The answer is that we record the CAFs referred to by a definition
in its info table, namely a part of it known as the Static Reference Table
(SRT).

Since SRTs are so common, we use a special compact encoding for them in: we
produce one table containing a list of CAFs in a module and then include a
bitmap in each info table describing which entries of this table the closure
references.

See also: Commentary/Rts/Storage/GC/CAFs on the GHC Wiki.


Note [What is a non-escaping let]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

NB: Nowadays this is recognized by the occurrence analyser by turning a
"non-escaping let" into a join point. The following is then an operational
account of join points.

Consider:

    let x = fvs \ args -> e
    in
        if ... then x else
           if ... then x else ...

`x' is used twice (so we probably can't unfold it), but when it is
entered, the stack is deeper than it was when the definition of `x'
happened.  Specifically, if instead of allocating a closure for `x',
we saved all `x's fvs on the stack, and remembered the stack depth at
that moment, then whenever we enter `x' we can simply set the stack
pointer(s) to these remembered (compile-time-fixed) values, and jump
to the code for `x'.

All of this is provided x is:
  1. non-updatable;
  2. guaranteed to be entered before the stack retreats -- ie x is not
     buried in a heap-allocated closure, or passed as an argument to
     something;
  3. all the enters have exactly the right number of arguments,
     no more no less;
  4. all the enters are tail calls; that is, they return to the
     caller enclosing the definition of `x'.

Under these circumstances we say that `x' is non-escaping.

An example of when (4) does not hold:

    let x = ...
    in case x of ...alts...

Here, `x' is certainly entered only when the stack is deeper than when
`x' is defined, but here it must return to ...alts... So we can't just
adjust the stack down to `x''s recalled points, because that would lost
alts' context.

Things can get a little more complicated.  Consider:

    let y = ...
    in let x = fvs \ args -> ...y...
    in ...x...

Now, if `x' is used in a non-escaping way in ...x..., and `y' is used in a
non-escaping way in ...y..., then `y' is non-escaping.

`x' can even be recursive!  Eg:

    letrec x = [y] \ [v] -> if v then x True else ...
    in
        ...(x b)...


Note [Cost-centre initialization plan]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Previously `coreToStg` was initializing cost-centre stack fields as `noCCS`,
and the fields were then fixed by a separate pass `stgMassageForProfiling`.
We now initialize these correctly. The initialization works like this:

  - For non-top level bindings always use `currentCCS`.

  - For top-level bindings, check if the binding is a CAF

    - CAF:      If -fcaf-all is enabled, create a new CAF just for this CAF
                and use it. Note that these new cost centres need to be
                collected to be able to generate cost centre initialization
                code, so `coreToTopStgRhs` now returns `CollectedCCs`.

                If -fcaf-all is not enabled, use "all CAFs" cost centre.

    - Non-CAF:  Top-level (static) data is not counted in heap profiles; nor
                do we set CCCS from it; so we just slam in
                dontCareCostCentre.
--------------------------------------------------------------
Setting variable info: top-level, binds, RHSs
--------------------------------------------------------------


Note [Collect args]
~~~~~~~~~~~~~~~~~~~

This big-lambda case occurred following a rather obscure eta expansion.
It all seems a bit yukky to me.
