[[src]](https://github.com/ghc/ghc/tree/master/compiler/simplCore/CSE.hs)

(c) The AQUA Project, Glasgow University, 1993-1998

\section{Common subexpression}



                        Simple common sub-expression
                        ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When we see
        x1 = C a b
        x2 = C x1 b
we build up a reverse mapping:   C a b  -> x1
                                 C x1 b -> x2
and apply that to the rest of the program.

When we then see
        y1 = C a b
        y2 = C y1 b
we replace the C a b with x1.  But then we *dont* want to
add   x1 -> y1  to the mapping.  Rather, we want the reverse, y1 -> x1
so that a subsequent binding
        y2 = C y1 b
will get transformed to C x1 b, and then to x2.

So we carry an extra var->var substitution which we apply *before* looking up in the
reverse mapping.

### Note: Shadowing

We have to be careful about shadowing.
For example, consider
        f = \x -> let y = x+x in
                      h = \x -> x+x
                  in ...

Here we must *not* do CSE on the inner x+x!  The simplifier used to guarantee no
shadowing, but it doesn't any more (it proved too hard), so we clone as we go.
We can simply add clones to the substitution already described.

### Note: CSE for bindings

Let-bindings have two cases, implemented by addBinding.

* SUBSTITUTE: applies when the RHS is a variable

     let x = y in ...(h x)....

  Here we want to extend the /substitution/ with x -> y, so that the
  (h x) in the body might CSE with an enclosing (let v = h y in ...).
  NB: the substitution maps InIds, so we extend the substitution with
      a binding for the original InId 'x'

  How can we have a variable on the RHS? Doesn't the simplifier inline them?

    - First, the original RHS might have been (g z) which has CSE'd
      with an enclosing (let y = g z in ...).  This is super-important.
      See Trac #5996:
         x1 = C a b
         x2 = C x1 b
         y1 = C a b
         y2 = C y1 b
      Here we CSE y1's rhs to 'x1', and then we must add (y1->x1) to
      the substitution so that we can CSE the binding for y2.

### Note: CSE for case expressions

* EXTEND THE REVERSE MAPPING: applies in all other cases

     let x = h y in ...(h y)...

  Here we want to extend the /reverse mapping (cs_map)/ so that
  we CSE the (h y) call to x.

  Note that we use EXTEND even for a trivial expression, provided it
  is not a variable or literal. In particular this /includes/ type
  applications. This can be important (Trac #13156); e.g.
     case f @ Int of { r1 ->
     case f @ Int of { r2 -> ...
  Here we want to common-up the two uses of (f @ Int) so we can
  remove one of the case expressions.

### Note: Corner case for case expressions

Notice that
  - The SUBSTITUTE situation extends the substitution (cs_subst)
  - The EXTEND situation extends the reverse mapping (cs_map)

### Note: Top level and postInlineUnconditionally

### Note: CSE for case expressions

### Note: CSE for bindings

For example:

* Trivial scrutinee
     f = \x -> case x of wild {
                 (a:as) -> case a of wild1 {
                             (p,q) -> ...(wild1:as)...

  Here, (wild1:as) is morally the same as (a:as) and hence equal to
  wild. But that's not quite obvious.  In the rest of the compiler we
  want to keep it as (wild1:as), but for CSE purpose that's a bad
  idea.

  By using addBinding we add the binding (wild1 -> a) to the substitution,
  which does exactly the right thing.

  (Notice this is exactly backwards to what the simplifier does, which
  is to try to replaces uses of 'a' with uses of 'wild1'.)

  This is the main reason that addBinding is called with a trivial rhs.

* Non-trivial scrutinee
     case (f x) of y { pat -> ...let z = f x in ... }

  By using addBinding we'll add (f x :-> y) to the cs_map, and
  thereby CSE the inner (f x) to y.

### Note: CSE for INLINE and NOINLINE

There are some subtle interactions of CSE with functions that the user
has marked as INLINE or NOINLINE. (Examples from Roman Leshchinskiy.)
Consider

### Note: Take care with literal strings

Consider this example:

  x = "foo"#
  y = "foo"#
  ...x...y...x...y....

We would normally turn this into:

  x = "foo"#
  y = x
  ...x...x...x...x....

But this breaks an invariant of Core, namely that the RHS of a top-level binding
of type Addr# must be a string literal, not another variable. See Note
[CoreSyn top-level string literals] in CoreSyn.

For this reason, we special case top-level bindings to literal strings and leave
the original RHS unmodified. This produces:

  x = "foo"#
  y = "foo"#
  ...x...x...x...x....

Now 'y' will be discarded as dead code, and we are done.

The net effect is that for the y-binding we want to
  - Use SUBSTITUTE, by extending the substitution with  y :-> x
  - but leave the original binding for y undisturbed

This is done by cse_bind.  I got it wrong the first time (Trac #13367).


### Note: Combine case alternatives

combineAlts is just a more heavyweight version of the use of
combineIdenticalAlts in SimplUtils.prepareAlts.  The basic idea is
to transform

    DEFAULT -> e1
    K x     -> e1
    W y z   -> e2
===>
   DEFAULT -> e1
   W y z   -> e2

In the simplifier we use cheapEqExpr, because it is called a lot.
But here in CSE we use the full eqExpr.  After all, two alterantives usually
differ near the root, so it probably isn't expensive to compare the full
alternative.  It seems like the the same kind of thing that CSE is supposed
to be doing, which is why I put it here.

I acutally saw some examples in the wild, where some inlining made e1 too
big for cheapEqExpr to catch it.

# \section{The CSE envt}
