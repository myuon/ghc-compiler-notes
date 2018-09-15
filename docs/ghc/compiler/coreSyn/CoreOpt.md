[[src]](https://github.com/ghc/ghc/tree/master/compiler/coreSyn/CoreOpt.hs)

(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998


# The Simple Optimiser


### Note: The simple optimiser

The simple optimiser is a lightweight, pure (non-monadic) function
that rapidly does a lot of simple optimisations, including

  - inlining things that occur just once,
      or whose RHS turns out to be trivial
  - beta reduction
  - case of known constructor
  - dead code elimination

It does NOT do any call-site inlining; it only inlines a function if
it can do so unconditionally, dropping the binding.  It thereby
guarantees to leave no un-reduced beta-redexes.

It is careful to follow the guidance of "Secrets of the GHC inliner",
and in particular the pre-inline-unconditionally and
post-inline-unconditionally story, to do effective beta reduction on
functions called precisely once, without repeatedly optimising the same
expression.  In fact, the simple optimiser is a good example of this
little dance in action; the full Simplifier is a lot more complicated.



 No rules active 

### Note: Exported Ids and trivial RHSs

### Note: Top level and postInlineUnconditionally

  * We do no rule rewrites
  * We do no call-site inlining

Those differences obviate the reasons for not inlining a trivial rhs,
and increase the benefit for doing so.  So we unconditionally inline trivial
rhss here.


### Note: Inline prag in simplOpt

If there's an INLINE/NOINLINE pragma that restricts the phase in
which the binder can be inlined, we don't inline here; after all,
we don't know what phase we're in.  Here's an example

# exprIsConApp_maybe


### Note: exprIsConApp_maybe

exprIsConApp_maybe is a very important function.  There are two principal
uses:
  * case e of { .... }
  * cls_op e, where cls_op is a class operation

In both cases you want to know if e is of form (C e1..en) where C is
a data constructor.

However e might not *look* as if

### Note: exprIsConApp_maybe on literal strings

See #9400 and #13317.

Conceptually, a string literal "abc" is just ('a':'b':'c':[]), but in Core
they are represented as unpackCString# "abc"# by MkCore.mkStringExprFS, or
unpackCStringUtf8# when the literal contains multi-byte UTF8 characters.

For optimizations we want to be able to treat it as a list, so they can be
decomposed when used in a case-statement. exprIsConApp_maybe detects those
calls to unpackCString# and returns:

Just (':', [Char], ['a', unpackCString# "bc"]).

We need to be careful about UTF8 strings here. ""# contains a ByteString, so
we must parse it back into a FastString to split off the first character.
That way we can treat unpackCString# and unpackCStringUtf8# in the same way.

We must also be caeful about
   lvl = "foo"#
   ...(unpackCString# lvl)...
to ensure that we see through the let-binding for 'lvl'.  Hence the
(exprIsLiteral_maybe .. arg) in the guard before the call to
dealWithStringLiteral.

### Note: Push coercions in exprIsConApp_maybe

In Trac #13025 I found a case where we had
    op (df @t1 @t2)     -- op is a ClassOp
where
    df = (/\a b. K e1 e2) |> g

To get this to come out we need to simplify on the fly
   ((/\a b. K e1 e2) |> g) @t1 @t2

Hence the use of pushCoArgs.


### Note: Unfolding DFuns

DFuns look like

  df :: forall a b. (Eq a, Eq b) -> Eq (a,b)
  df a b d_a d_b = MkEqD (a,b) ($c1 a b d_a d_b)
                               ($c2 a b d_a d_b)

So to split it up we just need to apply the ops $c1, $c2 etc
to the very same args as the dfun.  It takes a little more work
to compute the type arguments to the dictionary constructor.

### Note: DFun arity check

### Note: DFun unfoldings

### Note: exprIsLambda_maybe

exprIsLambda_maybe will, given an expression `e`, try to turn it into the form
`Lam v e'` (returned as `Just (v,e')`). Besides using lambdas, it looks through
casts (using the Push rule), and it unfolds function calls if the unfolding
has a greater arity than arguments are present.

Currently, it is used in Rules.match, and is required to make
"map coerce = coerce" match.


# The "push rules"


Here we implement the "push rules" from FC papers:

* The push-argument rules, where we can move a coercion past an argument.
  We have
      (fun |> co) arg
  and we want to transform it to
    (fun arg') |> co'
  for some suitable co' and tranformed arg'.

* The PushK rule for data constructors.  We have
       (K e1 .. en) |> co
  and we want to tranform to
       (K e1' .. en')
  by pushing the coercion into the arguments


### Note: collectBindersPushingCo

We just look for coercions of form
   <type> -> blah
(and similarly for foralls) to keep this function simple.  We could do
more elaborate stuff, but it'd involve substitution etc.
