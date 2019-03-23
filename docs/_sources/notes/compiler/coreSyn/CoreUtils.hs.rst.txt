Note [Type bindings]
~~~~~~~~~~~~~~~~~~~~
Core does allow type bindings, although such bindings are
not much used, except in the output of the desugarer.
Example:
     let a = Int in (\x:a. x)
Given this, exprType must be careful to substitute 'a' in the
result type (#8522).



Note [Existential variables and silly type synonyms]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
        data T = forall a. T (Funny a)
        type Funny a = Bool
        f :: T -> Bool
        f (T x) = x

Now, the type of 'x' is (Funny a), where 'a' is existentially quantified.
That means that 'exprType' and 'coreAltsType' may give a result that *appears*
to mention an out-of-scope type variable.  See #3409 for a more real-world
example.

Various possibilities suggest themselves:

 - Ignore the problem, and make Lint not complain about such variables

 - Expand all type synonyms (or at least all those that discard arguments)
      This is tricky, because at least for top-level things we want to
      retain the type the user originally specified.

 - Expand synonyms on the fly, when the problem arises. That is what
   we are doing here.  It's not too expensive, I think.

Note that there might be existentially quantified coercion variables, too.
Not defined with applyTypeToArg because you can't print from CoreSyn.


Note [Binding coercions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider binding a CoVar, c = e.  Then, we must atisfy
Note [CoreSyn type and coercion invariant] in CoreSyn,
which allows only (Coercion co) on the RHS.



Note [Unreachable code]
~~~~~~~~~~~~~~~~~~~~~~~~~~
It is possible (although unusual) for GHC to find a case expression
that cannot match.  For example:

     data Col = Red | Green | Blue
     x = Red
     f v = case x of
              Red -> ...
              _ -> ...(case x of { Green -> e1; Blue -> e2 })...

Suppose that for some silly reason, x isn't substituted in the case
expression.  (Perhaps there's a NOINLINE on it, or profiling SCC stuff
gets in the way; cf #3118.)  Then the full-lazines pass might produce
this

     x = Red
     lvl = case x of { Green -> e1; Blue -> e2 })
     f v = case x of
             Red -> ...
             _ -> ...lvl...

Now if x gets inlined, we won't be able to find a matching alternative
for 'Red'.  That's because 'lvl' is unreachable.  So rather than crashing
we generate (error "Inaccessible alternative").

Similar things can happen (augmented by GADTs) when the Simplifier
filters down the matching alternatives in Simplify.rebuildCase.
-------------------------------


Note [Combine identical alternatives]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If several alternatives are identical, merge them into a single
DEFAULT alternative.  I've occasionally seen this making a big
difference:

     case e of               =====>     case e of
       C _ -> f x                         D v -> ....v....
       D v -> ....v....                   DEFAULT -> f x
       DEFAULT -> f x

The point is that we merge common RHSs, at least for the DEFAULT case.
[One could do something more elaborate but I've never seen it needed.]
To avoid an expensive test, we just merge branches equal to the *first*
alternative; this picks up the common cases
     a) all branches equal
     b) some branches equal to the DEFAULT (which occurs first)

The case where Combine Identical Alternatives transformation showed up
was like this (base/Foreign/C/Err/Error.hs):

        x | p `is` 1 -> e1
          | p `is` 2 -> e2
        ...etc...

where @is@ was something like

        p `is` n = p /= (-1) && p == n

This gave rise to a horrible sequence of cases

        case p of
          (-1) -> $j p
          1    -> e1
          DEFAULT -> $j p

and similarly in cascade for all the join points!

NB: it's important that all this is done in [InAlt], *before* we work
on the alternatives themselves, because Simplify.simplAlt may zap the
occurrence info on the binders in the alternatives, which in turn
defeats combineIdenticalAlts (see #7360).



Note [Care with impossible-constructors when combining alternatives]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have (#10538)
   data T = A | B | C | D

      case x::T of   (Imposs-default-cons {A,B})
         DEFAULT -> e1
         A -> e2
         B -> e1

When calling combineIdentialAlts, we'll have computed that the
"impossible constructors" for the DEFAULT alt is {A,B}, since if x is
A or B we'll take the other alternatives.  But suppose we combine B
into the DEFAULT, to get

      case x::T of   (Imposs-default-cons {A})
         DEFAULT -> e1
         A -> e2

Then we must be careful to trim the impossible constructors to just {A},
else we risk compiling 'e1' wrong!

Not only that, but we take care when there is no DEFAULT beforehand,
because we are introducing one.  Consider

   case x of   (Imposs-default-cons {A,B,C})
     A -> e1
     B -> e2
     C -> e1

Then when combining the A and C alternatives we get

   case x of   (Imposs-default-cons {B})
     DEFAULT -> e1
     B -> e2

Note that we have a new DEFAULT branch that we didn't have before.  So
we need delete from the "impossible-default-constructors" all the
known-con alternatives that we have eliminated. (In #11172 we
missed the first one.)



Note [getIdFromTrivialExpr]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
When substituting in a breakpoint we need to strip away the type cruft
from a trivial expression and get back to the Id.  The invariant is
that the expression we're substituting was originally trivial
according to exprIsTrivial, AND the expression is not a literal.
See Note [substTickish] for how breakpoint substitution preserves
this extra invariant.

We also need this functionality in CorePrep to extract out Id of a
function which we are saturating.  However, in this case we don't know
if the variable actually refers to a literal; thus we use
'getIdFromTrivialExpr_maybe' to handle this case.  See test
T12076lit for an example where this matters.


Note [Bottoming expressions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A bottoming expression is guaranteed to diverge, or raise an
exception.  We can test for it in two different ways, and exprIsBottom
checks for both of these situations:

* Visibly-bottom computations.  For example
      (error Int "Hello")
  is visibly bottom.  The strictness analyser also finds out if
  a function diverges or raises an exception, and puts that info
  in its strictness signature.

* Empty types.  If a type is empty, its only inhabitant is bottom.
  For example:
      data T
      f :: T -> Bool
      f = \(x:t). case x of Bool {}
  Since T has no data constructors, the case alternatives are of course
  empty.  However note that 'x' is not bound to a visibly-bottom value;
  it's the *type* that tells us it's going to diverge.

A GADT may also be empty even though it has constructors:
        data T a where
          T1 :: a -> T Bool
          T2 :: T Int
        ...(case (x::T Char) of {})...
Here (T Char) is uninhabited.  A more realistic case is (Int ~ Bool),
which is likewise uninhabited.




Note [exprIsDupable]
~~~~~~~~~~~~~~~~~~~~
@exprIsDupable@ is true of expressions that can be duplicated at a modest
                cost in code size.  This will only happen in different case
                branches, so there's no issue about duplicating work.

                That is, exprIsDupable returns True of (f x) even if
                f is very very expensive to call.

                Its only purpose is to avoid fruitless let-binding
                and then inlining of case join points


Note [exprIsWorkFree]
~~~~~~~~~~~~~~~~~~~~~
exprIsWorkFree is used when deciding whether to inline something; we
don't inline it if doing so might duplicate work, by peeling off a
complete copy of the expression.  Here we do not want even to
duplicate a primop (#5623):
   eg   let x = a #+ b in x +# x
   we do not want to inline/duplicate x

Previously we were a bit more liberal, which led to the primop-duplicating
problem.  However, being more conservative did lead to a big regression in
one nofib benchmark, wheel-sieve1.  The situation looks like this:

   let noFactor_sZ3 :: GHC.Types.Int -> GHC.Types.Bool
       noFactor_sZ3 = case s_adJ of _ { GHC.Types.I# x_aRs ->
         case GHC.Prim.<=# x_aRs 2 of _ {
           GHC.Types.False -> notDivBy ps_adM qs_adN;
           GHC.Types.True -> lvl_r2Eb }}
       go = \x. ...(noFactor (I# y))....(go x')...

The function 'noFactor' is heap-allocated and then called.  Turns out
that 'notDivBy' is strict in its THIRD arg, but that is invisible to
the caller of noFactor, which therefore cannot do w/w and
heap-allocates noFactor's argument.  At the moment (May 12) we are just
going to put up with this, because the previous more aggressive inlining
(which treated 'noFactor' as work-free) was duplicating primops, which
in turn was making inner loops of array calculations runs slow (#5623)



Note [Case expressions are work-free]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Are case-expressions work-free?  Consider
    let v = case x of (p,q) -> p
        go = \y -> ...case v of ...
Should we inline 'v' at its use site inside the loop?  At the moment
we do.  I experimented with saying that case are *not* work-free, but
that increased allocation slightly.  It's a fairly small effect, and at
the moment we go for the slightly more aggressive version which treats
(case x of ....) as work-free if the alternatives are.

Moreover it improves arities of overloaded functions where
there is only dictionary selection (no construction) involved

Note [exprIsCheap]   See also Note [Interaction of exprIsCheap and lone variables]
~~~~~~~~~~~~~~~~~~   in CoreUnfold.hs
@exprIsCheap@ looks at a Core expression and returns \tr{True} if
it is obviously in weak head normal form, or is cheap to get to WHNF.
[Note that that's not the same as exprIsDupable; an expression might be
big, and hence not dupable, but still cheap.]

By ``cheap'' we mean a computation we're willing to:
        push inside a lambda, or
        inline at more than one place
That might mean it gets evaluated more than once, instead of being
shared.  The main examples of things which aren't WHNF but are
``cheap'' are:

  *     case e of
          pi -> ei
        (where e, and all the ei are cheap)

  *     let x = e in b
        (where e and b are cheap)

  *     op x1 ... xn
        (where op is a cheap primitive operator)

  *     error "foo"
        (because we are happy to substitute it inside a lambda)

Notice that a variable is considered 'cheap': we can push it inside a lambda,
because sharing will make sure it is only evaluated once.



Note [exprIsCheap and exprIsHNF]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Note that exprIsHNF does not imply exprIsCheap.  Eg
        let x = fac 20 in Just x
This responds True to exprIsHNF (you can discard a seq), but
False to exprIsCheap.



Note [Arguments and let-bindings exprIsCheapX]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
What predicate should we apply to the argument of an application, or the
RHS of a let-binding?

We used to say "exprIsTrivial arg" due to concerns about duplicating
nested constructor applications, but see #4978.  So now we just recursively
use exprIsCheapX.

We definitely want to treat let and app the same.  The principle here is
that
   let x = blah in f x
should behave equivalently to
   f blah

This in turn means that the 'letrec g' does not prevent eta expansion
in this (which it previously was):
    f = \x. let v = case x of
                      True -> letrec g = \w. blah
                              in g
                      False -> \x. x
            in \w. v True
------------------


Note [exprIsExpandable]
~~~~~~~~~~~~~~~~~~~~~~~~~~
An expression is "expandable" if we are willing to duplicate it, if doing
so might make a RULE or case-of-constructor fire.  Consider
   let x = (a,b)
       y = build g
   in ....(case x of (p,q) -> rhs)....(foldr k z y)....

We don't inline 'x' or 'y' (see Note [Lone variables] in CoreUnfold),
but we do want

 * the case-expression to simplify
   (via exprIsConApp_maybe, exprIsLiteral_maybe)

 * the foldr/build RULE to fire
   (by expanding the unfolding during rule matching)

So we classify the unfolding of a let-binding as "expandable" (via the
uf_expandable field) if we want to do this kind of on-the-fly
expansion.  Specifically:

* True of constructor applications (K a b)

* True of applications of a "CONLIKE" Id; see Note [CONLIKE pragma] in BasicTypes.
  (NB: exprIsCheap might not be true of this)

* False of case-expressions.  If we have
    let x = case ... in ...(case x of ...)...
  we won't simplify.  We have to inline x.  See #14688.

* False of let-expressions (same reason); and in any case we
  float lets out of an RHS if doing so will reveal an expandable
  application (see SimplEnv.doFloatFromRhs).

* Take care: exprIsExpandable should /not/ be true of primops.  I
  found this in test T5623a:
    let q = /\a. Ptr a (a +# b)
    in case q @ Float of Ptr v -> ...q...

  q's inlining should not be expandable, else exprIsConApp_maybe will
  say that (q @ Float) expands to (Ptr a (a +# b)), and that will
  duplicate the (a +# b) primop, which we should not do lightly.
  (It's quite hard to trigger this bug, but T13155 does so for GHC 8.0.)
-----------------------------------


Note [isCheapApp: bottoming functions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
I'm not sure why we have a special case for bottoming
functions in isCheapApp.  Maybe we don't need it.



Note [isExpandableApp: bottoming functions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It's important that isExpandableApp does not respond True to bottoming
functions.  Recall  undefined :: HasCallStack => a
Suppose isExpandableApp responded True to (undefined d), and we had:

  x = undefined <dict-expr>

Then Simplify.prepareRhs would ANF the RHS:

  d = <dict-expr>
  x = undefined d

This is already bad: we gain nothing from having x bound to (undefined
var), unlike the case for data constructors.  Worse, we get the
simplifier loop described in OccurAnal Note [Cascading inlines].
Suppose x occurs just once; OccurAnal.occAnalNonRecRhs decides x will
certainly_inline; so we end up inlining d right back into x; but in
the end x doesn't inline because it is bottom (preInlineUnconditionally);
so the process repeats.. We could elaborate the certainly_inline logic
some more, but it's better just to treat bottoming bindings as
non-expandable, because ANFing them is a bad idea in the first place.



Note [Record selection]
~~~~~~~~~~~~~~~~~~~~~~~~~~
I'm experimenting with making record selection
look cheap, so we will substitute it inside a
lambda.  Particularly for dictionary field selection.

BUT: Take care with (sel d x)!  The (sel d) might be cheap, but
there's no guarantee that (sel d x) will be too.  Hence (n_val_args == 1)



Note [Expandable overloadings]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose the user wrote this
   {-# RULE  forall x. foo (negate x) = h x #-}
   f x = ....(foo (negate x))....
He'd expect the rule to fire. But since negate is overloaded, we might
get this:
    f = \d -> let n = negate d in \x -> ...foo (n x)...
So we treat the application of a function (negate in this case) to a
*dictionary* as expandable.  In effect, every function is CONLIKE when
it's applied only to dictionaries.




Note [exprOkForSpeculation: case expressions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
exprOkForSpeculation accepts very special case expressions.
Reason: (a ==# b) is ok-for-speculation, but the litEq rules
in PrelRules convert it (a ==# 3#) to
   case a of { DEFAULT -> 0#; 3# -> 1# }
for excellent reasons described in
  PrelRules Note [The litEq rule: converting equality to case].
So, annoyingly, we want that case expression to be
ok-for-speculation too. Bother.

But we restrict it sharply:

* We restrict it to unlifted scrutinees. Consider this:
     case x of y {
       DEFAULT -> ... (let v::Int# = case y of { True  -> e1
                                               ; False -> e2 }
                       in ...) ...

  Does the RHS of v satisfy the let/app invariant?  Previously we said
  yes, on the grounds that y is evaluated.  But the binder-swap done
  by SetLevels would transform the inner alternative to
     DEFAULT -> ... (let v::Int# = case x of { ... }
                     in ...) ....
  which does /not/ satisfy the let/app invariant, because x is
  not evaluated. See Note [Binder-swap during float-out]
  in SetLevels.  To avoid this awkwardness it seems simpler
  to stick to unlifted scrutinees where the issue does not
  arise.

* We restrict it to exhaustive alternatives. A non-exhaustive
  case manifestly isn't ok-for-speculation. for example,
  this is a valid program (albeit a slightly dodgy one)
    let v = case x of { B -> ...; C -> ... }
    in case x of
         A -> ...
         _ ->  ...v...v....
  Should v be considered ok-for-speculation?  Its scrutinee may be
  evaluated, but the alternatives are incomplete so we should not
  evaluate it strictly.

  Now, all this is for lifted types, but it'd be the same for any
  finite unlifted type. We don't have many of them, but we might
  add unlifted algebraic types in due course.


----- Historical note: #15696: --------
  Previously SetLevels used exprOkForSpeculation to guide
  floating of single-alternative cases; it now uses exprIsHNF
  Note [Floating single-alternative cases].

  But in those days, consider
    case e of x { DEAFULT ->
      ...(case x of y
            A -> ...
            _ -> ...(case (case x of { B -> p; C -> p }) of
                       I# r -> blah)...
  If SetLevels considers the inner nested case as
  ok-for-speculation it can do case-floating (in SetLevels).
  So we'd float to:
    case e of x { DEAFULT ->
    case (case x of { B -> p; C -> p }) of I# r ->
    ...(case x of y
            A -> ...
            _ -> ...blah...)...
  which is utterly bogus (seg fault); see #5453.

----- Historical note: #3717: --------
    foo :: Int -> Int
    foo 0 = 0
    foo n = (if n < 5 then 1 else 2) `seq` foo (n-1)

In earlier GHCs, we got this:
    T.$wfoo =
      \ (ww :: GHC.Prim.Int#) ->
        case ww of ds {
          __DEFAULT -> case (case <# ds 5 of _ {
                          GHC.Types.False -> lvl1;
                          GHC.Types.True -> lvl})
                       of _ { __DEFAULT ->
                       T.$wfoo (GHC.Prim.-# ds_XkE 1) };
          0 -> 0 }

Before join-points etc we could only get rid of two cases (which are
redundant) by recognising that the (case <# ds 5 of { ... }) is
ok-for-speculation, even though it has /lifted/ type.  But now join
points do the job nicely.
------- End of historical note ------------




Note [Primops with lifted arguments]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Is this ok-for-speculation (see #13027)?
   reallyUnsafePtrEq# a b
Well, yes.  The primop accepts lifted arguments and does not
evaluate them.  Indeed, in general primops are, well, primitive
and do not perform evaluation.

Bottom line:
  * In exprOkForSpeculation we simply ignore all lifted arguments.
  * In the rare case of primops that /do/ evaluate their arguments,
    (namely DataToTagOp and SeqOp) return False; see
    Note [exprOkForSpeculation and evaluated variables]



Note [exprOkForSpeculation and SeqOp/DataToTagOp]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Most primops with lifted arguments don't evaluate them
(see Note [Primops with lifted arguments]), so we can ignore
that argument entirely when doing exprOkForSpeculation.

But DataToTagOp and SeqOp are exceptions to that rule.
For reasons described in Note [exprOkForSpeculation and
evaluated variables], we simply return False for them.

Not doing this made #5129 go bad.
Lots of discussion in #15696.



Note [exprOkForSpeculation and evaluated variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Recall that
  seq#       :: forall a s. a -> State# s -> (# State# s, a #)
  dataToTag# :: forall a.   a -> Int#
must always evaluate their first argument.

Now consider these examples:
 * case x of y { DEFAULT -> ....y.... }
   Should 'y' (alone) be considered ok-for-speculation?

 * case x of y { DEFAULT -> ....f (dataToTag# y)... }
   Should (dataToTag# y) be considered ok-for-spec?

You could argue 'yes', because in the case alternative we know that
'y' is evaluated.  But the binder-swap transformation, which is
extremely useful for float-out, changes these expressions to
   case x of y { DEFAULT -> ....x.... }
   case x of y { DEFAULT -> ....f (dataToTag# x)... }

And now the expression does not obey the let/app invariant!  Yikes!
Moreover we really might float (f (dataToTag# x)) outside the case,
and then it really, really doesn't obey the let/app invariant.

The solution is simple: exprOkForSpeculation does not try to take
advantage of the evaluated-ness of (lifted) variables.  And it returns
False (always) for DataToTagOp and SeqOp.

Note that exprIsHNF /can/ and does take advantage of evaluated-ness;
it doesn't have the trickiness of the let/app invariant to worry about.



Note [exprIsHNF]             See also Note [exprIsCheap and exprIsHNF]
~~~~~~~~~~~~~~~~


Note [Mark evaluated arguments]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When pattern matching on a constructor with strict fields, the binder
can have an 'evaldUnfolding'.  Moreover, it *should* have one, so that
when loading an interface file unfolding like:
  data T = MkT !Int
  f x = case x of { MkT y -> let v::Int# = case y of I# n -> n+1
                             in ... }
we don't want Lint to complain.  The 'y' is evaluated, so the
case in the RHS of the binding for 'v' is fine.  But only if we
*know* that 'y' is evaluated.

c.f. add_evals in Simplify.simplAlt



Note [Eta reduction conditions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We try for eta reduction here, but *only* if we get all the way to an
trivial expression.  We don't want to remove extra lambdas unless we
are going to avoid allocating this thing altogether.

There are some particularly delicate points here:

* We want to eta-reduce if doing so leaves a trivial expression,
  *including* a cast.  For example
       \x. f |> co  -->  f |> co
  (provided co doesn't mention x)

* Eta reduction is not valid in general:
        \x. bot  /=  bot
  This matters, partly for old-fashioned correctness reasons but,
  worse, getting it wrong can yield a seg fault. Consider
        f = \x.f x
        h y = case (case y of { True -> f `seq` True; False -> False }) of
                True -> ...; False -> ...

  If we (unsoundly) eta-reduce f to get f=f, the strictness analyser
  says f=bottom, and replaces the (f `seq` True) with just
  (f `cast` unsafe-co).  BUT, as thing stand, 'f' got arity 1, and it
  *keeps* arity 1 (perhaps also wrongly).  So CorePrep eta-expands
  the definition again, so that it does not termninate after all.
  Result: seg-fault because the boolean case actually gets a function value.
  See #1947.

  So it's important to do the right thing.

* Note [Arity care]: we need to be careful if we just look at f's
  arity. Currently (Dec07), f's arity is visible in its own RHS (see
  Note [Arity robustness] in SimplEnv) so we must *not* trust the
  arity when checking that 'f' is a value.  Otherwise we will
  eta-reduce
      f = \x. f x
  to
      f = f
  Which might change a terminating program (think (f `seq` e)) to a
  non-terminating one.  So we check for being a loop breaker first.

  However for GlobalIds we can look at the arity; and for primops we
  must, since they have no unfolding.

* Regardless of whether 'f' is a value, we always want to
  reduce (/\a -> f a) to f
  This came up in a RULE: foldr (build (/\a -> g a))
  did not match           foldr (build (/\b -> ...something complex...))
  The type checker can insert these eta-expanded versions,
  with both type and dictionary lambdas; hence the slightly
  ad-hoc isDictId

* Never *reduce* arity. For example
      f = \xy. g x y
  Then if h has arity 1 we don't want to eta-reduce because then
  f's arity would decrease, and that is bad

These delicacies are why we don't use exprIsTrivial and exprIsHNF here.
Alas.



Note [Eta reduction with casted arguments]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
    (\(x:t3). f (x |> g)) :: t3 -> t2
  where
    f :: t1 -> t2
    g :: t3 ~ t1
This should be eta-reduced to

    f |> (sym g -> t2)

So we need to accumulate a coercion, pushing it inward (past
variable arguments only) thus:
   f (x |> co_arg) |> co  -->  (f |> (sym co_arg -> co)) x
   f (x:t)         |> co  -->  (f |> (t -> co)) x
   f @ a           |> co  -->  (f |> (forall a.co)) @ a
   f @ (g:t1~t2)   |> co  -->  (f |> (t1~t2 => co)) @ (g:t1~t2)
These are the equations for ok_arg.

It's true that we could also hope to eta reduce these:
    (\xy. (f x |> g) y)
    (\xy. (f x y) |> g)
But the simplifier pushes those casts outwards, so we don't
need to address that here.


Note [Eta reduction of an eval'd function]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In Haskell it is not true that    f = \x. f x
because f might be bottom, and 'seq' can distinguish them.

But it *is* true that   f = f `seq` \x. f x
and we'd like to simplify the latter to the former.  This amounts
to the rule that
  * when there is just *one* value argument,
  * f is not bottom
we can eta-reduce    \x. f x  ===>  f

This turned up in #7542.


