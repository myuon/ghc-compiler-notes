[[src]](https://github.com/ghc/ghc/tree/master/compiler/coreSyn/CoreUtils.hs)

(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998


Utility functions on @Core@ syntax


# \subsection{Find the type of a Core atom/expression}


### Note: Type bindings

Core does allow type bindings, although such bindings are
not much used, except in the output of the desuguarer.
Example:
     let a = Int in (\x:a. x)
Given this, exprType must be careful to substitute 'a' in the
result type (Trac #8522).

### Note: Existential variables and silly type synonyms

Consider
        data T = forall a. T (Funny a)
        type Funny a = Bool
        f :: T -> Bool
        f (T x) = x

Now, the type of 'x' is (Funny a), where 'a' is existentially quantified.
That means that 'exprType' and 'coreAltsType' may give a result that *appears*
to mention an out-of-scope type variable.  See Trac #3409 for a more real-world
example.

Various possibilities suggest themselves:

 - Ignore the problem, and make Lint not complain about such variables

 - Expand all type synonyms (or at least all those that discard arguments)
      This is tricky, because at least for top-level things we want to
      retain the type the user originally specified.

 - Expand synonyms on the fly, when the problem arises. That is what
   we are doing here.  It's not too expensive, I think.

Note that there might be existentially quantified coercion variables, too.


# \subsection{Attaching notes}


# \subsection{Other expression construction}


# Operations oer case alternatives


The default alternative must be first, if it exists at all.
This makes it easy to find, though it makes matching marginally harder.


### Note: Unreachable code

It is possible (although unusual) for GHC to find a case expression
that cannot match.  For example:

     data Col = Red | Green | Blue
     x = Red
     f v = case x of
              Red -> ...
              _ -> ...(case x of { Green -> e1; Blue -> e2 })...

Suppose that for some silly reason, x isn't substituted in the case
expression.  (Perhaps there's a NOINLINE on it, or profiling SCC stuff
gets in the way; cf Trac #3118.)  Then the full-lazines pass might produce
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


### Note: Combine identical alternatives

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
defeats combineIdenticalAlts (see Trac #7360).

### Note: Care with impossible-constructors when combining alternatives

Suppose we have (Trac #10538)
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
known-con alternatives that we have eliminated. (In Trac #11172 we
missed the first one.)



# exprIsTrivial


### Note: exprIsTrivial

@exprIsTrivial@ is true of expressions we are unconditionally happy to
                duplicate; simple variables and constants, and type
                applications.  Note that primop Ids aren't considered
                trivial unless

### Note: Variables are trivial

There used to be a gruesome test for (hasNoBinding v) in the
Var case:
        exprIsTrivial (Var v) | hasNoBinding v = idArity v == 0
The idea here is that a constructor worker, like \$wJust, is
really short for (\x -> \$wJust x), because \$wJust has no binding.
So it should be treated like a lambda.  Ditto unsaturated primops.
But now constructor workers are not "have-no-binding" Ids.  And
completely un-applied primops and foreign-call Ids are sufficiently
rare that I plan to allow them to be duplicated and put up with
saturating them.

### Note: Tick trivial

Ticks are only trivial if they are pure annotations. If we treat
"tick<n> x" as trivial, it will be inlined inside lambdas and the
entry count will be skewed, for example.  Furthermore "scc<n> x" will
turn into just "x" in mkTick.

### Note: Empty case is trivial

### Note: Empty case alternatives

If the scrutinee is trivial, then so is the whole expression; and the
CoreToSTG pass in fact drops the case expression leaving only the
scrutinee.

Having more trivial expressions is good.  Moreover, if we don't treat
it as trivial we may land up with let-bindings like
   let v = case x of {} in ...
and after CoreToSTG that gives
   let v = x in ...
and that confuses the code generator (Trac #11155). So best to kill
it off at source.


### Note: getIdFromTrivialExpr

### Note: substTickish

We also need this functionality in CorePrep to extract out Id of a
function which we are saturating.  However, in this case we don't know
if the variable actually refers to a literal; thus we use
'getIdFromTrivialExpr_maybe' to handle this case.  See test
T12076lit for an example where this matters.



exprIsBottom is a very cheap and cheerful function; it may return
False for bottoming expressions, but it never costs much to ask.  See
also CoreArity.exprBotStrictness_maybe, but that's a bit more
expensive.


### Note: Bottoming expressions

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

# exprIsDupable


### Note: exprIsDupable

@exprIsDupable@ is true of expressions that can be duplicated at a modest
                cost in code size.  This will only happen in different case
                branches, so there's no issue about duplicating work.

                That is, exprIsDupable returns True of (f x) even if
                f is very very expensive to call.

                Its only purpose is to avoid fruitless let-binding
                and then inlining of case join points


# exprIsCheap, exprIsExpandable


### Note: exprIsWorkFree

exprIsWorkFree is used when deciding whether to inline something; we
don't inline it if doing so might duplicate work, by peeling off a
complete copy of the expression.  Here we do not want even to
duplicate a primop (Trac #5623):
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

### Note: Case expressions are work-free

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

### Note: exprIsCheap]   See also Note [Interaction of exprIsCheap and lone variables

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

### Note: exprIsCheap and exprIsHNF

Note that exprIsHNF does not imply exprIsCheap.  Eg
        let x = fac 20 in Just x
This responds True to exprIsHNF (you can discard a seq), but
False to exprIsCheap.

### Note: exprIsExpandable

### Note: CONLIKE pragma

It is used to set the uf_expandable field of an Unfolding, and that
in turn is used
  * In RULE matching
  * In exprIsConApp_maybe, exprIsLiteral_maybe, exprIsLambda_maybe

But take care: exprIsExpandable should /not/ be true of primops.  I
found this in test T5623a:
    let q = /\a. Ptr a (a +# b)
    in case q @ Float of Ptr v -> ...q...

q's inlining should not be expandable, else exprIsConApp_maybe will
say that (q @ Float) expands to (Ptr a (a +# b)), and that will
duplicate the (a +# b) primop, which we should not do lightly.
(It's quite hard to trigger this bug, but T13155 does so for GHC 8.0.)

### Note: Arguments and let-bindings exprIsCheapX

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


### Note: isCheapApp: bottoming functions

I'm not sure why we have a special case for bottoming
functions in isCheapApp.  Maybe we don't need it.

### Note: isExpandableApp: bottoming functions

It's important that isExpandableApp does not respond True to bottoming
functions.  Recall  undefined :: HasCallStack => a
Suppose isExpandableApp responded True to (undefined d), and we had:

  x = undefined <dict-expr>

Then Simplify.prepareRhs would ANF the RHS:

  d = <dict-expr>
  x = undefined d

### Note: Cascading inlines

### Note: Record selection

I'm experimenting with making record selection
look cheap, so we will substitute it inside a
lambda.  Particularly for dictionary field selection.

BUT: Take care with (sel d x)!  The (sel d) might be cheap, but
there's no guarantee that (sel d x) will be too.  Hence (n_val_args == 1)

### Note: Expandable overloadings

### Note: exprOkForSpeculation: case expressions

### Note: The litEq rule: converting equality to case

But we restrict it sharply:

* We restrict it to unlifted scrutinees. Consider this:
     case x of y {
       DEFAULT -> ... (let v::Int# = case y of { True  -> e1
                                               ; False -> e2 }
                       in ...) ...

### Note: Binder-swap during float-out

### Note: Floating cases

  Similarly, this is a valid program (albeit a slightly dodgy one)
    let v = case x of { B -> ...; C -> ... }
    in case x of
         A -> ...
         _ ->  ...v...v....
  Should v be considered ok-for-speculation?  Its scrutinee may be
  evaluated, but the alternatives are incomplete so we should not
  evalutate it strictly.

  Now, all this is for lifted types, but it'd be the same for any
  finite unlifted type. We don't have many of them, but we might
  add unlifted algebraic types in due course.

----- Historical note: Trac #3717: --------
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
redundant) by recognising that th e(case <# ds 5 of { ... }) is
ok-for-speculation, even though it has /lifted/ type.  But now join
points do the job nicely.
------- End of historical note ------------

### Note: Primops with lifted arguments

Is this ok-for-speculation (see Trac #13027)?
   reallyUnsafePtrEq# a b
Well, yes.  The primop accepts lifted arguments and does not
evaluate them.  Indeed, in general primops are, well, primitive
and do not perform evaluation.

There is one primop, dataToTag#, which does /require/ a lifted
argument to be evaluted.  To ensure this, CorePrep adds an
eval if it can't see the the argument is definitely evaluated
(see [dataToTag magic] in CorePrep).

We make no attempt to guarantee that dataToTag#'s argument is
evaluated here.  Main reason: it's very fragile to test for the
evaluatedness of a lifted argument.  Consider
    case x of y -> let v = dataToTag# y in ...

where x/y have type Int, say.  'y' looks evaluated (by the enclosing
case) so all is well.  Now the FloatOut pass does a binder-swap (for
very good reasons), changing to
   case x of y -> let v = dataToTag# x in ...

### Note: dataToTag#

### Note: seq# and expr_ok

### Note: seq# and expr_ok

### Note: Primops with lifted arguments

  -- Precisely, it returns @True@ iff:
  --  a) The expression guarantees to terminate,
         ...
  --  d) without throwing a Haskell exception

The lack of this special case caused Trac #5129 to go bad again.
See comment:24 and following

# exprIsHNF, exprIsConLike


### Note: exprIsHNF Tick

We can discard source annotations on HNFs as long as they aren't
tick-like:

  scc c (\x . e)    =>  \x . e
  scc c (C x1..xn)  =>  C x1..xn

So we regard these as HNFs.  Tick annotations that tick are not
regarded as HNF if the expression they surround is HNF, because the
tick is there to tell us that the expression was evaluated, so we
don't want to discard a seq on it.


# Instantiating data constructors


These InstPat functions go here to avoid circularity between DataCon and Id


### Note: Mark evaluated arguments

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

# Equality


# Eta reduction


### Note: Eta reduction conditions

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
  See Trac #1947.

  So it's important to do the right thing.

### Note: Arity care

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

### Note: Eta reduction with casted arguments

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


### Note: Eta reduction of an eval'd function

In Haskell it is not true that    f = \x. f x
because f might be bottom, and 'seq' can distinguish them.

But it *is* true that   f = f `seq` \x. f x
and we'd like to simplify the latter to the former.  This amounts
to the rule that
  * when there is just *one* value argument,
  * f is not bottom
we can eta-reduce    \x. f x  ===>  f

This turned up in Trac #7542.

# \subsection{Determining non-updatable right-hand-sides}


Top-level constructor applications can usually be allocated
statically, but they can't if the constructor, or any of the
arguments, come from another DLL (because we can't refer to static
labels in other DLLs).

If this happens we simply make the RHS into an updatable thunk,
and 'execute' it rather than allocating it statically.


# \subsection{Type utilities}


# StaticPtr


# \subsection{Join points}
