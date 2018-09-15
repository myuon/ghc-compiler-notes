[[src]](https://github.com/ghc/ghc/tree/master/compiler/deSugar/DsUtils.hs)

(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998


Utilities for desugaring

This module exports some utility functions of no great interest.


# \subsection{ Selecting match variables}


We're about to match against some patterns.  We want to make some
@Ids@ to use as match variables.  If a pattern has an @Id@ readily at
hand, which should indeed be bound to the pattern as a whole, then use it;
otherwise, make one up.


### Note: Localise pattern binders

Consider     module M where
               [Just a] = e
After renaming it looks like
             module M where
               [Just M.a] = e

We don't generalise, since it's a pattern binding, monomorphic, etc,
so after desugaring we may get something like
             M.a = case e of (v:_) ->
                   case v of Just M.a -> M.a
Notice the "M.a" in the pattern; after all, it was in the original
pattern.  However, after optimisation those pattern binders can become
let-binders, and then end up floated to top level.  They have a
different *unique* by then (the simplifier is good about maintaining
proper scoping), but it's BAD to have two top-level bindings with the
External Name M.a, because that turns into two linker symbols for M.a.
It's quite rare for this to actually *happen* -- the only case I know
of is tc003 compiled with the 'hpc' way -- but that only makes it
all the more annoying.

To avoid this, we craftily call 'localiseId' in the desugarer, which
simply turns the External Name for the Id into an Internal one, but
doesn't change the unique.  So the desugarer produces this:
             M.a{r8} = case e of (v:_) ->
                       case v of Just a{r8} -> M.a{r8}
The unique is still 'r8', but the binding site in the pattern
is now an Internal Name.  Now the simplifier's usual mechanisms
will propagate that Name to all the occurrence sites, as well as
un-shadowing it, so we'll get
             M.a{r8} = case e of (v:_) ->
                       case v of Just a{s77} -> a{s77}
In fact, even CoreSubst.simplOptExpr will do this, and simpleOptExpr
runs on the output of the desugarer, so all is well by the end of
the desugaring pass.

# type synonym EquationInfo and access functions for its pieces        

The ``equation info'' used by @match@ is relatively complicated and
worthy of a type synonym and a few handy functions.


# \subsection{Desugarer's versions of some Core functions}



'mkCoreAppDs' and 'mkCoreAppsDs' hand the special-case desugaring of 'seq'.

### Note: Desugaring seq (1)

The [CoreSyn let/app invariant] means that, other things being equal, because
the argument to the outer 'seq' has an unlifted type, we'll use call-by-value thus:

But that is bad for two reasons:
  (a) we now evaluate y before x, and
  (b) we can't bind v to an unboxed pair

### Note: Desugaring seq (2)

Consider
   let chp = case b of { True -> fst x; False -> 0 }
   in chp `seq` ...chp...
Here the seq is designed to plug the space leak of retaining (snd x)
for too long.

If we rely on the ordinary inlining of seq, we'll get
   let chp = case b of { True -> fst x; False -> 0 }
   case chp of _ { I# -> ...chp... }

But since chp is cheap, and the case is an alluring contet, we'll
inline chp into the case scrutinee.  Now there is only one use of chp,
so we'll inline a second copy.  Alas, we've now ruined the purpose of
the seq, by re-introducing the space leak:
    case (case b of {True -> fst x; False -> 0}) of
      I# _ -> ...case b of {True -> fst x; False -> 0}...

We can try to avoid doing this by ensuring that the binder-swap in the
case happens, so we get his at an early stage:
   case chp of chp2 { I# -> ...chp2... }
But this is fragile.  The real culprit is the source program.  Perhaps we
should have said explicitly
   let !chp2 = chp in ...chp2...

But that's painful.  So the code here does a little hack to make seq
more robust: a saturated application of 'seq' is turned *directly* into
the case expression, thus:
   x  `seq` e2 ==> case x of x -> e2    -- Note shadowing!
   e1 `seq` e2 ==> case x of _ -> e2

So we desugar our example to:
   let chp = case b of { True -> fst x; False -> 0 }
   case chp of chp { I# -> ...chp... }
And now all is well.

The reason it's a hack is because if you define mySeq=seq, the hack
won't work on mySeq.

### Note: Desugaring seq (3)

The isLocalId ensures that we don't turn
        True `seq` e
into
        case True of True { ... }
which stupidly tries to bind the datacon 'True'.


# Tuples and selector bindings


This is used in various places to do with lazy patterns.
For each binder $b$ in the pattern, we create a binding:
\begin{verbatim}
    b = case v of pat' -> b'
\end{verbatim}
where @pat'@ is @pat@ with each binder @b@ cloned into @b'@.

ToDo: making these bindings should really depend on whether there's
much work to be done per binding.  If the pattern is complex, it
should be de-mangled once, into a tuple (and then selected from).
Otherwise the demangling can be in-line in the bindings (as here).

Boring!  Boring!  One error message per binder.  The above ToDo is
even more helpful.  Something very similar happens for pattern-bound
expressions.

### Note: mkSelectorBinds

mkSelectorBinds is used to desugar a pattern binding {p = e},
in a binding group:
  let { ...; p = e; ... } in body
where p binds x,y (this list of binders can be empty).
There are two cases.

------ Special case (A) -------
  For a pattern that is just a variable,
     let !x = e in body
  ==>
     let x = e in x `seq` body
  So we return the binding, with 'x' as the variable to seq.

------ Special case (B) -------
  For a pattern that is essentially just a tuple:
      * A product type, so cannot fail
      * Only one level, so that
          - generating multiple matches is fine
          - seq'ing it evaluates the same as matching it
  Then instead we generate
       { v = e
       ; x = case v of p -> x
       ; y = case v of p -> y }
  with 'v' as the variable to force

------ General case (C) -------
  In the general case we generate these bindings:
       let { ...; p = e; ... } in body
  ==>
       let { t = case e of p -> (x,y)
           ; x = case t of (x,y) -> x
           ; y = case t of (x,y) -> y }
       in t `seq` body

  Note that we return 't' as the variable to force if the pattern
  is strict (i.e. with -XStrict or an outermost-bang-pattern)

  Note that (A) /includes/ the situation where

### Note: One-tuples

  * The pattern binds no variables
        let !(True,False) = e in body
    ==>
        let t = case e of (True,False) -> ()
        in t `seq` body


------ Examples ----------
  *   !(_, (_, a)) = e
    ==>
      t = case e of (_, (_, a)) -> Unit a
      a = case t of Unit a -> a

### Note: One-tuples

  *   !(Just x) = e
    ==>
      t = case e of Just x -> Unit x
      x = case t of Unit x -> x

    Again, forcing 't' will fail if 'e' yields Nothing.

Note that even though this is rather general, the special cases
work out well:

* One binder, not -XStrict:

    let Just (Just v) = e in body
  ==>
    let t = case e of Just (Just v) -> Unit v
        v = case t of Unit v -> v
    in body
  ==>
    let v = case (case e of Just (Just v) -> Unit v) of
              Unit v -> v
    in body
  ==>
    let v = case e of Just (Just v) -> v
    in body

* Non-recursive, -XStrict
     let p = e in body
  ==>
     let { t = case e of p -> (x,y)
         ; x = case t of (x,y) -> x
         ; y = case t of (x,y) -> x }
     in t `seq` body
  ==> {inline seq, float x,y bindings inwards}
     let t = case e of p -> (x,y) in
     case t of t' ->
     let { x = case t' of (x,y) -> x
         ; y = case t' of (x,y) -> x } in
     body
  ==> {inline t, do case of case}
     case e of p ->
     let t = (x,y) in
     let { x = case t' of (x,y) -> x
         ; y = case t' of (x,y) -> x } in
     body
  ==> {case-cancellation, drop dead code}
     case e of p -> body

* Special case (B) is there to avoid fruitlessly taking the tuple
  apart and rebuilding it. For example, consider
     { K x y = e }
  where K is a product constructor.  Then general case (A) does:
     { t = case e of K x y -> (x,y)
     ; x = case t of (x,y) -> x
     ; y = case t of (x,y) -> y }
  In the lazy case we can't optimise out this fruitless taking apart
  and rebuilding.  Instead (B) builds
     { v = e
     ; x = case v of K x y -> x
     ; y = case v of K x y -> y }
  which is better.


# 

# Code for pattern-matching and other failures


Generally, we handle pattern matching failure like this: let-bind a
fail-variable, and use that variable if the thing fails:
\begin{verbatim}
        let fail.33 = error "Help"
        in
        case x of
                p1 -> ...
                p2 -> fail.33
                p3 -> fail.33
                p4 -> ...
\end{verbatim}
Then
\begin{itemize}
\item
If the case can't fail, then there'll be no mention of @fail.33@, and the
simplifier will later discard it.

\item
If it can fail in only one way, then the simplifier will inline it.

\item
Only if it is used more than once will the let-binding remain.
\end{itemize}

There's a problem when the result of the case expression is of
unboxed type.  Then the type of @fail.33@ is unboxed too, and
there is every chance that someone will change the let into a case:
\begin{verbatim}
        case error "Help" of
          fail.33 -> case ....
\end{verbatim}

which is of course utterly wrong.  Rather than drop the condition that
only boxed types can be let-bound, we just turn the fail into a function
for the primitive case:
\begin{verbatim}
        let fail.33 :: Void -> Int#
            fail.33 = \_ -> error "Help"
        in
        case x of
                p1 -> ...
                p2 -> fail.33 void
                p3 -> fail.33 void
                p4 -> ...
\end{verbatim}

Now @fail.33@ is a function, so it can be let-bound.

We would *like* to use join points here; in fact, these "fail variables" are
paradigmatic join points! Sadly, this breaks pattern synonyms, which desugar as
CPS functions - i.e. they take "join points" as parameters. It's not impossible
to imagine extending our type system to allow passing join points around (very
carefully), but we certainly don't support it now.

99.99% of the time, the fail variables wind up as join points in short order
anyway, and the Void# doesn't do much harm.


### Note: Failure thunks and CPR

(This note predates join points as formal entities (hence the quotation marks).
We can't use actual join points here (see above); if we did, this would also
solve the CPR problem, since join points don't get CPR'd. See Note [Don't CPR
join points] in WorkWrap.)

When we make a failure point we ensure that it
does not look like a thunk. Example:

   let fail = \rw -> error "urk"
   in case x of
        [] -> fail realWorld#
        (y:ys) -> case ys of
                    [] -> fail realWorld#
                    (z:zs) -> (y,z)

Reason: we know that a failure point is always a "join point" and is
entered at most once.  Adding a dummy 'realWorld' token argument makes
it clear that sharing is not an issue.  And that in turn makes it more
CPR-friendly.  This matters a lot: if you don't get it right, you lose
the tail call property.  For example, see Trac #3403.

# Ticks
