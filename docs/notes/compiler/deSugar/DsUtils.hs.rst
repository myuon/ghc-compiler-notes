`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/deSugar/DsUtils.hs>`_

compiler/deSugar/DsUtils.hs
===========================


Note [Localise pattern binders]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/deSugar/DsUtils.hs#L136>`__

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

See also Note [MatchIds] in Match.hs



Note [mkSelectorBinds]
~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/deSugar/DsUtils.hs#L531>`__

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

::

  Note that we return 't' as the variable to force if the pattern
  is strict (i.e. with -XStrict or an outermost-bang-pattern)

::

  Note that (A) /includes/ the situation where

   * The pattern binds exactly one variable
        let !(Just (Just x) = e in body
     ==>
       let { t = case e of Just (Just v) -> Unit v
           ; v = case t of Unit v -> v }
       in t `seq` body
    The 'Unit' is a one-tuple; see Note [One-tuples] in TysWiredIn
    Note that forcing 't' makes the pattern match happen,
    but does not force 'v'.

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

    Note that
     - Forcing 't' will force the pattern to match fully;
       e.g. will diverge if (snd e) is bottom
     - But 'a' itself is not forced; it is wrapped in a one-tuple
       (see Note [One-tuples] in TysWiredIn)

  *   !(Just x) = e
    ==>
      t = case e of Just x -> Unit x
      x = case t of Unit x -> x

::

    Again, forcing 't' will fail if 'e' yields Nothing.

Note that even though this is rather general, the special cases
work out well:

* One binder, not -XStrict:

::

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



Note [Failure thunks and CPR]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/deSugar/DsUtils.hs#L861>`__

(This note predates join points as formal entities (hence the quotation marks).
We can't use actual join points here (see above); if we did, this would also
solve the CPR problem, since join points don't get CPR'd. See Note [Don't CPR
join points] in WorkWrap.)

When we make a failure point we ensure that it
does not look like a thunk. Example:

::

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
the tail call property.  For example, see #3403.



Note [decideBangHood]
~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/deSugar/DsUtils.hs#L912>`__

With -XStrict we may make /outermost/ patterns more strict.
E.g.
       let (Just x) = e in ...
          ==>
       let !(Just x) = e in ...
and
       f x = e
          ==>
       f !x = e

This adjustment is done by decideBangHood,

  * Just before constructing an EqnInfo, in Match
      (matchWrapper and matchSinglePat)

  * When desugaring a pattern-binding in DsBinds.dsHsBind

Note that it is /not/ done recursively.  See the -XStrict
spec in the user manual.

Specifically:
   ~pat    => pat    -- when -XStrict (even if pat = ~pat')
   !pat    => !pat   -- always
   pat     => !pat   -- when -XStrict
   pat     => pat    -- otherwise

