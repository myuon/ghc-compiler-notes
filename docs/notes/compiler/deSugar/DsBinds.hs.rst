Note [Desugaring AbsBinds]
~~~~~~~~~~~~~~~~~~~~~~~~~~
In the general AbsBinds case we desugar the binding to this:

       tup a (d:Num a) = let fm = ...gm...
                             gm = ...fm...
                         in (fm,gm)
       f a d = case tup a d of { (fm,gm) -> fm }
       g a d = case tup a d of { (fm,gm) -> fm }



Note [Rules and inlining]
~~~~~~~~~~~~~~~~~~~~~~~~~
Common special case: no type or dictionary abstraction
This is a bit less trivial than you might suppose
The naive way would be to desugar to something like
        f_lcl = ...f_lcl...     -- The "binds" from AbsBinds
        M.f = f_lcl             -- Generated from "exports"
But we don't want that, because if M.f isn't exported,
it'll be inlined unconditionally at every call site (its rhs is
trivial).  That would be ok unless it has RULES, which would
thereby be completely lost.  Bad, bad, bad.

Instead we want to generate
        M.f = ...f_lcl...
        f_lcl = M.f
Now all is cool. The RULES are attached to M.f (by SimplCore),
and f_lcl is rapidly inlined away.

This does not happen in the same way to polymorphic binds,
because they desugar to
        M.f = /\a. let f_lcl = ...f_lcl... in f_lcl
Although I'm a bit worried about whether full laziness might
float the f_lcl binding out and then inline M.f at its call site



Note [Specialising in no-dict case]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Even if there are no tyvars or dicts, we may have specialisation pragmas.
Class methods can generate
      AbsBinds [] [] [( ... spec-prag]
         { AbsBinds [tvs] [dicts] ...blah }
So the overloading is in the nested AbsBinds. A good example is in GHC.Float:

  class  (Real a, Fractional a) => RealFrac a  where
    round :: (Integral b) => a -> b

  instance  RealFrac Float  where
    {-# SPECIALIZE round :: Float -> Int #-}

The top-level AbsBinds for $cround has no tyvars or dicts (because the
instance does not).  But the method is locally overloaded!



Note [Abstracting over tyvars only]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When abstracting over type variable only (not dictionaries), we don't really need to
built a tuple and select from it, as we do in the general case. Instead we can take

        AbsBinds [a,b] [ ([a,b], fg, fl, _),
                         ([b],   gg, gl, _) ]
                { fl = e1
                  gl = e2
                   h = e3 }

and desugar it to

        fg = /\ab. let B in e1
        gg = /\b. let a = () in let B in S(e2)
        h  = /\ab. let B in e3

where B is the *non-recursive* binding
        fl = fg a b
        gl = gg b
        h  = h a b    -- See (b); note shadowing!

Notice (a) g has a different number of type variables to f, so we must
             use the mkArbitraryType thing to fill in the gaps.
             We use a type-let to do that.

         (b) The local variable h isn't in the exports, and rather than
             clone a fresh copy we simply replace h by (h a b), where
             the two h's have different types!  Shadowing happens here,
             which looks confusing but works fine.

         (c) The result is *still* quadratic-sized if there are a lot of
             small bindings.  So if there are more than some small
             number (10), we filter the binding set B by the free
             variables of the particular RHS.  Tiresome.

Why got to this trouble?  It's a common case, and it removes the
quadratic-sized tuple desugaring.  Less clutter, hopefully faster
compilation, especially in a case where there are a *lot* of
bindings.




Note [Eta-expanding INLINE things]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
   foo :: Eq a => a -> a
   {-# INLINE foo #-}
   foo x = ...

If (foo d) ever gets floated out as a common sub-expression (which can
happen as a result of method sharing), there's a danger that we never
get to do the inlining, which is a Terribly Bad thing given that the
user said "inline"!

To avoid this we pre-emptively eta-expand the definition, so that foo
has the arity with which it is declared in the source code.  In this
example it has arity 2 (one for the Eq and one for x). Doing this
should mean that (foo d) is a PAP and we don't share it.



Note [Nested arities]
~~~~~~~~~~~~~~~~~~~~~
For reasons that are not entirely clear, method bindings come out looking like
this:

  AbsBinds [] [] [$cfromT <= [] fromT]
    $cfromT [InlPrag=INLINE] :: T Bool -> Bool
    { AbsBinds [] [] [fromT <= [] fromT_1]
        fromT :: T Bool -> Bool
        { fromT_1 ((TBool b)) = not b } } }

Note the nested AbsBind.  The arity for the InlineRule on $cfromT should be
gotten from the binding for fromT_1.

It might be better to have just one level of AbsBinds, but that requires more
thought!




Note [Desugar Strict binds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
See https://ghc.haskell.org/trac/ghc/wiki/StrictPragma

Desugaring strict variable bindings looks as follows (core below ==>)

  let !x = rhs
  in  body
==>
  let x = rhs
  in x `seq` body -- seq the variable

and if it is a pattern binding the desugaring looks like

  let !pat = rhs
  in body
==>
  let x = rhs -- bind the rhs to a new variable
      pat = x
  in x `seq` body -- seq the new variable

if there is no variable in the pattern desugaring looks like

  let False = rhs
  in body
==>
  let x = case rhs of {False -> (); _ -> error "Match failed"}
  in x `seq` body

In order to force the Ids in the binding group they are passed around
in the dsHsBind family of functions, and later seq'ed in DsExpr.ds_val_bind.

Consider a recursive group like this

  letrec
     f : g = rhs[f,g]
  in <body>

Without `Strict`, we get a translation like this:

  let t = /\a. letrec tm = rhs[fm,gm]
                      fm = case t of fm:_ -> fm
                      gm = case t of _:gm -> gm
                in
                (fm,gm)

  in let f = /\a. case t a of (fm,_) -> fm
  in let g = /\a. case t a of (_,gm) -> gm
  in <body>

Here `tm` is the monomorphic binding for `rhs`.

With `Strict`, we want to force `tm`, but NOT `fm` or `gm`.
Alas, `tm` isn't in scope in the `in <body>` part.

The simplest thing is to return it in the polymorphic
tuple `t`, thus:

  let t = /\a. letrec tm = rhs[fm,gm]
                      fm = case t of fm:_ -> fm
                      gm = case t of _:gm -> gm
                in
                (tm, fm, gm)

  in let f = /\a. case t a of (_,fm,_) -> fm
  in let g = /\a. case t a of (_,_,gm) -> gm
  in let tm = /\a. case t a of (tm,_,_) -> tm
  in tm `seq` <body>


See https://ghc.haskell.org/trac/ghc/wiki/StrictPragma for a more
detailed explanation of the desugaring of strict bindings.



Note [Strict binds checks]
~~~~~~~~~~~~~~~~~~~~~~~~~~
There are several checks around properly formed strict bindings. They
all link to this Note. These checks must be here in the desugarer because
we cannot know whether or not a type is unlifted until after zonking, due
to levity polymorphism. These checks all used to be handled in the typechecker
in checkStrictBinds (before Jan '17).

We define an "unlifted bind" to be any bind that binds an unlifted id. Note that

  x :: Char
  (# True, x #) = blah

is *not* an unlifted bind. Unlifted binds are detected by HsUtils.isUnliftedHsBind.

Define a "banged bind" to have a top-level bang. Detected by HsPat.isBangedHsBind.
Define a "strict bind" to be either an unlifted bind or a banged bind.

The restrictions are:
  1. Strict binds may not be top-level. Checked in dsTopLHsBinds.

  2. Unlifted binds must also be banged. (There is no trouble to compile an unbanged
     unlifted bind, but an unbanged bind looks lazy, and we don't want users to be
     surprised by the strictness of an unlifted bind.) Checked in first clause
     of DsExpr.ds_val_bind.

  3. Unlifted binds may not have polymorphism (#6078). (That is, no quantified type
     variables or constraints.) Checked in first clause
     of DsExpr.ds_val_bind.

  4. Unlifted binds may not be recursive. Checked in second clause of ds_val_bind.

----------------------


Note [SPECIALISE on INLINE functions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We used to warn that using SPECIALISE for a function marked INLINE
would be a no-op; but it isn't!  Especially with worker/wrapper split
we might have
   {-# INLINE f #-}
   f :: Ord a => Int -> a -> ...
   f d x y = case x of I# x' -> $wf d x' y

We might want to specialise 'f' so that we in turn specialise '$wf'.
We can't even /name/ '$wf' in the source code, so we can't specialise
it even if we wanted to.  #10721 is a case in point.



Note [Activation pragmas for SPECIALISE]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
From a user SPECIALISE pragma for f, we generate
  a) A top-level binding    spec_fn = rhs
  b) A RULE                 f dOrd = spec_fn

We need two pragma-like things:

* spec_fn's inline pragma: inherited from f's inline pragma (ignoring
                           activation on SPEC), unless overriden by SPEC INLINE

* Activation of RULE: from SPECIALISE pragma (if activation given)
                      otherwise from f's inline pragma

This is not obvious (see #5237)!

Examples      Rule activation   Inline prag on spec'd fn
---------------------------------------------------------------------
SPEC [n] f :: ty            [n]   Always, or NOINLINE [n]
                                  copy f's prag

NOINLINE f
SPEC [n] f :: ty            [n]   NOINLINE
                                  copy f's prag

NOINLINE [k] f
SPEC [n] f :: ty            [n]   NOINLINE [k]
                                  copy f's prag

INLINE [k] f
SPEC [n] f :: ty            [n]   INLINE [k]
                                  copy f's prag

SPEC INLINE [n] f :: ty     [n]   INLINE [n]
                                  (ignore INLINE prag on f,
                                  same activation for rule and spec'd fn)

NOINLINE [k] f
SPEC f :: ty                [n]   INLINE [k]




Note [Decomposing the left-hand side of a RULE]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
There are several things going on here.
* drop_dicts: see Note [Drop dictionary bindings on rule LHS]
* simpleOptExpr: see Note [Simplify rule LHS]
* extra_dict_bndrs: see Note [Free dictionaries]



Note [Free tyvars on rule LHS]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
  data T a = C

  foo :: T a -> Int
  foo C = 1

  {-# RULES "myrule"  foo C = 1 #-}

After type checking the LHS becomes (foo alpha (C alpha)), where alpha
is an unbound meta-tyvar.  The zonker in TcHsSyn is careful not to
turn the free alpha into Any (as it usually does).  Instead it turns it
into a TyVar 'a'.  See TcHsSyn Note [Zonking the LHS of a RULE].

Now we must quantify over that 'a'.  It's /really/ inconvenient to do that
in the zonker, because the HsExpr data type is very large.  But it's /easy/
to do it here in the desugarer.

Moreover, we have to do something rather similar for dictionaries;
see Note [Free dictionaries on rule LHS].   So that's why we look for
type variables free on the LHS, and quantify over them.



Note [Free dictionaries on rule LHS]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When the LHS of a specialisation rule, (/\as\ds. f es) has a free dict,
which is presumably in scope at the function definition site, we can quantify
over it too.  *Any* dict with that type will do.

So for example when you have
        f :: Eq a => a -> a
        f = <rhs>
        ... SPECIALISE f :: Int -> Int ...

Then we get the SpecPrag
        SpecPrag (f Int dInt)

And from that we want the rule

        RULE forall dInt. f Int dInt = f_spec
        f_spec = let f = <rhs> in f Int dInt

But be careful!  That dInt might be GHC.Base.$fOrdInt, which is an External
Name, and you can't bind them in a lambda or forall without getting things
confused.   Likewise it might have an InlineRule or something, which would be
utterly bogus. So we really make a fresh Id, with the same unique and type
as the old one, but with an Internal name and no IdInfo.



Note [Drop dictionary bindings on rule LHS]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
drop_dicts drops dictionary bindings on the LHS where possible.
   E.g.  let d:Eq [Int] = $fEqList $fEqInt in f d
     --> f d
   Reasoning here is that there is only one d:Eq [Int], and so we can
   quantify over it. That makes 'd' free in the LHS, but that is later
   picked up by extra_dict_bndrs (Note [Dead spec binders]).

   NB 1: We can only drop the binding if the RHS doesn't bind
         one of the orig_bndrs, which we assume occur on RHS.
         Example
            f :: (Eq a) => b -> a -> a
            {-# SPECIALISE f :: Eq a => b -> [a] -> [a] #-}
         Here we want to end up with
            RULE forall d:Eq a.  f ($dfEqList d) = f_spec d
         Of course, the ($dfEqlist d) in the pattern makes it less likely
         to match, but there is no other way to get d:Eq a

   NB 2: We do drop_dicts *before* simplOptEpxr, so that we expect all
         the evidence bindings to be wrapped around the outside of the
         LHS.  (After simplOptExpr they'll usually have been inlined.)
         dsHsWrapper does dependency analysis, so that civilised ones
         will be simple NonRec bindings.  We don't handle recursive
         dictionaries!

    NB3: In the common case of a non-overloaded, but perhaps-polymorphic
         specialisation, we don't need to bind *any* dictionaries for use
         in the RHS. For example (#8331)
             {-# SPECIALIZE INLINE useAbstractMonad :: ReaderST s Int #-}
             useAbstractMonad :: MonadAbstractIOST m => m Int
         Here, deriving (MonadAbstractIOST (ReaderST s)) is a lot of code
         but the RHS uses no dictionaries, so we want to end up with
             RULE forall s (d :: MonadAbstractIOST (ReaderT s)).
                useAbstractMonad (ReaderT s) d = $suseAbstractMonad s

   #8848 is a good example of where there are some interesting
   dictionary bindings to discard.

The drop_dicts algorithm is based on these observations:

  * Given (let d = rhs in e) where d is a DictId,
    matching 'e' will bind e's free variables.

  * So we want to keep the binding if one of the needed variables (for
    which we need a binding) is in fv(rhs) but not already in fv(e).

  * The "needed variables" are simply the orig_bndrs.  Consider
       f :: (Eq a, Show b) => a -> b -> String
       ... SPECIALISE f :: (Show b) => Int -> b -> String ...
    Then orig_bndrs includes the *quantified* dictionaries of the type
    namely (dsb::Show b), but not the one for Eq Int

So we work inside out, applying the above criterion at each step.




Note [Simplify rule LHS]
~~~~~~~~~~~~~~~~~~~~~~~~
simplOptExpr occurrence-analyses and simplifies the LHS:

   (a) Inline any remaining dictionary bindings (which hopefully
       occur just once)

   (b) Substitute trivial lets, so that they don't get in the way.
       Note that we substitute the function too; we might
       have this as a LHS:  let f71 = M.f Int in f71

   (c) Do eta reduction.  To see why, consider the fold/build rule,
       which without simplification looked like:
          fold k z (build (/\a. g a))  ==>  ...
       This doesn't match unless you do eta reduction on the build argument.
       Similarly for a LHS like
         augment g (build h)
       we do not want to get
         augment (\a. g a) (build h)
       otherwise we don't match when given an argument like
          augment (\a. h a a) (build h)



Note [Matching seqId]
~~~~~~~~~~~~~~~~~~~
The desugarer turns (seq e r) into (case e of _ -> r), via a special-case hack
and this code turns it back into an application of seq!
See Note [Rules for seq] in MkId for the details.



Note [Unused spec binders]
~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
        f :: a -> a
        ... SPECIALISE f :: Eq a => a -> a ...
It's true that this *is* a more specialised type, but the rule
we get is something like this:
        f_spec d = f
        RULE: f = f_spec d
Note that the rule is bogus, because it mentions a 'd' that is
not bound on the LHS!  But it's a silly specialisation anyway, because
the constraint is unused.  We could bind 'd' to (error "unused")
but it seems better to reject the program because it's almost certainly
a mistake.  That's what the isDeadBinder call detects.



Note [No RULES on datacons]
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Previously, `RULES` like

    "JustNothing" forall x . Just x = Nothing

were allowed. Simon Peyton Jones says this seems to have been a
mistake, that such rules have never been supported intentionally,
and that he doesn't know if they can break in horrible ways.
Furthermore, Ben Gamari and Reid Barton are considering trying to
detect the presence of "static data" that the simplifier doesn't
need to traverse at all. Such rules do not play well with that.
So for now, we ban them altogether as requested by #13290. See also #7398.


