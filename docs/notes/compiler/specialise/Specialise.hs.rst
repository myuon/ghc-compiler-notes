`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/specialise/Specialise.hs>`_

compiler/specialise/Specialise.hs
=================================


Note [Wrap bindings returned by specImports]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/specialise/Specialise.hs#L614>`__

'specImports' returns a set of specialized bindings. However, these are lacking
necessary floated dictionary bindings, which are returned by
UsageDetails(ud_binds). These dictionaries need to be brought into scope with
'wrapDictBinds' before the bindings returned by 'specImports' can be used. See,
for instance, the 'specImports' call in 'specProgram'.



Note [Disabling cross-module specialisation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/specialise/Specialise.hs#L623>`__

Since GHC 7.10 we have performed specialisation of INLINABLE bindings living
in modules outside of the current module. This can sometimes uncover user code
which explodes in size when aggressively optimized. The
-fno-cross-module-specialise option was introduced to allow users to being
bitten by such instances to revert to the pre-7.10 behavior.

See #10491



Note [Warning about missed specialisations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/specialise/Specialise.hs#L779>`__

Suppose
 * In module Lib, you carefully mark a function 'foo' INLINABLE
 * Import Lib(foo) into another module M
 * Call 'foo' at some specialised type in M
Then you jolly well expect it to be specialised in M.  But what if
'foo' calls another function 'Lib.bar'.  Then you'd like 'bar' to be
specialised too.  But if 'bar' is not marked INLINABLE it may well
not be specialised.  The warning Opt_WarnMissedSpecs warns about this.

It's more noisy to warning about a missed specialisation opportunity
for /every/ overloaded imported function, but sometimes useful. That
is what Opt_WarnAllMissedSpecs does.

ToDo: warn about missed opportunities for local functions.



Note [Specialise imported INLINABLE things]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/specialise/Specialise.hs#L796>`__

What imported functions do we specialise?  The basic set is
 * DFuns and things with INLINABLE pragmas.
but with -fspecialise-aggressively we add
 * Anything with an unfolding template

#8874 has a good example of why we want to auto-specialise DFuns.

We have the -fspecialise-aggressively flag (usually off), because we
risk lots of orphan modules from over-vigorous specialisation.
However it's not a big deal: anything non-recursive with an
unfolding-template will probably have been inlined already.



Note [Glom the bindings if imported functions are specialised]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/specialise/Specialise.hs#L810>`__

Suppose we have an imported, *recursive*, INLINABLE function
   f :: Eq a => a -> a
   f = /\a \d x. ...(f a d)...
In the module being compiled we have
   g x = f (x::Int)
Now we'll make a specialised function
   f_spec :: Int -> Int
   f_spec = \x -> ...(f Int dInt)...
   {-# RULE  f Int _ = f_spec #-}
   g = \x. f Int dInt x
Note that f_spec doesn't look recursive
After rewriting with the RULE, we get
   f_spec = \x -> ...(f_spec)...
BUT since f_spec was non-recursive before it'll *stay* non-recursive.
The occurrence analyser never turns a NonRec into a Rec.  So we must
make sure that f_spec is recursive.  Easiest thing is to make all
the specialisations for imported bindings recursive.



Note [Avoiding recursive specialisation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/specialise/Specialise.hs#L831>`__

When we specialise 'f' we may find new overloaded calls to 'g', 'h' in
'f's RHS.  So we want to specialise g,h.  But we don't want to
specialise f any more!  It's possible that f's RHS might have a
recursive yet-more-specialised call, so we'd diverge in that case.
And if the call is to the same type, one specialisation is enough.
Avoiding this recursive specialisation loop is the reason for the
'done' VarSet passed to specImports and specImport.



Note [Floating dictionaries out of cases]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/specialise/Specialise.hs#L1005>`__

Consider
   g = \d. case d of { MkD sc ... -> ...(f sc)... }
Naively we can't float d2's binding out of the case expression,
because 'sc' is bound by the case, and that in turn means we can't
specialise f, which seems a pity.

So we invert the case, by floating out a binding
for 'sc_flt' thus:
    sc_flt = case d of { MkD sc ... -> sc }
Now we can float the call instance for 'f'.  Indeed this is just
what'll happen if 'sc' was originally bound with a let binding,
but case is more efficient, and necessary with equalities. So it's
good to work with both.

You might think that this won't make any difference, because the
call instance will only get nuked by the \d.  BUT if 'g' itself is
specialised, then transitively we should be able to specialise f.

In general, given
   case e of cb { MkD sc ... -> ...(f sc)... }
we transform to
   let cb_flt = e
       sc_flt = case cb_flt of { MkD sc ... -> sc }
   in
   case cb_flt of bg { MkD sc ... -> ....(f sc_flt)... }

The "_flt" things are the floated binds; we use the current substitution
to substitute sc -> sc_flt in the RHS



Note [Account for casts in binding]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/specialise/Specialise.hs#L1376>`__

Consider
   f :: Eq a => a -> IO ()
   {-# INLINABLE f
       StableUnf = (/\a \(d:Eq a) (x:a). blah) |> g
     #-}
   f = ...

In f's stable unfolding we have done some modest simplification which
has pushed the cast to the outside.  (I wonder if this is the Right
Thing, but it's what happens now; see SimplUtils Note [Casts and
lambdas].)  Now that stable unfolding must be specialised, so we want
to push the cast back inside. It would be terrible if the cast
defeated specialisation!  Hence the use of collectBindersPushingCo.



Note [Evidence foralls]
~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/specialise/Specialise.hs#L1392>`__

Suppose (#12212) that we are specialising
   f :: forall a b. (Num a, F a ~ F b) => blah
with a=b=Int. Then the RULE will be something like
   RULE forall (d:Num Int) (g :: F Int ~ F Int).
        f Int Int d g = f_spec
But both varToCoreExpr (when constructing the LHS args), and the
simplifier (when simplifying the LHS args), will transform to
   RULE forall (d:Num Int) (g :: F Int ~ F Int).
        f Int Int d <F Int> = f_spec
by replacing g with Refl.  So now 'g' is unbound, which results in a later
crash. So we use Refl right off the bat, and do not forall-quantify 'g':
 * varToCoreExpr generates a Refl
 * exprsFreeIdsList returns the Ids bound by the args,
   which won't include g

You might wonder if this will match as often, but the simplifier replaces
complicated Refl coercions with Refl pretty aggressively.



Note [Orphans and auto-generated rules]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/specialise/Specialise.hs#L1412>`__

When we specialise an INLINABLE function, or when we have
-fspecialise-aggressively, we auto-generate RULES that are orphans.
We don't want to warn about these, or we'd generate a lot of warnings.
Thus, we only warn about user-specified orphan rules.

Indeed, we don't even treat the module as an orphan module if it has
auto-generated *rule* orphans.  Orphan modules are read every time we
compile, so they are pretty obtrusive and slow down every compilation,
even non-optimised ones.  (Reason: for type class instances it's a
type correctness issue.)  But specialisation rules are strictly for
*optimisation* only so it's fine not to read the interface.

What this means is that a SPEC rules from auto-specialisation in
module M will be used in other modules only if M.hi has been read for
some other reason, which is actually pretty likely.



Note [Make the new dictionaries interesting]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/specialise/Specialise.hs#L1468>`__

Important!  We're going to substitute dx_id1 for d
and we want it to look "interesting", else we won't gather *any*
consequential calls. E.g.
    f d = ...g d....
If we specialise f for a call (f (dfun dNumInt)), we'll get
a consequent call (g d') with an auxiliary definition
    d' = df dNumInt
We want that consequent call to look interesting



Note [From non-recursive to recursive]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/specialise/Specialise.hs#L1480>`__

Even in the non-recursive case, if any dict-binds depend on 'fn' we might
have built a recursive knot

::

      f a d x = <blah>
      MkUD { ud_binds = NonRec d7  (MkD ..f..)
           , ud_calls = ...(f T d7)... }

..

The we generate

::

     Rec { fs x = <blah>[T/a, d7/d]
           f a d x = <blah>
               RULE f T _ = fs
           d7 = ...f... }

..

Here the recursion is only through the RULE.

However we definitely should /not/ make the Rec in this wildly common
case:
      d = ...
      MkUD { ud_binds = NonRec d7 (...d...)
           , ud_calls = ...(f T d7)... }

Here we want simply to add d to the floats, giving
      MkUD { ud_binds = NonRec d (...)
                        NonRec d7 (...d...)
           , ud_calls = ...(f T d7)... }

In general, we need only make this Rec if
  - there are some specialisations (spec_binds non-empty)
  - there are some dict_binds that depend on f (dump_dbs non-empty)



Note [Avoiding loops]
~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/specialise/Specialise.hs#L1513>`__

When specialising /dictionary functions/ we must be very careful to
avoid building loops. Here is an example that bit us badly: #3591

::

     class Eq a => C a
     instance Eq [a] => C [a]

..

This translates to
     dfun :: Eq [a] -> C [a]
     dfun a d = MkD a d (meth d)

::

     d4 :: Eq [T] = <blah>
     d2 ::  C [T] = dfun T d4
     d1 :: Eq [T] = $p1 d2
     d3 ::  C [T] = dfun T d1

..

None of these definitions is recursive. What happened was that we
generated a specialisation:

::

     RULE forall d. dfun T d = dT  :: C [T]
     dT = (MkD a d (meth d)) [T/a, d1/d]
        = MkD T d1 (meth d1)

..

But now we use the RULE on the RHS of d2, to get

::

    d2 = dT = MkD d1 (meth d1)
    d1 = $p1 d2

..

and now d1 is bottom!  The problem is that when specialising 'dfun' we
should first dump "below" the binding all floated dictionary bindings
that mention 'dfun' itself.  So d2 and d3 (and hence d1) must be
placed below 'dfun', and thus unavailable to it when specialising
'dfun'.  That in turn means that the call (dfun T d1) must be
discarded.  On the other hand, the call (dfun T d4) is fine, assuming
d4 doesn't mention dfun.

Solution:
  Discard all calls that mention dictionaries that depend
  (directly or indirectly) on the dfun we are specialising.
  This is done by 'filterCalls'

--------------
Here's another example, this time for an imported dfun, so the call
to filterCalls is in specImports (#13429). Suppose we have
  class Monoid v => C v a where ...

We start with a call
   f @ [Integer] @ Integer $fC[]Integer

Specialising call to 'f' gives dict bindings
   $dMonoid_1 :: Monoid [Integer]
   $dMonoid_1 = M.$p1C @ [Integer] $fC[]Integer

::

   $dC_1 :: C [Integer] (Node [Integer] Integer)
   $dC_1 = M.$fCvNode @ [Integer] $dMonoid_1

..

...plus a recursive call to
   f @ [Integer] @ (Node [Integer] Integer) $dC_1

Specialising that call gives
   $dMonoid_2  :: Monoid [Integer]
   $dMonoid_2  = M.$p1C @ [Integer] $dC_1

::

   $dC_2 :: C [Integer] (Node [Integer] Integer)
   $dC_2 = M.$fCvNode @ [Integer] $dMonoid_2

..

Now we have two calls to the imported function
  M.$fCvNode :: Monoid v => C v a
  M.$fCvNode @v @a m = C m some_fun

But we must /not/ use the call (M.$fCvNode @ [Integer] $dMonoid_2)
for specialisation, else we get:

::

  $dC_1 = M.$fCvNode @ [Integer] $dMonoid_1
  $dMonoid_2 = M.$p1C @ [Integer] $dC_1
  $s$fCvNode = C $dMonoid_2 ...
    RULE M.$fCvNode [Integer] _ _ = $s$fCvNode

..

Now use the rule to rewrite the call in the RHS of $dC_1
and we get a loop!

--------------
Here's yet another example

::

  class C a where { foo,bar :: [a] -> [a] }

..

::

  instance C Int where
     foo x = r_bar x
     bar xs = reverse xs

..

::

  r_bar :: C a => [a] -> [a]
  r_bar xs = bar (xs ++ xs)

..

That translates to:

::

    r_bar a (c::C a) (xs::[a]) = bar a d (xs ++ xs)

..

::

    Rec { $fCInt :: C Int = MkC foo_help reverse
          foo_help (xs::[Int]) = r_bar Int $fCInt xs }

..

The call (r_bar $fCInt) mentions $fCInt,
                        which mentions foo_help,
                        which mentions r_bar
But we DO want to specialise r_bar at Int:

::

    Rec { $fCInt :: C Int = MkC foo_help reverse
          foo_help (xs::[Int]) = r_bar Int $fCInt xs

..

::

          r_bar a (c::C a) (xs::[a]) = bar a d (xs ++ xs)
            RULE r_bar Int _ = r_bar_Int

..

::

          r_bar_Int xs = bar Int $fCInt (xs ++ xs)
           }

..

Note that, because of its RULE, r_bar joins the recursive
group.  (In this case it'll unravel a short moment later.)



Note [Specialising a recursive group]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/specialise/Specialise.hs#L1632>`__

Consider
    let rec { f x = ...g x'...
            ; g y = ...f y'.... }
    in f 'a'
Here we specialise 'f' at Char; but that is very likely to lead to
a specialisation of 'g' at Char.  We must do the latter, else the
whole point of specialisation is lost.

But we do not want to keep iterating to a fixpoint, because in the
presence of polymorphic recursion we might generate an infinite number
of specialisations.

So we use the following heuristic:
  * Arrange the rec block in dependency order, so far as possible
    (the occurrence analyser already does this)

  * Specialise it much like a sequence of lets

  * Then go through the block a second time, feeding call-info from
    the RHSs back in the bottom, as it were

In effect, the ordering maxmimises the effectiveness of each sweep,
and we do just two sweeps.   This should catch almost every case of
monomorphic recursion -- the exception could be a very knotted-up
recursion with multiple cycles tied up together.

This plan is implemented in the Rec case of specBindItself.



Note [Specialisations already covered]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/specialise/Specialise.hs#L1662>`__

We obviously don't want to generate two specialisations for the same
argument pattern.  There are two wrinkles

1. We do the already-covered test in specDefn, not when we generate
the CallInfo in mkCallUDs.  We used to test in the latter place, but
we now iterate the specialiser somewhat, and the Id at the call site
might therefore not have all the RULES that we can see in specDefn

2. What about two specialisations where the second is an *instance*
of the first?  If the more specific one shows up first, we'll generate
specialisations for both.  If the *less* specific one shows up first,
we *don't* currently generate a specialisation for the more specific
one.  (See the call to lookupRule in already_covered.)  Reasons:
  (a) lookupRule doesn't say which matches are exact (bad reason)
  (b) if the earlier specialisation is user-provided, it's
      far from clear that we should auto-specialise further



Note [Auto-specialisation and RULES]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/specialise/Specialise.hs#L1681>`__

Consider:
   g :: Num a => a -> a
   g = ...

::

   f :: (Int -> Int) -> Int
   f w = ...
   {-# RULE f g = 0 #-}

..

Suppose that auto-specialisation makes a specialised version of
g::Int->Int That version won't appear in the LHS of the RULE for f.
So if the specialisation rule fires too early, the rule for f may
never fire.

It might be possible to add new rules, to "complete" the rewrite system.
Thus when adding
        RULE forall d. g Int d = g_spec
also add
        RULE f g_spec = 0

But that's a bit complicated.  For now we ask the programmer's help,
by *copying the INLINE activation pragma* to the auto-specialised
rule.  So if g says {-# NOINLINE[2] g #-}, then the auto-spec rule
will also not be active until phase 2.  And that's what programmers
should jolly well do anyway, even aside from specialisation, to ensure
that g doesn't inline too early.

This in turn means that the RULE would never fire for a NOINLINE
thing so not much point in generating a specialisation at all.



Note [Specialisation shape]
~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/specialise/Specialise.hs#L1712>`__

We only specialise a function if it has visible top-level lambdas
corresponding to its overloading.  E.g. if
        f :: forall a. Eq a => ....
then its body must look like
        f = /\a. \d. ...

Reason: when specialising the body for a call (f ty dexp), we want to
substitute dexp for d, and pick up specialised calls in the body of f.

This doesn't always work.  One example I came across was this:
        newtype Gen a = MkGen{ unGen :: Int -> a }

::

        choose :: Eq a => a -> Gen a
        choose n = MkGen (\r -> n)

..

::

        oneof = choose (1::Int)

..

It's a silly example, but we get
        choose = /\a. g `cast` co
where choose doesn't have any dict arguments.  Thus far I have not
tried to fix this (wait till there's a real example).

Mind you, then 'choose' will be inlined (since RHS is trivial) so
it doesn't matter.  This comes up with single-method classes

   class C a where { op :: a -> a }
   instance C a => C [a] where ....
==>
   $fCList :: C a => C [a]
   $fCList = $copList |> (...coercion>...)
   ....(uses of $fCList at particular types)...

So we suppress the WARN if the rhs is trivial.



Note [Inline specialisations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/specialise/Specialise.hs#L1748>`__

Here is what we do with the InlinePragma of the original function
  * Activation/RuleMatchInfo: both transferred to the
                              specialised function
  * InlineSpec:
       (a) An INLINE pragma is transferred
       (b) An INLINABLE pragma is *not* transferred

Why (a): transfer INLINE pragmas? The point of INLINE was precisely to
specialise the function at its call site, and arguably that's not so
important for the specialised copies.  BUT *pragma-directed*
specialisation now takes place in the typechecker/desugarer, with
manually specified INLINEs.  The specialisation here is automatic.
It'd be very odd if a function marked INLINE was specialised (because
of some local use), and then forever after (including importing
modules) the specialised version wasn't INLINEd.  After all, the
programmer said INLINE!

You might wonder why we specialise INLINE functions at all.  After
all they should be inlined, right?  Two reasons:

 * Even INLINE functions are sometimes not inlined, when they aren't
   applied to interesting arguments.  But perhaps the type arguments
   alone are enough to specialise (even though the args are too boring
   to trigger inlining), and it's certainly better to call the
   specialised version.

 * The RHS of an INLINE function might call another overloaded function,
   and we'd like to generate a specialised version of that function too.
   This actually happens a lot. Consider
      replicateM_ :: (Monad m) => Int -> m a -> m ()
      {-# INLINABLE replicateM_ #-}
      replicateM_ d x ma = ...
   The strictness analyser may transform to
      replicateM_ :: (Monad m) => Int -> m a -> m ()
      {-# INLINE replicateM_ #-}
      replicateM_ d x ma = case x of I# x' -> $wreplicateM_ d x' ma

      $wreplicateM_ :: (Monad m) => Int# -> m a -> m ()
      {-# INLINABLE $wreplicateM_ #-}
      $wreplicateM_ = ...
   Now an importing module has a specialised call to replicateM_, say
   (replicateM_ dMonadIO).  We certainly want to specialise $wreplicateM_!
   This particular example had a huge effect on the call to replicateM_
   in nofib/shootout/n-body.

Why (b): discard INLINABLE pragmas? See #4874 for persuasive examples.
Suppose we have
    {-# INLINABLE f #-}
    f :: Ord a => [a] -> Int
    f xs = letrec f' = ...f'... in f'
Then, when f is specialised and optimised we might get
    wgo :: [Int] -> Int#
    wgo = ...wgo...
    f_spec :: [Int] -> Int
    f_spec xs = case wgo xs of { r -> I# r }
and we clearly want to inline f_spec at call sites.  But if we still
have the big, un-optimised of f (albeit specialised) captured in an
INLINABLE pragma for f_spec, we won't get that optimisation.

So we simply drop INLINABLE pragmas when specialising. It's not really
a complete solution; ignoring specialisation for now, INLINABLE functions
don't get properly strictness analysed, for example. But it works well
for examples involving specialisation, which is the dominant use of
INLINABLE.  See #4874.



Note [Floated dictionary bindings]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/specialise/Specialise.hs#L1842>`__

We float out dictionary bindings for the reasons described under
"Dictionary floating" above.  But not /just/ dictionary bindings.
Consider

::

   f :: Eq a => blah
   f a d = rhs

..

::

   $c== :: T -> T -> Bool
   $c== x y = ...

..

::

   $df :: Eq T
   $df = Eq $c== ...

..

::

   gurgle = ...(f @T $df)...

..

We gather the call info for (f @T $df), and we don't want to drop it
when we come across the binding for $df.  So we add $df to the floats
and continue.  But then we have to add $c== to the floats, and so on.
These all float above the binding for 'f', and and now we can
successfully specialise 'f'.

So the DictBinds in (ud_binds :: Bag DictBind) may contain
non-dictionary bindings too.



Note [Type determines value]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/specialise/Specialise.hs#L2023>`__

Only specialise if all overloading is on non-IP *class* params,
because these are the ones whose *type* determines their *value*.  In
parrticular, with implicit params, the type args *don't* say what the
value of the implicit param is!  See #7101

However, consider
         type family D (v::*->*) :: Constraint
         type instance D [] = ()
         f :: D v => v Char -> Int
If we see a call (f "foo"), we'll pass a "dictionary"
  () |> (g :: () ~ D [])
and it's good to specialise f at this dictionary.

So the question is: can an implicit parameter "hide inside" a
type-family constraint like (D a).  Well, no.  We don't allow
        type instance D Maybe = ?x:Int
Hence the IrredPred case in type_determines_value.
See #7785.



Note [Interesting dictionary arguments]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/specialise/Specialise.hs#L2044>`__

Consider this
         \a.\d:Eq a.  let f = ... in ...(f d)...
There really is not much point in specialising f wrt the dictionary d,
because the code for the specialised f is not improved at all, because
d is lambda-bound.  We simply get junk specialisations.

What is "interesting"?  Just that it has *some* structure.  But what about
variables?

 * A variable might be imported, in which case its unfolding
   will tell us whether it has useful structure

 * Local variables are cloned on the way down (to avoid clashes when
   we float dictionaries), and cloning drops the unfolding
   (cloneIdBndr).  Moreover, we make up some new bindings, and it's a
   nuisance to give them unfoldings.  So we keep track of the
   "interesting" dictionaries as a VarSet in SpecEnv.
   We have to take care to put any new interesting dictionary
   bindings in the set.

We accidentally lost accurate tracking of local variables for a long
time, because cloned variables don't have unfoldings. But makes a
massive difference in a few cases, eg #5113. For nofib as a
whole it's only a small win: 2.2% improvement in allocation for ansi,
1.2% for bspt, but mostly 0.0!  Average 0.1% increase in binary size.

