[[src]](https://github.com/ghc/ghc/tree/master/compiler/specialise/Specialise.hs)

(c) The GRASP/AQUA Project, Glasgow University, 1993-1998

# Stamping out overloading, and (optionally) polymorphism

# \subsection[notes-Specialise]{Implementation notes [SLPJ, Aug 18 1993]}


These notes describe how we implement specialisation to eliminate
overloading.

The specialisation pass works on Core
syntax, complete with all the explicit dictionary application,
abstraction and construction as added by the type checker.  The
existing type checker remains largely as it is.

One important thought: the {\em types} passed to an overloaded
function, and the {\em dictionaries} passed are mutually redundant.
If the same function is applied to the same type(s) then it is sure to
be applied to the same dictionary(s)---or rather to the same {\em
values}.  (The arguments might look different but they will evaluate
to the same value.)

Second important thought: we know that we can make progress by
treating dictionary arguments as static and worth specialising on.  So
we can do without binding-time analysis, and instead specialise on
dictionary arguments and no others.

# basic idea

Suppose we have

        let f = <f_rhs>
        in <body>

and suppose f is overloaded.

STEP 1: CALL-INSTANCE COLLECTION

We traverse <body>, accumulating all applications of f to types and
dictionaries.

(Might there be partial applications, to just some of its types and
dictionaries?  In principle yes, but in practice the type checker only
builds applications of f to all its types and dictionaries, so partial
applications could only arise as a result of transformation, and even
then I think it's unlikely.  In any case, we simply don't accumulate such
partial applications.)


STEP 2: EQUIVALENCES

So now we have a collection of calls to f:
        f t1 t2 d1 d2
        f t3 t4 d3 d4
        ...
Notice that f may take several type arguments.  To avoid ambiguity, we
say that f is called at type t1/t2 and t3/t4.

We take equivalence classes using equality of the *types* (ignoring
the dictionary args, which as mentioned previously are redundant).

STEP 3: SPECIALISATION

For each equivalence class, choose a representative (f t1 t2 d1 d2),
and create a local instance of f, defined thus:

        f@t1/t2 = <f_rhs> t1 t2 d1 d2

f_rhs presumably has some big lambdas and dictionary lambdas, so lots
of simplification will now result.  However we don't actually *do* that
simplification.  Rather, we leave it for the simplifier to do.  If we
*did* do it, though, we'd get more call instances from the specialised
RHS.  We can work out what they are by instantiating the call-instance
set from f's RHS with the types t1, t2.

Add this new id to f's IdInfo, to record that f has a specialised version.

Before doing any of this, check that f's IdInfo doesn't already
tell us about an existing instance of f at the required type/s.
(This might happen if specialisation was applied more than once, or
it might arise from user SPECIALIZE pragmas.)

Recursion
~~~~~~~~~

Wait a minute!  What if f is recursive?  Then we can't just plug in
its right-hand side, can we?

But it's ok.  The type checker *always* creates non-recursive definitions
for overloaded recursive functions.  For example:

        f x = f (x+x)           -- Yes I know its silly

becomes

        f a (d::Num a) = let p = +.sel a d
                         in
                         letrec fl (y::a) = fl (p y y)
                         in
                         fl

We still have recursion for non-overloaded functions which we
specialise, but the recursive call should get specialised to the
same recursive version.

# Polymorphism 1


All this is crystal clear when the function is applied to *constant
types*; that is, types which have no type variables inside.  But what if
it is applied to non-constant types?  Suppose we find a call of f at type
t1/t2.  There are two possibilities:

(a) The free type variables of t1, t2 are in scope at the definition point
of f.  In this case there's no problem, we proceed just as before.  A common
example is as follows.  Here's the Haskell:

        g y = let f x = x+x
              in f y + f y

After typechecking we have

        g a (d::Num a) (y::a) = let f b (d'::Num b) (x::b) = +.sel b d' x x
                                in +.sel a d (f a d y) (f a d y)

Notice that the call to f is at type type "a"; a non-constant type.
Both calls to f are at the same type, so we can specialise to give:

        g a (d::Num a) (y::a) = let f@a (x::a) = +.sel a d x x
                                in +.sel a d (f@a y) (f@a y)


(b) The other case is when the type variables in the instance types
are *not* in scope at the definition point of f.  The example we are
working with above is a good case.  There are two instances of (+.sel a d),
but "a" is not in scope at the definition of +.sel.  Can we do anything?
Yes, we can "common them up", a sort of limited common sub-expression deal.
This would give:

        g a (d::Num a) (y::a) = let +.sel@a = +.sel a d
                                    f@a (x::a) = +.sel@a x x
                                in +.sel@a (f@a y) (f@a y)

This can save work, and can't be spotted by the type checker, because
the two instances of +.sel weren't originally at the same type.

Further notes on (b)

* There are quite a few variations here.  For example, the defn of
  +.sel could be floated ouside the \y, to attempt to gain laziness.
  It certainly mustn't be floated outside the \d because the d has to
  be in scope too.

* We don't want to inline f_rhs in this case, because
that will duplicate code.  Just commoning up the call is the point.

* Nothing gets added to +.sel's IdInfo.

* Don't bother unless the equivalence class has more than one item!

Not clear whether this is all worth it.  It is of course OK to
simply discard call-instances when passing a big lambda.

# 2 -- Overloading

Consider a function whose most general type is

        f :: forall a b. Ord a => [a] -> b -> b

There is really no point in making a version of g at Int/Int and another
at Int/Bool, because it's only instantiating the type variable "a" which
buys us any efficiency. Since g is completely polymorphic in b there
ain't much point in making separate versions of g for the different
b types.

That suggests that we should identify which of g's type variables
are constrained (like "a") and which are unconstrained (like "b").
Then when taking equivalence classes in STEP 2, we ignore the type args
corresponding to unconstrained type variable.  In STEP 3 we make
polymorphic versions.  Thus:

        f@t1/ = /\b -> <f_rhs> t1 b d1 d2

We do this.

# Dictionary floating

Consider this

        f a (d::Num a) = let g = ...
                         in
                         ...(let d1::Ord a = Num.Ord.sel a d in g a d1)...

Here, g is only called at one type, but the dictionary isn't in scope at the
definition point for g.  Usually the type checker would build a
definition for d1 which enclosed g, but the transformation system
might have moved d1's defn inward.  Solution: float dictionary bindings
outwards along with call instances.

Consider

        f x = let g p q = p==q
                  h r s = (r+s, g r s)
              in
              h x x


Before specialisation, leaving out type abstractions we have

        f df x = let g :: Eq a => a -> a -> Bool
                     g dg p q = == dg p q
                     h :: Num a => a -> a -> (a, Bool)
                     h dh r s = let deq = eqFromNum dh
                                in (+ dh r s, g deq r s)
              in
              h df x x

After specialising h we get a specialised version of h, like this:

                    h' r s = let deq = eqFromNum df
                             in (+ df r s, g deq r s)

But we can't naively make an instance for g from this, because deq is not in scope
at the defn of g.  Instead, we have to float out the (new) defn of deq
to widen its scope.  Notice that this floating can't be done in advance -- it only
shows up when specialisation is done.

# SPECIALIZE pragmas

Specialisation pragmas can be digested by the type checker, and implemented
by adding extra definitions along with that of f, in the same way as before

        f@t1/t2 = <f_rhs> t1 t2 d1 d2

Indeed the pragmas *have* to be dealt with by the type checker, because
only it knows how to build the dictionaries d1 and d2!  For example

### Note: Wrap bindings returned by specImports

'specImports' returns a set of specialized bindings. However, these are lacking
necessary floated dictionary bindings, which are returned by
UsageDetails(ud_binds). These dictionaries need to be brought into scope with
'wrapDictBinds' before the bindings returned by 'specImports' can be used. See,
for instance, the 'specImports' call in 'specProgram'.

### Note: Disabling cross-module specialisation

Since GHC 7.10 we have performed specialisation of INLINABLE bindings living
in modules outside of the current module. This can sometimes uncover user code
which explodes in size when aggressively optimized. The
-fno-cross-module-specialise option was introduced to allow users to being
bitten by such instances to revert to the pre-7.10 behavior.

See Trac #10491


### Note: Warning about missed specialisations

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

### Note: Specialise imported INLINABLE things

What imported functions do we specialise?  The basic set is
 * DFuns and things with INLINABLE pragmas.
but with -fspecialise-aggressively we add
 * Anything with an unfolding template

Trac #8874 has a good example of why we want to auto-specialise DFuns.

We have the -fspecialise-aggressively flag (usually off), because we
risk lots of orphan modules from over-vigorous specialisation.
However it's not a big deal: anything non-recursive with an
unfolding-template will probably have been inlined already.

### Note: Glom the bindings if imported functions are specialised

### Note: Floating dictionaries out of cases

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

# Dealing with a binding


 Auto generated 

### Note: Account for casts in binding

Consider
   f :: Eq a => a -> IO ()
   {-# INLINABLE f
       StableUnf = (/\a \(d:Eq a) (x:a). blah) |> g
     #

### Note: Make the new dictionaries interesting

Important!  We're going to substitute dx_id1 for d
and we want it to look "interesting", else we won't gather *any*
consequential calls. E.g.
    f d = ...g d....
If we specialise f for a call (f (dfun dNumInt)), we'll get
a consequent call (g d') with an auxiliary definition
    d' = df dNumInt
We want that consequent call to look interesting

### Note: From non-recursive to recursive

Even in the non-recursive case, if any dict-binds depend on 'fn' we might
have built a recursive knot

      f a d x = <blah>
      MkUD { ud_binds = NonRec d7  (MkD ..f..)
           , ud_calls = ...(f T d7)... }

The we generate

     Rec { fs x = <blah>[T/a, d7/d]
           f a d x = <blah>
               RULE f T _ = fs
           d7 = ...f... }

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

### Note: Avoiding loops

When specialising /dictionary functions/ we must be very careful to
avoid building loops. Here is an example that bit us badly: Trac #3591

     class Eq a => C a
     instance Eq [a] => C [a]

This translates to
     dfun :: Eq [a] -> C [a]
     dfun a d = MkD a d (meth d)

     d4 :: Eq [T] = <blah>
     d2 ::  C [T] = dfun T d4
     d1 :: Eq [T] = $p1 d2
     d3 ::  C [T] = dfun T d1

None of these definitions is recursive. What happened was that we
generated a specialisation:

     RULE forall d. dfun T d = dT  :: C [T]
     dT = (MkD a d (meth d)) [T/a, d1/d]
        = MkD T d1 (meth d1)

But now we use the RULE on the RHS of d2, to get

    d2 = dT = MkD d1 (meth d1)
    d1 = $p1 d2

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
to filterCalls is in specImports (Trac #13429). Suppose we have
  class Monoid v => C v a where ...

We start with a call
   f @ [Integer] @ Integer $fC[]Integer

Specialising call to 'f' gives dict bindings
   $dMonoid_1 :: Monoid [Integer]
   $dMonoid_1 = M.$p1C @ [Integer] $fC[]Integer

   $dC_1 :: C [Integer] (Node [Integer] Integer)
   $dC_1 = M.$fCvNode @ [Integer] $dMonoid_1

...plus a recursive call to
   f @ [Integer] @ (Node [Integer] Integer) $dC_1

Specialising that call gives
   $dMonoid_2  :: Monoid [Integer]
   $dMonoid_2  = M.$p1C @ [Integer] $dC_1

   $dC_2 :: C [Integer] (Node [Integer] Integer)
   $dC_2 = M.$fCvNode @ [Integer] $dMonoid_2

Now we have two calls to the imported function
  M.$fCvNode :: Monoid v => C v a
  M.$fCvNode @v @a m = C m some_fun

But we must /not/ use the call (M.$fCvNode @ [Integer] $dMonoid_2)
for specialisation, else we get:

  $dC_1 = M.$fCvNode @ [Integer] $dMonoid_1
  $dMonoid_2 = M.$p1C @ [Integer] $dC_1
  $s$fCvNode = C $dMonoid_2 ...
    RULE M.$fCvNode [Integer] _ _ = $s$fCvNode

Now use the rule to rewrite the call in the RHS of $dC_1
and we get a loop!

--------------
Here's yet another example

  class C a where { foo,bar :: [a] -> [a] }

  instance C Int where
     foo x = r_bar x
     bar xs = reverse xs

  r_bar :: C a => [a] -> [a]
  r_bar xs = bar (xs ++ xs)

That translates to:

    r_bar a (c::C a) (xs::[a]) = bar a d (xs ++ xs)

    Rec { $fCInt :: C Int = MkC foo_help reverse
          foo_help (xs::[Int]) = r_bar Int $fCInt xs }

The call (r_bar $fCInt) mentions $fCInt,
                        which mentions foo_help,
                        which mentions r_bar
But we DO want to specialise r_bar at Int:

    Rec { $fCInt :: C Int = MkC foo_help reverse
          foo_help (xs::[Int]) = r_bar Int $fCInt xs

          r_bar a (c::C a) (xs::[a]) = bar a d (xs ++ xs)
            RULE r_bar Int _ = r_bar_Int

          r_bar_Int xs = bar Int $fCInt (xs ++ xs)
           }

Note that, because of its RULE, r_bar joins the recursive
group.  (In this case it'll unravel a short moment later.)

### Note: Specialising a recursive group

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

### Note: Specialisations already covered

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

### Note: Auto-specialisation and RULES

Consider:
   g :: Num a => a -> a
   g = ...

### Note: Floated dictionary bindings

We float out dictionary bindings for the reasons described under
"Dictionary floating" above.  But not /just/ dictionary bindings.
Consider

   f :: Eq a => blah
   f a d = rhs

   $c== :: T -> T -> Bool
   $c== x y = ...

   $df :: Eq T
   $df = Eq $c== ...

   gurgle = ...(f @T $df)...

We gather the call info for (f @T $df), and we don't want to drop it
when we come across the binding for $df.  So we add $df to the floats
and continue.  But then we have to add $c== to the floats, and so on.
These all float above the binding for 'f', and and now we can
successfully specialise 'f'.

So the DictBinds in (ud_binds :: Bag DictBind) may contain
non-dictionary bindings too.


### Note: Type determines value

Only specialise if all overloading is on non-IP *class* params,
because these are the ones whose *type* determines their *value*.  In
parrticular, with implicit params, the type args *don't* say what the
value of the implicit param is!  See Trac #7101

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
See Trac #7785.

### Note: Interesting dictionary arguments

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
massive difference in a few cases, eg Trac #5113. For nofib as a
whole it's only a small win: 2.2% improvement in allocation for ansi,
1.2% for bspt, but mostly 0.0!  Average 0.1% increase in binary size.


# \subsubsection{Boring helper functions}



                Old (but interesting) stuff about unboxed bindings
                ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


What should we do when a value is specialised to a *strict* unboxed value?

        map_*_* f (x:xs) = let h = f x
                               t = map f xs
                           in h:t

Could convert let to case:

        map_*_Int# f (x:xs) = case f x of h# ->
                              let t = map f xs
                              in h#:t

This may be undesirable since it forces evaluation here, but the value
may not be used in all branches of the body. In the general case this
transformation is impossible since the mutual recursion in a letrec
cannot be expressed as a case.

There is also a problem with top-level unboxed values, since our
implementation cannot handle unboxed values at the top level.

Solution: Lift the binding of the unboxed value and extract it when it
is used:

        map_*_Int# f (x:xs) = let h = case (f x) of h# -> _Lift h#
                                  t = map f xs
                              in case h of
                                 _Lift h# -> h#:t

Now give it to the simplifier and the _Lifting will be optimised away.

The benefit is that we have given the specialised "unboxed" values a
very simple lifted semantics and then leave it up to the simplifier to
optimise it --- knowing that the overheads will be removed in nearly
all cases.

In particular, the value will only be evaluated in the branches of the
program which use it, rather than being forced at the point where the
value is bound. For example:

        filtermap_*_* p f (x:xs)
          = let h = f x
                t = ...
            in case p x of
                True  -> h:t
                False -> t
   ==>
        filtermap_*_Int# p f (x:xs)
          = let h = case (f x) of h# -> _Lift h#
                t = ...
            in case p x of
                True  -> case h of _Lift h#
                           -> h#:t
                False -> t

The binding for h can still be inlined in the one branch and the
_Lifting eliminated.


Question: When won't the _Lifting be eliminated?

Answer: When they at the top-level (where it is necessary) or when
inlining would duplicate work (or possibly code depending on
options). However, the _Lifting will still be eliminated if the
strictness analyser deems the lifted binding strict.
