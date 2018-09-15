[[src]](https://github.com/ghc/ghc/tree/master/compiler/specialise/SpecConstr.hs)
# ToDo [Oct 2013]

1. Nuke ForceSpecConstr for good (it is subsumed by GHC.Types.SPEC in ghc-prim)
2. Nuke NoSpecConstr


(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

# Specialise over constructors


-----------------------------------------------------
                        Game plan
-----------------------------------------------------

Consider
        drop n []     = []
        drop 0 xs     = []
        drop n (x:xs) = drop (n-1) xs

After the first time round, we could pass n unboxed.  This happens in
numerical code too.  Here's what it looks like in Core:

        drop n xs = case xs of
                      []     -> []
                      (y:ys) -> case n of
                                  I# n# -> case n# of
                                             0 -> []
                                             _ -> drop (I# (n# -# 1#)) xs

Notice that the recursive call has an explicit constructor as argument.
Noticing this, we can make a specialised version of drop

        RULE: drop (I# n#) xs ==> drop' n# xs

        drop' n# xs = let n = I# n# in ...orig RHS...

Now the simplifier will apply the specialisation in the rhs of drop', giving

        drop' n# xs = case xs of
                      []     -> []
                      (y:ys) -> case n# of
                                  0 -> []
                                  _ -> drop' (n# -# 1#) xs

Much better!

We'd also like to catch cases where a parameter is carried along unchanged,
but evaluated each time round the loop:

        f i n = if i>0 || i>n then i else f (i*2) n

Here f isn't strict in n, but we'd like to avoid evaluating it each iteration.
In Core, by the time we've w/wd (f is strict in i) we get

        f i# n = case i# ># 0 of
                   False -> I# i#
                   True  -> case n of { I# n# ->
                            case i# ># n# of
                                False -> I# i#
                                True  -> f (i# *# 2#) n

At the call to f, we see that the argument, n is known to be (I# n#),
and n is evaluated elsewhere in the body of f, so we can play the same
trick as above.

### Note: Reboxing

We must be careful not to allocate the same constructor twice.  Consider
        f p = (...(case p of (a,b) -> e)...p...,
               ...let t = (r,s) in ...t...(f t)...)
At the recursive call to f, we can see that t is a pair.  But we do NOT want
to make a specialised copy:
        f' a b = let p = (a,b) in (..., ...)
because now t is allocated by the caller, then r and s are passed to the
recursive call, which allocates the (r,s) pair again.

This happens if
  (a) the argument p is used in other than a case-scrutinisation way.
  (b) the argument to the call is not a 'fresh' tuple; you have to
        look into its unfolding to see that it's a tuple

### Note: Good arguments

ALTERNATIVE 2: pass both boxed and unboxed versions.  This no longer saves
allocation, but does perhaps save evals. In the RULE we'd have
something like

  f (I# x#) = f' (I# x#) x#

If at the call site the (I# x) was an unfolding, then we'd have to
rely on CSE to eliminate the duplicate allocation.... This alternative
doesn't look attractive enough to pursue.

ALTERNATIVE 3: ignore the reboxing problem.  The trouble is that
the conservative reboxing story prevents many useful functions from being
specialised.  Example:
        foo :: Maybe Int -> Int -> Int
        foo   (Just m) 0 = 0
        foo x@(Just m) n = foo x (n-m)
Here the use of 'x' will clearly not require boxing in the specialised function.

The strictness analyser has the same problem, in fact.  Example:
        f p@(a,b) = ...
If we pass just 'a' and 'b' to the worker, it might need to rebox the
pair to create (a,b).  A more sophisticated analysis might figure out
precisely the cases in which this could happen, but the strictness
analyser does no such analysis; it just passes 'a' and 'b', and hopes
for the best.

So my current choice is to make SpecConstr similarly aggressive, and
ignore the bad potential of reboxing.

### Note: Good arguments

So we look for

* A self-recursive function.  Ignore mutual recursion for now,
  because it's less common, and the code is simpler for self-recursion.

* EITHER

   a) At a recursive call, one or more parameters is an explicit
      constructor application
        AND
      That same parameter is scrutinised by a case somewhere in
      the RHS of the function

  OR

### Note: Reboxing

# What to abstract over

There's a bit of a complication with type arguments.  If the call
site looks like

        f p = ...f ((:) [a] x xs)...

then our specialised function look like

        f_spec x xs = let p = (:) [a] x xs in ....as before....

This only makes sense if either
  a) the type variable 'a' is in scope at the top of f, or
  b) the type variable 'a' is an argument to f (and hence fs)

Actually, (a) may hold for value arguments too, in which case
we may not want to pass them.  Suppose 'x' is in scope at f's
defn, but xs is not.  Then we'd like

        f_spec xs = let p = (:) [a] x xs in ....as before....

Similarly (b) may hold too.  If x is already an argument at the
call, no need to pass it again.

Finally, if 'a' is not in scope at the call site, we could abstract
it as we do the term variables:

        f_spec a x xs = let p = (:) [a] x xs in ...as before...

So the grand plan is:

        * abstract the call site to a constructor-only pattern
          e.g.  C x (D (f p) (g q))  ==>  C s1 (D s2 s3)

        * Find the free variables of the abstracted pattern

### Note: Shadowing


NOTICE that we only abstract over variables that are not in scope,
so we're in no danger of shadowing variables used in "higher up"
in f_spec's RHS.

### Note: Shadowing

In this pass we gather up usage information that may mention variables
that are bound between the usage site and the definition site; or (more
seriously) may be bound to something different at the definition site.
For example:

        f x = letrec g y v = let x = ...
                             in ...(g (a,b) x)...

Since 'x' is in scope at the call site, we may make a rewrite rule that
looks like
        RULE forall a,b. g (a,b) x = ...
But this rule will never match, because it's really a different 'x' at
the call site -- and that difference will be manifest by the time the
simplifier gets to it.  [A worry: the simplifier doesn't *guarantee*
no-shadowing, so perhaps it may not be distinct?]

Anyway, the rule isn't actually wrong, it's just not useful.  One possibility
is to run deShadowBinds before running SpecConstr, but instead we run the
simplifier.  That gives the simplest possible program for SpecConstr to
chew on; and it virtually guarantees no shadowing.

### Note: Specialising for constant parameters

This one is about specialising on a *constant* (but not necessarily
constructor) argument

    foo :: Int -> (Int -> Int) -> Int
    foo 0 f = 0
    foo m f = foo (f m) (+1)

It produces

    lvl_rmV :: GHC.Base.Int -> GHC.Base.Int
    lvl_rmV =
      \ (ds_dlk :: GHC.Base.Int) ->
        case ds_dlk of wild_alH { GHC.Base.I# x_alG ->
        GHC.Base.I# (GHC.Prim.+# x_alG 1)

    T.$wfoo :: GHC.Prim.Int# -> (GHC.Base.Int -> GHC.Base.Int) ->
    GHC.Prim.Int#
    T.$wfoo =
      \ (ww_sme :: GHC.Prim.Int#) (w_smg :: GHC.Base.Int -> GHC.Base.Int) ->
        case ww_sme of ds_Xlw {
          __DEFAULT ->
        case w_smg (GHC.Base.I# ds_Xlw) of w1_Xmo { GHC.Base.I# ww1_Xmz ->
        T.$wfoo ww1_Xmz lvl_rmV
        };
          0 -> 0
        }

The recursive call has lvl_rmV as its argument, so we could create a specialised copy
with that argument baked in; that is, not passed at all.   Now it can perhaps be inlined.

When is this worth it?  Call the constant 'lvl'
- If 'lvl' has an unfolding that is a constructor, see if the corresponding
  parameter is scrutinised anywhere in the body.

- If 'lvl' has an unfolding that is a inlinable function, see if the corresponding
  parameter is applied (...to enough arguments...?)

  Also do this is if the function has RULES?

Also

### Note: Specialising for lambda parameters

    foo :: Int -> (Int -> Int) -> Int
    foo 0 f = 0
    foo m f = foo (f m) (\n -> n-m)

This is subtly different from the previous one in that we get an
explicit lambda as the argument:

    T.$wfoo :: GHC.Prim.Int# -> (GHC.Base.Int -> GHC.Base.Int) ->
    GHC.Prim.Int#
    T.$wfoo =
      \ (ww_sm8 :: GHC.Prim.Int#) (w_sma :: GHC.Base.Int -> GHC.Base.Int) ->
        case ww_sm8 of ds_Xlr {
          __DEFAULT ->
        case w_sma (GHC.Base.I# ds_Xlr) of w1_Xmf { GHC.Base.I# ww1_Xmq ->
        T.$wfoo
          ww1_Xmq
          (\ (n_ad3 :: GHC.Base.Int) ->
             case n_ad3 of wild_alB { GHC.Base.I# x_alA ->
             GHC.Base.I# (GHC.Prim.-# x_alA ds_Xlr)
             })
        };
          0 -> 0
        }

I wonder if SpecConstr couldn't be extended to handle this? After all,
lambda is a sort of constructor for functions and perhaps it already
has most of the necessary machinery?

Furthermore, there's an immediate win, because you don't need to allocate the lambda
at the call site; and if perchance it's called in the recursive call, then you
may avoid allocating it altogether.  Just like for constructors.

Looks cool, but probably rare...but it might be easy to implement.

### Note: SpecConstr for casts

Consider
    data family T a :: *
    data instance T Int = T Int

    foo n = ...
       where
         go (T 0) = 0
         go (T n) = go (T (n-1))

The recursive call ends up looking like
        go (T (I# ...) `cast` g)
So we want to spot the constructor application inside the cast.
That's why we have the Cast case in argToPat

### Note: Local recursive groups

For a *local* recursive group, we can see all the calls to the
function, so we seed the specialisation loop from the calls in the
body, not from the calls in the RHS.  Consider:

  bar m n = foo n (n,n) (n,n) (n,n) (n,n)
   where
     foo n p q r s
       | n == 0    = m
       | n > 3000  = case p of { (p1,p2) -> foo (n-1) (p2,p1) q r s }
       | n > 2000  = case q of { (q1,q2) -> foo (n-1) p (q2,q1) r s }
       | n > 1000  = case r of { (r1,r2) -> foo (n-1) p q (r2,r1) s }
       | otherwise = case s of { (s1,s2) -> foo (n-1) p q r (s2,s1) }

If we start with the RHSs of 'foo', we get lots and lots of specialisations,
most of which are not needed.  But if we start with the (single) call
in the rhs of 'bar' we get exactly one fully-specialised copy, and all
the recursive calls go to this fully-specialised copy. Indeed, the original
function is later collected as dead code.  This is very important in
specialising the loops arising from stream fusion, for example in NDP where
we were getting literally hundreds of (mostly unused) specialisations of
a local function.

In a case like the above we end up never calling the original un-specialised
function.  (Although we still leave its code around just in case.)

However, if we find any boring calls in the body, including *unsaturated*
ones, such as
      letrec foo x y = ....foo...
      in map foo xs
then we will end up calling the un-specialised function, so then we *should*
use the calls in the un-specialised RHS as seeds.  We call these
"boring call patterns", and callsToPats reports if it finds any of these.

### Note: Seeding top-level recursive groups

This seeding is done in the binding for seed_calls in specRec.

### Note: Local recursive groups

2. But if any of the bindings are exported, the function may be called
   with any old arguments, so (for lack of anything better) we specialise
   based on
     (a) the call patterns in the RHS
     (b) the call patterns in the rest of the top-level bindings
   NB: before Apr 15 we used (a) only, but Dimitrios had an example
       where (b) was crucial, so I added that.
       Adding (b) also improved nofib allocation results:
                  multiplier: 4%   better
                  minimax:    2.8% better

Actually in case (2), instead of using the calls from the RHS, it
would be better to specialise in the importing module.  We'd need to
add an INLINABLE pragma to the function, and then it can be
specialised in the importing scope, just as is done for type classes
in Specialise.specImports. This remains to be done (#10346).

### Note: Top-level recursive groups

### Note: Seeding top-level recursive groups

### Note: Do not specialise diverging functions

# \subsection{Environment: goes downwards}


### Note: Work-free values only in environment

The sc_vals field keeps track of in-scope value bindings, so
that if we come across (case x of Just y ->...) we can reduce the
case from knowing that x is bound to a pair.

But only *work-free* values are ok here. For example if the envt had
    x -> Just (expensive v)
then we do NOT want to expand to
     let y = expensive v in ...
because the x-binding still exists and we've now duplicated (expensive v).

This seldom happens because let-bound constructor applications are
ANF-ised, but it can happen as a result of on-the-fly transformations in
SpecConstr itself.  Here is Trac #7865:

        let {
          a'_shr =
            case xs_af8 of _ {
              [] -> acc_af6;
              : ds_dgt [Dmd=<L,A>] ds_dgu [Dmd=<L,A>] ->
                (expensive x_af7, x_af7
            } } in
        let {
          ds_sht =
            case a'_shr of _ { (p'_afd, q'_afe) ->
            TSpecConstr_DoubleInline.recursive
              (GHC.Types.: @ GHC.Types.Int x_af7 wild_X6) (q'_afe, p'_afd)
            } } in

When processed knowing that xs_af8 was bound to a cons, we simplify to
   a'_shr = (expensive x_af7, x_af7)
and we do NOT want to inline that at the occurrence of a'_shr in ds_sht.
(There are other occurrences of a'_shr.)  No no no.

It would be possible to do some on-the-fly ANF-ising, so that a'_shr turned
into a work-free value again, thus
   a1 = expensive x_af7
   a'_shr = (a1, x_af7)
but that's more work, so until its shown to be important I'm going to
leave it for now.

### Note: Making SpecConstr keener

Consider this, in (perf/should_run/T9339)
   last (filter odd [1..1000])

After optimisation, including SpecConstr, we get:
   f :: Int# -> Int -> Int
   f x y = case case remInt# x 2# of
             __DEFAULT -> case x of
                            __DEFAULT -> f (+# wild_Xp 1#) (I# x)
                            1000000# -> ...
             0# -> case x of
                     __DEFAULT -> f (+# wild_Xp 1#) y
                    1000000#   -> y

Not good!  We build an (I# x) box every time around the loop.
SpecConstr (as described in the paper) does not specialise f, despite
the call (f ... (I# x)) because 'y' is not scrutinied in the body.
But it is much better to specialise f for the case where the argument
is of form (I# x); then we build the box only when returning y, which
is on the cold path.

Another example:

   f x = ...(g x)....

Here 'x' is not scrutinised in f's body; but if we did specialise 'f'
then the call (g x) might allow 'g' to be specialised in turn.

So sc_keen controls whether or not we take account of whether argument is
scrutinised in the body.  True <=> ignore that, and speicalise whenever
the function is applied to a data constructor.


### Note: Add scrutinee to ValueEnv too

Consider this:
   case x of y
     (a,b) -> case b of c
                I# v -> ...(f y)...
By the time we get to the call (f y), the ValueEnv
will have a binding for y, and for c
    y -> (a,b)
    c -> I# v
BUT that's not enough!  Looking at the call (f y) we
see that y is pair (a,b), but we also need to know what 'b' is.
So in extendCaseBndrs we must *also* add the binding
   b -> I# v
else we lose a useful specialisation for f.  This is necessary even
though the simplifier has systematically replaced uses of 'x' with 'y'
and 'b' with 'c' in the code.  The use of 'b' in the ValueEnv came
from outside the case.  See Trac #4908 for the live example.

### Note: Avoiding exponential blowup

The sc_count field of the ScEnv says how many times we are prepared to
duplicate a single function.  But we must take care with recursive
specialisations.  Consider

        let $j1 = let $j2 = let $j3 = ...
                            in
                            ...$j3...
                  in
                  ...$j2...
        in
        ...$j1...

If we specialise $j1 then in each specialisation (as well as the original)
we can specialise $j2, and similarly $j3.  Even if we make just *one*
specialisation of each, because we also have the original we'll get 2^n
copies of $j3, which is not good.

So when recursively specialising we divide the sc_count by the number of
copies we are making at this level, including the original.

# \subsection{Usage information: flows upwards}


# Note  [ScrutOcc]

An occurrence of ScrutOcc indicates that the thing, or a `cast` version of the thing,
is *only* taken apart or applied.

  Functions, literal: ScrutOcc emptyUFM
  Data constructors:  ScrutOcc subs,

where (subs :: UniqFM [ArgOcc]) gives usage of the *pattern-bound* components,
The domain of the UniqFM is the Unique of the data constructor

The [ArgOcc] is the occurrences of the *pattern-bound* components
of the data structure.  E.g.
        data T a = forall b. MkT a b (b->a)
A pattern binds b, x::a, y::b, z::b->a, but not 'a'!



# \subsection{The main recursive function}


The main recursive function gathers up usage information, and
creates specialised versions of functions.


### Note: Local let bindings

It is not uncommon to find this

   let $j = \x. <blah> in ...$j True...$j True...

Here $j is an arbitrary let-bound function, but it often comes up for
join points.  We might like to specialise $j for its call patterns.
Notice the difference from a letrec, where we look for call patterns
in the *RHS* of the function.  Here we look for call patterns in the
*body* of the let.

At one point I predicated this on the RHS mentioning the outer
recursive function, but that's not essential and might even be
harmful.  I'm not sure.



scTopBind _ usage _
  | pprTrace "scTopBind_usage" (ppr (scu_calls usage)) False
  = error "false"


# The specialiser itself



  Example

     In-scope: a, x::a
     f = /\b \y::[(a,b)] -> ....f (b,c) ((:) (a,(b,c)) (x,v) (h w))...
          [c::*, v::(b,c) are presumably bound by the (...) part]
  ==>
     f_spec = /\ b c \ v::(b,c) hw::[(a,(b,c))] ->
                  (...entire body of f...) [b -> (b,c),
                                            y -> ((:) (a,(b,c)) (x,v) hw)]

     RULE:  forall b::* c::*,           -- Note, *not* forall a, x
                   v::(b,c),
                   hw::[(a,(b,c))] .

            f (b,c) ((:) (a,(b,c)) (x,v) hw) = f_spec b c v hw


 Auto 

 Local 

### Note: spec_usg includes rhs_usg

In calls to 'specialise', the returned ScUsage must include the rhs_usg in
the passed-in SpecInfo, unless there are no calls at all to the function.

The caller can, indeed must, assume this.  He should not combine in rhs_usg
himself, or he'll get rhs_usg twice -- and that can lead to an exponential
blowup of duplicates in the CallEnv.  This is what gave rise to the massive
performance loss in Trac #8852.

### Note: Specialise original body

The RhsInfo for a binding keeps the *original* body of the binding.  We
must specialise that, *not* the result of applying specExpr to the RHS
(which is also kept in RhsInfo). Otherwise we end up specialising a
specialised RHS, and that can lead directly to exponential behaviour.

### Note: Transfer activation

### Note: Auto-specialisation and RULES

In which phase should the specialise-constructor rules be active?
Originally I made them always-active, but Manuel found that this
defeated some clever user-written rules.  Then I made them active only
in Phase 0; after all, currently, the specConstr transformation is
only run after the simplifier has reached Phase 0, but that meant
that specialisations didn't fire inside wrappers; see test
simplCore/should_compile/spec-inline.

So now I just use the inline-activation of the parent Id, as the
activation for the specialisation RULE, just like the main specialiser;

This in turn means there is no point in specialising NOINLINE things,
so we test for that.

### Note: Transfer strictness

We must transfer strictness information from the original function to
the specialised one.  Suppose, for example

  f has strictness     SS
        and a RULE     f (a:as) b = f_spec a as b

Now we want f_spec to have strictness  LLS, otherwise we'll use call-by-need
when calling f_spec instead of call-by-value.  And that can result in
unbounded worsening in space (cf the classic foldl vs foldl')

See Trac #3437 for a good example.

The function calcSpecStrictness performs the calculation.

### Note: Strictness information in worker binders


After having calculated the strictness annotation for the worker (see Note
[Transfer strictness] above), we also want to have this information attached to
the worker’s arguments, for the benefit of later passes. The function
handOutStrictnessInformation decomposes the strictness annotation calculated by
calcSpecStrictness and attaches them to the variables.

# \subsection{Argument analysis}


This code deals with analysing call-site arguments to see whether
they are constructor applications.

### Note: Free type variables of the qvar types

In a call (f @a x True), that we want to specialise, what variables should
we quantify over.  Clearly over 'a' and 'x', but what about any type variables
free in x's type?  In fact we don't need to worry about them because (f @a)
can only be a well-typed application if its type is compatible with x, so any
variables free in x's type must be free in (f @a), and hence either be gathered
via 'a' itself, or be in scope at f's defn.  Hence we just take
  (exprsFreeVars pats).

BUT phantom type synonyms can mess this reasoning up,
  eg   x::T b   with  type T b = Int
So we apply expandTypeSynonyms to the bound Ids.
See Trac # 5458.  Yuk.

### Note: SpecConstr call patterns

A "call patterns" that we collect is going to become the LHS of a RULE.
It's important that it doesn't have
     e |> Refl
or
    e |> g1 |> g2
because both of these will be optimised by Simplify.simplRule. In the
former case such optimisation benign, because the rule will match more
terms; but in the latter we may lose a binding of 'g1' or 'g2', and
end up with a rule LHS that doesn't bind the template variables
(Trac #10602).

The simplifier eliminates such things, but SpecConstr itself constructs
new terms by substituting.  So the 'mkCast' in the Cast case of scExpr
is very important!

### Note: Choosing patterns

If we get lots of patterns we may not want to make a specialisation
for each of them (code bloat), so we choose as follows, implemented
by trim_pats.

* The flag -fspec-constr-count-N sets the sc_count field
  of the ScEnv to (Just n).  This limits the total number
  of specialisations for a given function to N.

* -fno-spec-constr-count sets the sc_count field to Nothing,
  which switches of the limit.

* The ghastly ForceSpecConstr trick also switches of the limit
  for a particular function

* Otherwise we sort the patterns to choose the most general
  ones first; more general => more widely applicable.

### Note: SpecConstr and casts

Consider (Trac #14270) a call like

    let f = e
    in ... f (K @(a |> co)) ...

where 'co' is a coercion variable not in scope at f's definition site.
If we aren't caereful we'll get

    let $sf a co = e (K @(a |> co))
        RULE "SC:f" forall a co.  f (K @(a |> co)) = $sf a co
        f = e
    in ...

But alas, when we match the call we won't bind 'co', because type-matching
(for good reasons) discards casts).

I don't know how to solve this, so for now I'm just discarding any
call patterns that
  * Mentions a coercion variable in a type argument
  * That is not in scope at the binding of the function

I think this is very rare.

It is important (e.g. Trac #14936) that this /only/ applies to
coercions mentioned in casts.  We don't want to be discombobulated
by casts in terms!  For example, consider
   f ((e1,e2) |> sym co)
where, say,
   f  :: Foo -> blah
   co :: Foo ~R (Int,Int)

Here we definitely do want to specialise for that pair!  We do not
match on the structre of the coercion; instead we just match on a
coercion variable, so the RULE looks like

   forall (x::Int, y::Int, co :: (Int,Int) ~R Foo)
     f ((x,y) |> co) = $sf x y co

Often the body of f looks like
   f arg = ...(case arg |> co' of
                (x,y) -> blah)...

so that the specialised f will turn into
   $sf x y co = let arg = (x,y) |> co
                in ...(case arg>| co' of
                         (x,y) -> blah)....

which will simplify to not use 'co' at all.  But we can't guarantee
that co will end up unused, so we still pass it.  Absence analysis
may remove it later.

Note that this /also/ discards the call pattern if we have a cast in a
/term/, although in fact Rules.match does make a very flaky and
fragile attempt to match coercions.  e.g. a call like
    f (Maybe Age) (Nothing |> co) blah
    where co :: Maybe Int ~ Maybe Age
will be discarded.  It's extremely fragile to match on the form of a
coercion, so I think it's better just not to try.  A more complicated
alternative would be to discard calls that mention coercion variables
only in kind-casts, but I'm doing the simple thing for now.


### Note: Matching cases

      Disabling lambda specialisation for now
        It's fragile, and the spec_loop can be infinite
argToPat in_scope val_env arg arg_occ
  | is_value_lam arg
  = return (True, arg)
  where
    is_value_lam (Lam v e)         -- Spot a value lambda, even if
        | isId v       = True      -- it is inside a type lambda
        | otherwise    = is_value_lam e
    is_value_lam other = False


### Note: Ignore type differences

We do not want to generate specialisations where the call patterns
differ only in their type arguments!  Not only is it utterly useless,
but it also means that (with polymorphic recursion) we can generate
an infinite number of specialisations. Example is Data.Sequence.adjustTree,
I think.
