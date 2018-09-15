[[src]](https://github.com/ghc/ghc/tree/master/compiler/simplCore/SAT.hs)

(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

# 

               Static Argument Transformation pass

# 

May be seen as removing invariants from loops:
Arguments of recursive functions that do not change in recursive
calls are removed from the recursion, which is done locally
and only passes the arguments which effectively change.

Example:
map = /\ ab -> \f -> \xs -> case xs of
                 []       -> []
                 (a:b) -> f a : map f b

as map is recursively called with the same argument f (unmodified)
we transform it to

map = /\ ab -> \f -> \xs -> let map' ys = case ys of
                       []     -> []
                       (a:b) -> f a : map' b
                in map' xs

Notice that for a compiler that uses lambda lifting this is
useless as map' will be transformed back to what map was.

We could possibly do the same for big lambdas, but we don't as
they will eventually be removed in later stages of the compiler,
therefore there is no penalty in keeping them.

We only apply the SAT when the number of static args is > 2. This
produces few bad cases.  See
                should_transform
in saTransform.

Here are the headline nofib results:
                  Size    Allocs   Runtime
Min             +0.0%    -13.7%    -21.4%
Max             +0.1%     +0.0%     +5.4%
Geometric Mean  +0.0%     -0.2%     -6.9%

The previous patch, to fix polymorphic floatout demand signatures, is
essential to make this work well!



pprIdSATInfo id_sat_info = vcat (map pprIdAndSATInfo (Map.toList id_sat_info))
  where pprIdAndSATInfo (v, sat_info) = hang (ppr v <> colon) 4 (pprSATInfo sat_info)


# 

                Static Argument Transformation Monad

# 

# 

                Static Argument Transformation Monad

# 

To do the transformation, the game plan is to:

1. Create a small nonrecursive RHS that takes the
   original arguments to the function but discards
   the ones that are static and makes a call to the
   SATed version with the remainder. We intend that
   this will be inlined later, removing the overhead

2. Bind this nonrecursive RHS over the original body
   WITH THE SAME UNIQUE as the original body so that
   any recursive calls to the original now go via
   the small wrapper

3. Rebind the original function to a new one which contains
   our SATed function and just makes a call to it:
   we call the thing making this call the local body

Example: transform this

    map :: forall a b. (a->b) -> [a] -> [b]
    map = /\ab. \(f:a->b) (as:[a]) -> body[map]
to
    map :: forall a b. (a->b) -> [a] -> [b]
    map = /\ab. \(f:a->b) (as:[a]) ->
         letrec map' :: [a] -> [b]
                    -- The "worker function
                map' = \(as:[a]) ->
                         let map :: forall a' b'. (a -> b) -> [a] -> [b]
                                -- The "shadow function
                             map = /\a'b'. \(f':(a->b) (as:[a]).
                                   map' as
                         in body[map]
         in map' as

### Note: Shadow binding

The calls to the inner map inside body[map] should get inlined
by the local re-binding of 'map'.  We call this the "shadow binding".

But we can't use the original binder 'map' unchanged, because
it might be exported, in which case the shadow binding won't be
discarded as dead code after it is inlined.

So we use a hack: we make a new SysLocal binder with the *same* unique
as binder.  (Another alternative would be to reset the export flag.)

### Note: Binder type capture

Notice that in the inner map (the "shadow function"), the static arguments
are discarded -- it's as if they were underscores.  Instead, mentions
of these arguments (notably in the types of dynamic arguments) are bound
by the *outer* lambdas of the main function.  So we must make up fresh
names for the static arguments so that they do not capture variables
mentioned in the types of dynamic args.

In the map example, the shadow function must clone the static type
argument a,b, giving a',b', to ensure that in the \(as:[a]), the 'a'
is bound by the outer forall.  We clone f' too for consistency, but
that doesn't matter either way because static Id arguments aren't
mentioned in the shadow binding at all.

If we don't we get something like this:

[Exported]
[Arity 3]
GHC.Base.until =
  \ (@ a_aiK)
    (p_a6T :: a_aiK -> GHC.Types.Bool)
    (f_a6V :: a_aiK -> a_aiK)
    (x_a6X :: a_aiK) ->
    letrec {
      sat_worker_s1aU :: a_aiK -> a_aiK
      []
      sat_worker_s1aU =
        \ (x_a6X :: a_aiK) ->
          let {
            sat_shadow_r17 :: forall a_a3O.
                              (a_a3O -> GHC.Types.Bool) -> (a_a3O -> a_a3O) -> a_a3O -> a_a3O
            []
            sat_shadow_r17 =
              \ (@ a_aiK)
                (p_a6T :: a_aiK -> GHC.Types.Bool)
                (f_a6V :: a_aiK -> a_aiK)
                (x_a6X :: a_aiK) ->
                sat_worker_s1aU x_a6X } in
          case p_a6T x_a6X of wild_X3y [ALWAYS Dead Nothing] {
            GHC.Types.False -> GHC.Base.until @ a_aiK p_a6T f_a6V (f_a6V x_a6X);
            GHC.Types.True -> x_a6X
          }; } in
    sat_worker_s1aU x_a6X

Where sat_shadow has captured the type variables of x_a6X etc as it has a a_aiK
type argument. This is bad because it means the application sat_worker_s1aU x_a6X
is not well typed.
