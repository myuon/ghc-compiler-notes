`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/simplCore/SAT.hs>`_

compiler/simplCore/SAT.hs
=========================


Note [Shadow binding]
~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/simplCore/SAT.hs#L306>`__

The calls to the inner map inside body[map] should get inlined
by the local re-binding of 'map'.  We call this the "shadow binding".

But we can't use the original binder 'map' unchanged, because
it might be exported, in which case the shadow binding won't be
discarded as dead code after it is inlined.

So we use a hack: we make a new SysLocal binder with the *same* unique
as binder.  (Another alternative would be to reset the export flag.)



Note [Binder type capture]
~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/simplCore/SAT.hs#L318>`__

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

