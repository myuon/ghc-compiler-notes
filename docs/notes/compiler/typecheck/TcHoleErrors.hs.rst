`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcHoleErrors.hs>`_

Note [Valid hole fits include ...]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
`findValidHoleFits` returns the "Valid hole fits include ..." message.
For example, look at the following definitions in a file called test.hs:

   import Data.List (inits)

   f :: [String]
   f = _ "hello, world"

The hole in `f` would generate the message:

  • Found hole: _ :: [Char] -> [String]
  • In the expression: _
    In the expression: _ "hello, world"
    In an equation for ‘f’: f = _ "hello, world"
  • Relevant bindings include f :: [String] (bound at test.hs:6:1)
    Valid hole fits include
      lines :: String -> [String]
        (imported from ‘Prelude’ at mpt.hs:3:8-9
          (and originally defined in ‘base-4.11.0.0:Data.OldList’))
      words :: String -> [String]
        (imported from ‘Prelude’ at mpt.hs:3:8-9
          (and originally defined in ‘base-4.11.0.0:Data.OldList’))
      inits :: forall a. [a] -> [[a]]
        with inits @Char
        (imported from ‘Data.List’ at mpt.hs:4:19-23
          (and originally defined in ‘base-4.11.0.0:Data.OldList’))
      repeat :: forall a. a -> [a]
        with repeat @String
        (imported from ‘Prelude’ at mpt.hs:3:8-9
          (and originally defined in ‘GHC.List’))
      fail :: forall (m :: * -> *). Monad m => forall a. String -> m a
        with fail @[] @String
        (imported from ‘Prelude’ at mpt.hs:3:8-9
          (and originally defined in ‘GHC.Base’))
      return :: forall (m :: * -> *). Monad m => forall a. a -> m a
        with return @[] @String
        (imported from ‘Prelude’ at mpt.hs:3:8-9
          (and originally defined in ‘GHC.Base’))
      pure :: forall (f :: * -> *). Applicative f => forall a. a -> f a
        with pure @[] @String
        (imported from ‘Prelude’ at mpt.hs:3:8-9
          (and originally defined in ‘GHC.Base’))
      read :: forall a. Read a => String -> a
        with read @[String]
        (imported from ‘Prelude’ at mpt.hs:3:8-9
          (and originally defined in ‘Text.Read’))
      mempty :: forall a. Monoid a => a
        with mempty @([Char] -> [String])
        (imported from ‘Prelude’ at mpt.hs:3:8-9
          (and originally defined in ‘GHC.Base’))

Valid hole fits are found by checking top level identifiers and local bindings
in scope for whether their type can be instantiated to the the type of the hole.
Additionally, we also need to check whether all relevant constraints are solved
by choosing an identifier of that type as well, see Note [Relevant Constraints]

Since checking for subsumption results in the side-effect of type variables
being unified by the simplifier, we need to take care to restore them after
to being flexible type variables after we've checked for subsumption.
This is to avoid affecting the hole and later checks by prematurely having
unified one of the free unification variables.

When outputting, we sort the hole fits by the size of the types we'd need to
apply by type application to the type of the fit to to make it fit. This is done
in order to display "more relevant" suggestions first. Another option is to
sort by building a subsumption graph of fits, i.e. a graph of which fits subsume
what other fits, and then outputting those fits which are are subsumed by other
fits (i.e. those more specific than other fits) first. This results in the ones
"closest" to the type of the hole to be displayed first.

To help users understand how the suggested fit works, we also display the values
that the quantified type variables would take if that fit is used, like
`mempty @([Char] -> [String])` and `pure @[] @String` in the example above.
If -XTypeApplications is enabled, this can even be copied verbatim as a
replacement for the hole.




Note [Nested implications]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

For the simplifier to be able to use any givens present in the enclosing
implications to solve relevant constraints, we nest the wanted subsumption
constraints and relevant constraints within the enclosing implications.

As an example, let's look at the following code:

  f :: Show a => a -> String
  f x = show _

The hole will result in the hole constraint:

  [WD] __a1ph {0}:: a0_a1pd[tau:2] (CHoleCan: ExprHole(_))

Here the nested implications are just one level deep, namely:

  [Implic {
      TcLevel = 2
      Skolems = a_a1pa[sk:2]
      No-eqs = True
      Status = Unsolved
      Given = $dShow_a1pc :: Show a_a1pa[sk:2]
      Wanted =
        WC {wc_simple =
              [WD] __a1ph {0}:: a_a1pd[tau:2] (CHoleCan: ExprHole(_))
              [WD] $dShow_a1pe {0}:: Show a_a1pd[tau:2] (CDictCan(psc))}
      Binds = EvBindsVar<a1pi>
      Needed inner = []
      Needed outer = []
      the type signature for:
        f :: forall a. Show a => a -> String }]

As we can see, the givens say that the information about the skolem
`a_a1pa[sk:2]` fulfills the Show constraint.

The simples are:

  [[WD] __a1ph {0}:: a0_a1pd[tau:2] (CHoleCan: ExprHole(_)),
    [WD] $dShow_a1pe {0}:: Show a0_a1pd[tau:2] (CNonCanonical)]

I.e. the hole `a0_a1pd[tau:2]` and the constraint that the type of the hole must
fulfill `Show a0_a1pd[tau:2])`.

So when we run the check, we need to make sure that the

  [WD] $dShow_a1pe {0}:: Show a0_a1pd[tau:2] (CNonCanonical)

Constraint gets solved. When we now check for whether `x :: a0_a1pd[tau:2]` fits
the hole in `tcCheckHoleFit`, the call to `tcSubType` will end up writing the
meta type variable `a0_a1pd[tau:2] := a_a1pa[sk:2]`. By wrapping the wanted
constraints needed by tcSubType_NC and the relevant constraints (see
Note [Relevant Constraints] for more details) in the nested implications, we
can pass the information in the givens along to the simplifier. For our example,
we end up needing to check whether the following constraints are soluble.

  WC {wc_impl =
        Implic {
          TcLevel = 2
          Skolems = a_a1pa[sk:2]
          No-eqs = True
          Status = Unsolved
          Given = $dShow_a1pc :: Show a_a1pa[sk:2]
          Wanted =
            WC {wc_simple =
                  [WD] $dShow_a1pe {0}:: Show a0_a1pd[tau:2] (CNonCanonical)}
          Binds = EvBindsVar<a1pl>
          Needed inner = []
          Needed outer = []
          the type signature for:
            f :: forall a. Show a => a -> String }}

But since `a0_a1pd[tau:2] := a_a1pa[sk:2]` and we have from the nested
implications that Show a_a1pa[sk:2] is a given, this is trivial, and we end up
with a final WC of WC {}, confirming x :: a0_a1pd[tau:2] as a match.

To avoid side-effects on the nested implications, we create a new EvBindsVar so
that any changes to the ev binds during a check remains localised to that check.




Note [Valid refinement hole fits include ...]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When the `-frefinement-level-hole-fits=N` flag is given, we additionally look
for "valid refinement hole fits"", i.e. valid hole fits with up to N
additional holes in them.

With `-frefinement-level-hole-fits=0` (the default), GHC will find all
identifiers 'f' (top-level or nested) that will fit in the hole.

With `-frefinement-level-hole-fits=1`, GHC will additionally find all
applications 'f _' that will fit in the hole, where 'f' is an in-scope
identifier, applied to single argument.  It will also report the type of the
needed argument (a new hole).

And similarly as the number of arguments increases

As an example, let's look at the following code:

  f :: [Integer] -> Integer
  f = _

with `-frefinement-level-hole-fits=1`, we'd get:

  Valid refinement hole fits include

    foldl1 (_ :: Integer -> Integer -> Integer)
      with foldl1 @[] @Integer
      where foldl1 :: forall (t :: * -> *).
                      Foldable t =>
                      forall a. (a -> a -> a) -> t a -> a
    foldr1 (_ :: Integer -> Integer -> Integer)
      with foldr1 @[] @Integer
      where foldr1 :: forall (t :: * -> *).
                      Foldable t =>
                      forall a. (a -> a -> a) -> t a -> a
    const (_ :: Integer)
      with const @Integer @[Integer]
      where const :: forall a b. a -> b -> a
    ($) (_ :: [Integer] -> Integer)
      with ($) @'GHC.Types.LiftedRep @[Integer] @Integer
      where ($) :: forall a b. (a -> b) -> a -> b
    fail (_ :: String)
      with fail @((->) [Integer]) @Integer
      where fail :: forall (m :: * -> *).
                    Monad m =>
                    forall a. String -> m a
    return (_ :: Integer)
      with return @((->) [Integer]) @Integer
      where return :: forall (m :: * -> *). Monad m => forall a. a -> m a
    (Some refinement hole fits suppressed;
      use -fmax-refinement-hole-fits=N or -fno-max-refinement-hole-fits)

Which are hole fits with holes in them. This allows e.g. beginners to
discover the fold functions and similar, but also allows for advanced users
to figure out the valid functions in the Free monad, e.g.

  instance Functor f => Monad (Free f) where
      Pure a >>= f = f a
      Free f >>= g = Free (fmap _a f)

Will output (with -frefinment-level-hole-fits=1):
    Found hole: _a :: Free f a -> Free f b
          Where: ‘a’, ‘b’ are rigid type variables bound by
                  the type signature for:
                    (>>=) :: forall a b. Free f a -> (a -> Free f b) -> Free f b
                  at fms.hs:25:12-14
                ‘f’ is a rigid type variable bound by
    ...
    Relevant bindings include
      g :: a -> Free f b (bound at fms.hs:27:16)
      f :: f (Free f a) (bound at fms.hs:27:10)
      (>>=) :: Free f a -> (a -> Free f b) -> Free f b
        (bound at fms.hs:25:12)
    ...
    Valid refinement hole fits include
      ...
      (=<<) (_ :: a -> Free f b)
        with (=<<) @(Free f) @a @b
        where (=<<) :: forall (m :: * -> *) a b.
                      Monad m =>
                      (a -> m b) -> m a -> m b
        (imported from ‘Prelude’ at fms.hs:5:18-22
        (and originally defined in ‘GHC.Base’))
      ...

Where `(=<<) _` is precisely the function we want (we ultimately want `>>= g`).

We find these refinement suggestions by considering hole fits that don't
fit the type of the hole, but ones that would fit if given an additional
argument. We do this by creating a new type variable with `newOpenFlexiTyVar`
(e.g. `t_a1/m[tau:1]`), and then considering hole fits of the type
`t_a1/m[tau:1] -> v` where `v` is the type of the hole.

Since the simplifier is free to unify this new type variable with any type, we
can discover any identifiers that would fit if given another identifier of a
suitable type. This is then generalized so that we can consider any number of
additional arguments by setting the `-frefinement-level-hole-fits` flag to any
number, and then considering hole fits like e.g. `foldl _ _` with two additional
arguments.

To make sure that the refinement hole fits are useful, we check that the types
of the additional holes have a concrete value and not just an invented type
variable. This eliminates suggestions such as `head (_ :: [t0 -> a]) (_ :: t0)`,
and limits the number of less than useful refinement hole fits.

Additionally, to further aid the user in their implementation, we show the
types of the holes the binding would have to be applied to in order to work.
In the free monad example above, this is demonstrated with
`(=<<) (_ :: a -> Free f b)`, which tells the user that the `(=<<)` needs to
be applied to an expression of type `a -> Free f b` in order to match.
If -XScopedTypeVariables is enabled, this hole fit can even be copied verbatim.




Note [Relevant Constraints]
~~~~~~~~~~~~~~~~~~~

As highlighted by #14273, we need to check any relevant constraints as well
as checking for subsumption. Relevant constraints are the simple constraints
whose free unification variables are mentioned in the type of the hole.

In the simplest case, these are all non-hole constraints in the simples, such
as is the case in

  f :: String
  f = show _

Where the simples will be :

  [[WD] __a1kz {0}:: a0_a1kv[tau:1] (CHoleCan: ExprHole(_)),
    [WD] $dShow_a1kw {0}:: Show a0_a1kv[tau:1] (CNonCanonical)]

However, when there are multiple holes, we need to be more careful. As an
example, Let's take a look at the following code:

  f :: Show a => a -> String
  f x = show (_b (show _a))

Here there are two holes, `_a` and `_b`, and the simple constraints passed to
findValidHoleFits are:

  [[WD] _a_a1pi {0}:: String
                        -> a0_a1pd[tau:2] (CHoleCan: ExprHole(_b)),
    [WD] _b_a1ps {0}:: a1_a1po[tau:2] (CHoleCan: ExprHole(_a)),
    [WD] $dShow_a1pe {0}:: Show a0_a1pd[tau:2] (CNonCanonical),
    [WD] $dShow_a1pp {0}:: Show a1_a1po[tau:2] (CNonCanonical)]


Here we have the two hole constraints for `_a` and `_b`, but also additional
constraints that these holes must fulfill. When we are looking for a match for
the hole `_a`, we filter the simple constraints to the "Relevant constraints",
by throwing out all hole constraints and any constraints which do not mention
a variable mentioned in the type of the hole. For hole `_a`, we will then
only require that the `$dShow_a1pp` constraint is solved, since that is
the only non-hole constraint that mentions any free type variables mentioned in
the hole constraint for `_a`, namely `a_a1pd[tau:2]` , and similarly for the
hole `_b` we only require that the `$dShow_a1pe` constraint is solved.



Note [Leaking errors]
~~~~~~~~~~~~~~~~~~~

When considering candidates, GHC believes that we're checking for validity in
actual source. However, As evidenced by #15321, #15007 and #15202, this can
cause bewildering error messages. The solution here is simple: if a candidate
would cause the type checker to error, it is not a valid hole fit, and thus it
is discarded.


