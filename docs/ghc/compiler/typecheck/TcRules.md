[[src]](https://github.com/ghc/ghc/tree/master/compiler/typecheck/TcRules.hs)

(c) The University of Glasgow 2006
(c) The AQUA Project, Glasgow University, 1993-1998


TcRules: Typechecking transformation rules


### Note: Typechecking rules

We *infer* the typ of the LHS, and use that type to *check* the type of
the RHS.  That means that higher-rank rules work reasonably well. Here's
an example (test simplCore/should_compile/rule2.hs) produced by Roman:

   foo :: (forall m. m a -> m b) -> m a -> m b
   foo f = ...

   bar :: (forall m. m a -> m a) -> m a -> m a
   bar f = ...

# Constraint simplification for rules


### Note: The SimplifyRule Plan

Example.  Consider the following left-hand side of a rule
        f (x == y) (y > z) = ...
If we typecheck this expression we get constraints
        d1 :: Ord a, d2 :: Eq a
We do NOT want to "simplify" to the LHS
        forall x::a, y::a, z::a, d1::Ord a.
          f ((==) (eqFromOrd d1) x y) ((>) d1 y z) = ...
Instead we want
        forall x::a, y::a, z::a, d1::Ord a, d2::Eq a.
          f ((==) d2 x y) ((>) d1 y z) = ...

# RULES "foo" forall (v::forall b. Eq b => b->b).
       f b True = ...
    #