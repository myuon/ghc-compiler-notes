`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcDerivInfer.hs>`_

compiler/typecheck/TcDerivInfer.hs
==================================


Note [Inferring the instance context]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcDerivInfer.hs#L325>`__

There are two sorts of 'deriving', as represented by the two constructors
for DerivContext:

  * InferContext mb_wildcard: This can either be:
    - The deriving clause for a data type.
        (e.g, data T a = T1 a deriving( Eq ))
      In this case, mb_wildcard = Nothing.
    - A standalone declaration with an extra-constraints wildcard
        (e.g., deriving instance _ => Eq (Foo a))
      In this case, mb_wildcard = Just loc, where loc is the location
      of the extra-constraints wildcard.

    Here we must infer an instance context,
    and generate instance declaration
      instance Eq a => Eq (T a) where ...

  * SupplyContext theta: standalone deriving
      deriving instance Eq a => Eq (T a)
    Here we only need to fill in the bindings;
    the instance context (theta) is user-supplied

For the InferContext case, we must figure out the
instance context (inferConstraintsDataConArgs). Suppose we are inferring
the instance context for
    C t1 .. tn (T s1 .. sm)
There are two cases

  * (T s1 .. sm) :: *         (the normal case)
    Then we behave like Eq and guess (C t1 .. tn t)
    for each data constructor arg of type t.  More
    details below.

  * (T s1 .. sm) :: * -> *    (the functor-like case)
    Then we behave like Functor.

In both cases we produce a bunch of un-simplified constraints
and them simplify them in simplifyInstanceContexts; see
Note [Simplifying the instance context].

In the functor-like case, we may need to unify some kind variables with * in
order for the generated instance to be well-kinded. An example from
#10524:

::

  newtype Compose (f :: k2 -> *) (g :: k1 -> k2) (a :: k1)
    = Compose (f (g a)) deriving Functor

..

Earlier in the deriving pipeline, GHC unifies the kind of Compose f g
(k1 -> *) with the kind of Functor's argument (* -> *), so k1 := *. But this
alone isn't enough, since k2 wasn't unified with *:

::

  instance (Functor (f :: k2 -> *), Functor (g :: * -> k2)) =>
    Functor (Compose f g) where ...

..

The two Functor constraints are ill-kinded. To ensure this doesn't happen, we:

  1. Collect all of a datatype's subtypes which require functor-like
     constraints.
  2. For each subtype, create a substitution by unifying the subtype's kind
     with (* -> *).
  3. Compose all the substitutions into one, then apply that substitution to
     all of the in-scope type variables and the instance types.



Note [Getting base classes]
~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcDerivInfer.hs#L389>`__

Functor and Typeable are defined in package 'base', and that is not available
when compiling 'ghc-prim'.  So we must be careful that 'deriving' for stuff in
ghc-prim does not use Functor or Typeable implicitly via these lookups.



Note [Deriving and unboxed types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcDerivInfer.hs#L395>`__

We have some special hacks to support things like
   data T = MkT Int# deriving ( Show )

Specifically, we use TcGenDeriv.box to box the Int# into an Int
(which we know how to show), and append a '#'. Parentheses are not required
for unboxed values (`MkT -3#` is a valid expression).



Note [Superclasses of derived instance]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcDerivInfer.hs#L404>`__

In general, a derived instance decl needs the superclasses of the derived
class too.  So if we have
        data T a = ...deriving( Ord )
then the initial context for Ord (T a) should include Eq (T a).  Often this is
redundant; we'll also generate an Ord constraint for each constructor argument,
and that will probably generate enough constraints to make the Eq (T a) constraint
be satisfied too.  But not always; consider:

::

 data S a = S
 instance Eq (S a)
 instance Ord (S a)

..

::

 data T a = MkT (S a) deriving( Ord )
 instance Num a => Eq (T a)

..

The derived instance for (Ord (T a)) must have a (Num a) constraint!
Similarly consider:
        data T a = MkT deriving( Data )
Here there *is* no argument field, but we must nevertheless generate
a context for the Data instances:
        instance Typeable a => Data (T a) where ...



Note [Simplifying the instance context]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcDerivInfer.hs#L435>`__

Consider

::

        data T a b = C1 (Foo a) (Bar b)
                   | C2 Int (T b a)
                   | C3 (T a a)
                   deriving (Eq)

..

We want to come up with an instance declaration of the form

::

        instance (Ping a, Pong b, ...) => Eq (T a b) where
                x == y = ...

..

It is pretty easy, albeit tedious, to fill in the code "...".  The
trick is to figure out what the context for the instance decl is,
namely Ping, Pong and friends.

Let's call the context reqd for the T instance of class C at types
(a,b, ...)  C (T a b).  Thus:

::

        Eq (T a b) = (Ping a, Pong b, ...)

..

Now we can get a (recursive) equation from the data decl.  This part
is done by inferConstraintsDataConArgs.

::

        Eq (T a b) = Eq (Foo a) u Eq (Bar b)    -- From C1
                   u Eq (T b a) u Eq Int        -- From C2
                   u Eq (T a a)                 -- From C3

..


Foo and Bar may have explicit instances for Eq, in which case we can
just substitute for them.  Alternatively, either or both may have
their Eq instances given by deriving clauses, in which case they
form part of the system of equations.

Now all we need do is simplify and solve the equations, iterating to
find the least fixpoint.  This is done by simplifyInstanceConstraints.
Notice that the order of the arguments can
switch around, as here in the recursive calls to T.

Let's suppose Eq (Foo a) = Eq a, and Eq (Bar b) = Ping b.

We start with:

::

        Eq (T a b) = {}         -- The empty set

..

Next iteration:
        Eq (T a b) = Eq (Foo a) u Eq (Bar b)    -- From C1
                   u Eq (T b a) u Eq Int        -- From C2
                   u Eq (T a a)                 -- From C3

        After simplification:
                   = Eq a u Ping b u {} u {} u {}
                   = Eq a u Ping b

Next iteration:

::

        Eq (T a b) = Eq (Foo a) u Eq (Bar b)    -- From C1
                   u Eq (T b a) u Eq Int        -- From C2
                   u Eq (T a a)                 -- From C3

..

        After simplification:
                   = Eq a u Ping b
                   u (Eq b u Ping a)
                   u (Eq a u Ping a)

::

                   = Eq a u Ping b u Eq b u Ping a

..

The next iteration gives the same result, so this is the fixpoint.  We
need to make a canonical form of the RHS to ensure convergence.  We do
this by simplifying the RHS to a form in which

        - the classes constrain only tyvars
        - the list is sorted by tyvar (major key) and then class (minor key)
        - no duplicates, of course



Note [Deterministic simplifyInstanceContexts]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcDerivInfer.hs#L512>`__

Canonicalisation uses nonDetCmpType which is nondeterministic. Sorting
with nonDetCmpType puts the returned lists in a nondeterministic order.
If we were to return them, we'd get class constraints in
nondeterministic order.

Consider:

::

  data ADT a b = Z a b deriving Eq

..

The generated code could be either:

::

  instance (Eq a, Eq b) => Eq (Z a b) where

..

Or:

::

  instance (Eq b, Eq a) => Eq (Z a b) where

..

To prevent the order from being nondeterministic we only
canonicalize when comparing and return them in the same order as
simplifyDeriv returned them.
See also Note [nonDetCmpType nondeterminism]



Note [Overlap and deriving]
~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcDerivInfer.hs#L753>`__

Consider some overlapping instances:
  instance Show a => Show [a] where ..
  instance Show [Char] where ...

Now a data type with deriving:
  data T a = MkT [a] deriving( Show )

We want to get the derived instance
  instance Show [a] => Show (T a) where...
and NOT
  instance Show a => Show (T a) where...
so that the (Show (T Char)) instance does the Right Thing

It's very like the situation when we're inferring the type
of a function
   f x = show [x]
and we want to infer
   f :: Show [a] => a -> String

BOTTOM LINE: use vanilla, non-overlappable skolems when inferring
             the context for the derived instance.
             Hence tcInstSkolTyVars not tcInstSuperSkolTyVars



Note [Gathering and simplifying constraints for DeriveAnyClass]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcDerivInfer.hs#L778>`__

DeriveAnyClass works quite differently from stock and newtype deriving in
the way it gathers and simplifies constraints to be used in a derived
instance's context. Stock and newtype deriving gather constraints by looking
at the data constructors of the data type for which we are deriving an
instance. But DeriveAnyClass doesn't need to know about a data type's
definition at all!

To see why, consider this example of DeriveAnyClass:

::

  class Foo a where
    bar :: forall b. Ix b => a -> b -> String
    default bar :: (Show a, Ix c) => a -> c -> String
    bar x y = show x ++ show (range (y,y))

..

::

    baz :: Eq a => a -> a -> Bool
    default baz :: (Ord a, Show a) => a -> a -> Bool
    baz x y = compare x y == EQ

..

Because 'bar' and 'baz' have default signatures, this generates a top-level
definition for these generic default methods

::

  $gdm_bar :: forall a. Foo a
           => forall c. (Show a, Ix c)
           => a -> c -> String
  $gdm_bar x y = show x ++ show (range (y,y))

..

(and similarly for baz).  Now consider a 'deriving' clause
  data Maybe s = ... deriving Foo

This derives an instance of the form:
  instance (CX) => Foo (Maybe s) where
    bar = $gdm_bar
    baz = $gdm_baz

Now it is GHC's job to fill in a suitable instance context (CX).  If
GHC were typechecking the binding
   bar = $gdm bar
it would
   * skolemise the expected type of bar
   * instantiate the type of $gdm_bar with meta-type variables
   * build an implication constraint

[STEP DAC BUILD]
So that's what we do.  We build the constraint (call it C1)

::

   forall[2] b. Ix b => (Show (Maybe s), Ix cc,
                        Maybe s -> b -> String
                            ~ Maybe s -> cc -> String)

..

Here:
* The level of this forall constraint is forall[2], because we are later
  going to wrap it in a forall[1] in [STEP DAC RESIDUAL]

* The 'b' comes from the quantified type variable in the expected type
  of bar (i.e., 'to_anyclass_skols' in 'ThetaOrigin'). The 'cc' is a unification
  variable that comes from instantiating the quantified type variable 'c' in
  $gdm_bar's type (i.e., 'to_anyclass_metas' in 'ThetaOrigin).

* The (Ix b) constraint comes from the context of bar's type
  (i.e., 'to_wanted_givens' in 'ThetaOrigin'). The (Show (Maybe s)) and (Ix cc)
  constraints come from the context of $gdm_bar's type
  (i.e., 'to_anyclass_givens' in 'ThetaOrigin').

* The equality constraint (Maybe s -> b -> String) ~ (Maybe s -> cc -> String)
  comes from marrying up the instantiated type of $gdm_bar with the specified
  type of bar. Notice that the type variables from the instance, 's' in this
  case, are global to this constraint.

Note that it is vital that we instantiate the `c` in $gdm_bar's type with a new
unification variable for each iteration of simplifyDeriv. If we re-use the same
unification variable across multiple iterations, then bad things can happen,
such as #14933.

Similarly for 'baz', givng the constraint C2

::

   forall[2]. Eq (Maybe s) => (Ord a, Show a,
                              Maybe s -> Maybe s -> Bool
                                ~ Maybe s -> Maybe s -> Bool)

..

In this case baz has no local quantification, so the implication
constraint has no local skolems and there are no unification
variables.

[STEP DAC SOLVE]
We can combine these two implication constraints into a single
constraint (C1, C2), and simplify, unifying cc:=b, to get:

::

   forall[2] b. Ix b => Show a
   /   forall[2]. Eq (Maybe s) => (Ord a, Show a)

..


[STEP DAC HOIST]
Let's call that (C1', C2').  Now we need to hoist the unsolved
constraints out of the implications to become our candidate for
(CX). That is done by approximateWC, which will return:

::

  (Show a, Ord a, Show a)

..

Now we can use mkMinimalBySCs to remove superclasses and duplicates, giving

::

  (Show a, Ord a)

..

And that's what GHC uses for CX.

[STEP DAC RESIDUAL]
In this case we have solved all the leftover constraints, but what if
we don't?  Simple!  We just form the final residual constraint

::

   forall[1] s. CX => (C1',C2')

..

and simplify that. In simple cases it'll succeed easily, because CX
literally contains the constraints in C1', C2', but if there is anything
more complicated it will be reported in a civilised way.



Note [Error reporting for deriving clauses]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcDerivInfer.hs#L894>`__

A suprisingly tricky aspect of deriving to get right is reporting sensible
error messages. In particular, if simplifyDeriv reaches a constraint that it
cannot solve, which might include:

1. Insoluble constraints
2. "Exotic" constraints (See Note [Exotic derived instance contexts])

Then we report an error immediately in simplifyDeriv.

Another possible choice is to punt and let another part of the typechecker
(e.g., simplifyInstanceContexts) catch the errors. But this tends to lead
to worse error messages, so we do it directly in simplifyDeriv.

simplifyDeriv checks for errors in a clever way. If the deriving machinery
infers the context (Foo a)--that is, if this instance is to be generated:

::

  instance Foo a => ...

..

Then we form an implication of the form:

::

  forall a. Foo a => <residual_wanted_constraints>

..

And pass it to the simplifier. If the context (Foo a) is enough to discharge
all the constraints in <residual_wanted_constraints>, then everything is
hunky-dory. But if <residual_wanted_constraints> contains, say, an insoluble
constraint, then (Foo a) won't be able to solve it, causing GHC to error.



Note [Exotic derived instance contexts]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcDerivInfer.hs#L923>`__

In a 'derived' instance declaration, we *infer* the context.  It's a
bit unclear what rules we should apply for this; the Haskell report is
silent.  Obviously, constraints like (Eq a) are fine, but what about
        data T f a = MkT (f a) deriving( Eq )
where we'd get an Eq (f a) constraint.  That's probably fine too.

One could go further: consider
        data T a b c = MkT (Foo a b c) deriving( Eq )
        instance (C Int a, Eq b, Eq c) => Eq (Foo a b c)

Notice that this instance (just) satisfies the Paterson termination
conditions.  Then we *could* derive an instance decl like this:

        instance (C Int a, Eq b, Eq c) => Eq (T a b c)
even though there is no instance for (C Int a), because there just
*might* be an instance for, say, (C Int Bool) at a site where we
need the equality instance for T's.

However, this seems pretty exotic, and it's quite tricky to allow
this, and yet give sensible error messages in the (much more common)
case where we really want that instance decl for C.

So for now we simply require that the derived instance context
should have only type-variable constraints.

Here is another example:
        data Fix f = In (f (Fix f)) deriving( Eq )
Here, if we are prepared to allow -XUndecidableInstances we
could derive the instance
        instance Eq (f (Fix f)) => Eq (Fix f)
but this is so delicate that I don't think it should happen inside
'deriving'. If you want this, write it yourself!

NB: if you want to lift this condition, make sure you still meet the
termination conditions!  If not, the deriving mechanism generates
larger and larger constraints.  Example:
  data Succ a = S a
  data Seq a = Cons a (Seq (Succ a)) | Nil deriving Show

Note the lack of a Show instance for Succ.  First we'll generate
  instance (Show (Succ a), Show a) => Show (Seq a)
and then
  instance (Show (Succ (Succ a)), Show (Succ a), Show a) => Show (Seq a)
and so on.  Instead we want to complain of no instance for (Show (Succ a)).

The bottom line
~~~~~~~~~~~~~~~
Allow constraints which consist only of type variables, with no repeats.

