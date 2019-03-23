Note [Data decl contexts]
~~~~~~~~~~~~~~~~~~~~~~~~~
Consider

        data (RealFloat a) => Complex a = !a :+ !a deriving( Read )

We will need an instance decl like:

        instance (Read a, RealFloat a) => Read (Complex a) where
          ...

The RealFloat in the context is because the read method for Complex is bound
to construct a Complex, and doing that requires that the argument type is
in RealFloat.

But this ain't true for Show, Eq, Ord, etc, since they don't construct
a Complex; they only take them apart.

Our approach: identify the offending classes, and add the data type
context to the instance decl.  The "offending classes" are

        Read, Enum?

FURTHER NOTE ADDED March 2002.  In fact, Haskell98 now requires that
pattern matching against a constructor from a data type with a context
gives rise to the constraints for that context -- or at least the thinned
version.  So now all classes are "offending".



Note [Newtype deriving]
~~~~~~~~~~~~~~~~~~~~~~~
Consider this:
    class C a b
    instance C [a] Char
    newtype T = T Char deriving( C [a] )

Notice the free 'a' in the deriving.  We have to fill this out to
    newtype T = T Char deriving( forall a. C [a] )

And then translate it to:
    instance C [a] Char => C [a] T where ...




Note [Newtype deriving superclasses]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(See also #1220 for an interesting exchange on newtype
deriving and superclasses.)

The 'tys' here come from the partial application in the deriving
clause. The last arg is the new instance type.

We must pass the superclasses; the newtype might be an instance
of them in a different way than the representation type
E.g.            newtype Foo a = Foo a deriving( Show, Num, Eq )
Then the Show instance is not done via Coercible; it shows
        Foo 3 as "Foo 3"
The Num instance is derived via Coercible, but the Show superclass
dictionary must the Show instance for Foo, *not* the Show dictionary
gotten from the Num dictionary. So we must build a whole new dictionary
not just use the Num one.  The instance we want is something like:
     instance (Num a, Show (Foo a), Eq (Foo a)) => Num (Foo a) where
        (+) = ((+)@a)
        ...etc...
There may be a coercion needed which we get from the tycon for the newtype
when the dict is constructed in TcInstDcls.tcInstDecl2




Note [Unused constructors and deriving clauses]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
See #3221.  Consider
   data T = T1 | T2 deriving( Show )
Are T1 and T2 unused?  Well, no: the deriving clause expands to mention
both of them.  So we gather defs/uses from deriving just like anything else.



Note [Newtype deriving and unused constructors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this (see #1954):

  module Bug(P) where
  newtype P a = MkP (IO a) deriving Monad

If you compile with -Wunused-binds you do not expect the warning
"Defined but not used: data constructor MkP". Yet the newtype deriving
code does not explicitly mention MkP, but it should behave as if you
had written
  instance Monad P where
     return x = MkP (return x)
     ...etc...

So we want to signal a user of the data constructor 'MkP'.
This is the reason behind the [Name] part of the return type
of genInst.



Note [Staging of tcDeriving]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Here's a tricky corner case for deriving (adapted from #2721):

    class C a where
      type T a
      foo :: a -> T a

    instance C Int where
      type T Int = Int
      foo = id

    newtype N = N Int deriving C

This will produce an instance something like this:

    instance C N where
      type T N = T Int
      foo = coerce (foo :: Int -> T Int) :: N -> T N

We must be careful in order to typecheck this code. When determining the
context for the instance (in simplifyInstanceContexts), we need to determine
that T N and T Int have the same representation, but to do that, the T N
instance must be in the local family instance environment. Otherwise, GHC
would be unable to conclude that T Int is representationally equivalent to
T Int, and simplifyInstanceContexts would get stuck.

Previously, tcDeriving would defer adding any derived type family instances to
the instance environment until the very end, which meant that
simplifyInstanceContexts would get called without all the type family instances
it needed in the environment in order to properly simplify instance like
the C N instance above.

To avoid this scenario, we carefully structure the order of events in
tcDeriving. We first call genInst on the standalone derived instance specs and
the instance specs obtained from deriving clauses. Note that the return type of
genInst is a triple:

    TcM (ThetaType -> TcM (InstInfo RdrName), BagDerivStuff, Maybe Name)

The type family instances are in the BagDerivStuff. The first field of the
triple is a suspended computation which, given an instance context, produces
the rest of the instance. The fact that it is suspended is important, because
right now, we don't have ThetaTypes for the instances that use deriving clauses
(only the standalone-derived ones).

Now we can can collect the type family instances and extend the local instance
environment. At this point, it is safe to run simplifyInstanceContexts on the
deriving-clause instance specs, which gives us the ThetaTypes for the
deriving-clause instances. Now we can feed all the ThetaTypes to the
suspended computations and obtain our InstInfos, at which point
tcDeriving is done.

An alternative design would be to split up genInst so that the
family instances are generated separately from the InstInfos. But this would
require carving up a lot of the GHC deriving internals to accommodate the
change. On the other hand, we can keep all of the InstInfo and type family
instance logic together in genInst simply by converting genInst to
continuation-returning style, so we opt for that route.



Note [Why we don't pass rep_tc into deriveTyData]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Down in the bowels of mkEqnHelp, we need to convert the fam_tc back into
the rep_tc by means of a lookup. And yet we have the rep_tc right here!
Why look it up again? Answer: it's just easier this way.
We drop some number of arguments from the end of the datatype definition
in deriveTyData. The arguments are dropped from the fam_tc.
This action may drop a *different* number of arguments
passed to the rep_tc, depending on how many free variables, etc., the
dropped patterns have.

Also, this technique carries over the kind substitution from deriveTyData
nicely.



Note [Avoid RebindableSyntax when deriving]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The RebindableSyntax extension interacts awkwardly with the derivation of
any stock class whose methods require the use of string literals. The Show
class is a simple example (see #12688):

  {-# LANGUAGE RebindableSyntax, OverloadedStrings #-}
  newtype Text = Text String
  fromString :: String -> Text
  fromString = Text

  data Foo = Foo deriving Show

This will generate code to the effect of:

  instance Show Foo where
    showsPrec _ Foo = showString "Foo"

But because RebindableSyntax and OverloadedStrings are enabled, the "Foo"
string literal is now of type Text, not String, which showString doesn't
accept! This causes the generated Show instance to fail to typecheck.

To avoid this kind of scenario, we simply turn off RebindableSyntax entirely
in derived code.



Note [Flattening deriving clauses]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider what happens if you run this program (from #10684) without
DeriveGeneric enabled:

    data A = A deriving (Show, Generic)
    data B = B A deriving (Show)

Naturally, you'd expect GHC to give an error to the effect of:

    Can't make a derived instance of `Generic A':
      You need -XDeriveGeneric to derive an instance for this class

And *only* that error, since the other two derived Show instances appear to be
independent of this derived Generic instance. Yet GHC also used to give this
additional error on the program above:

    No instance for (Show A)
      arising from the 'deriving' clause of a data type declaration
    When deriving the instance for (Show B)

This was happening because when GHC encountered any error within a single
data type's set of deriving clauses, it would call recoverM and move on
to the next data type's deriving clauses. One unfortunate consequence of
this design is that if A's derived Generic instance failed, so its derived
Show instance would be skipped entirely, leading to the "No instance for
(Show A)" error cascade.

The solution to this problem is to "flatten" the set of classes that are
derived for a particular data type via deriving clauses. That is, if
you have:

    newtype C = C D
      deriving (E, F, G)
      deriving anyclass (H, I, J)
      deriving newtype  (K, L, M)

Then instead of processing instances E through M under the scope of a single
recoverM, we flatten these deriving clauses into the list:

    [ E (Nothing)
    , F (Nothing)
    , G (Nothing)
    , H (Just anyclass)
    , I (Just anyclass)
    , J (Just anyclass)
    , K (Just newtype)
    , L (Just newtype)
    , M (Just newtype) ]

And then process each class individually, under its own recoverM scope. That
way, failure to derive one class doesn't cancel out other classes in the
same set of clause-derived classes.
----------------------------------------------------------------


Note [tc_args and tycon arity]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
You might wonder if we could use (tyConArity tc) at this point, rather
than (length tc_args).  But for data families the two can differ!  The
tc and tc_args passed into 'deriveTyData' come from 'deriveClause' which
in turn gets them from 'tyConFamInstSig_maybe' which in turn gets them
from DataFamInstTyCon:

| DataFamInstTyCon          -- See Note [Data type families]
      (CoAxiom Unbranched)
      TyCon   -- The family TyCon
      [Type]  -- Argument types (mentions the tyConTyVars of this TyCon)
              -- No shorter in length than the tyConTyVars of the family TyCon
              -- How could it be longer? See [Arity of data families] in FamInstEnv

Notice that the arg tys might not be the same as the family tycon arity
(= length tyConTyVars).



Note [Unify kinds in deriving]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider (#8534)
    data T a b = MkT a deriving( Functor )
    -- where Functor :: (*->*) -> Constraint

So T :: forall k. * -> k -> *.   We want to get
    instance Functor (T * (a:*)) where ...
Notice the '*' argument to T.

Moreover, as well as instantiating T's kind arguments, we may need to instantiate
C's kind args.  Consider (#8865):
  newtype T a b = MkT (Either a b) deriving( Category )
where
  Category :: forall k. (k -> k -> *) -> Constraint
We need to generate the instance
  instance Category * (Either a) where ...
Notice the '*' argument to Category.

So we need to
 * drop arguments from (T a b) to match the number of
   arrows in the (last argument of the) class;
 * and then *unify* kind of the remaining type against the
   expected kind, to figure out how to instantiate C's and T's
   kind arguments.

In the two examples,
 * we unify   kind-of( T k (a:k) ) ~ kind-of( Functor )
         i.e.      (k -> *) ~ (* -> *)   to find k:=*.
         yielding  k:=*

 * we unify   kind-of( Either ) ~ kind-of( Category )
         i.e.      (* -> * -> *)  ~ (k -> k -> k)
         yielding  k:=*

Now we get a kind substitution.  We then need to:

  1. Remove the substituted-out kind variables from the quantified kind vars

  2. Apply the substitution to the kinds of quantified *type* vars
     (and extend the substitution to reflect this change)

  3. Apply that extended substitution to the non-dropped args (types and
     kinds) of the type and class

Forgetting step (2) caused #8893:
  data V a = V [a] deriving Functor
  data P (x::k->*) (a:k) = P (x a) deriving Functor
  data C (x::k->*) (a:k) = C (V (P x a)) deriving Functor

When deriving Functor for P, we unify k to *, but we then want
an instance   $df :: forall (x:*->*). Functor x => Functor (P * (x:*->*))
and similarly for C.  Notice the modified kind of x, both at binding
and occurrence sites.

This can lead to some surprising results when *visible* kind binder is
unified (in contrast to the above examples, in which only non-visible kind
binders were considered). Consider this example from #11732:

    data T k (a :: k) = MkT deriving Functor

Since unification yields k:=*, this results in a generated instance of:

    instance Functor (T *) where ...

which looks odd at first glance, since one might expect the instance head
to be of the form Functor (T k). Indeed, one could envision an alternative
generated instance of:

    instance (k ~ *) => Functor (T k) where

But this does not typecheck by design: kind equalities are not allowed to be
bound in types, only terms. But in essence, the two instance declarations are
entirely equivalent, since even though (T k) matches any kind k, the only
possibly value for k is *, since anything else is ill-typed. As a result, we can
just as comfortably use (T *).

Another way of thinking about is: deriving clauses often infer constraints.
For example:

    data S a = S a deriving Eq

infers an (Eq a) constraint in the derived instance. By analogy, when we
are deriving Functor, we might infer an equality constraint (e.g., k ~ *).
The only distinction is that GHC instantiates equality constraints directly
during the deriving process.

Another quirk of this design choice manifests when typeclasses have visible
kind parameters. Consider this code (also from #11732):

    class Cat k (cat :: k -> k -> *) where
      catId   :: cat a a
      catComp :: cat b c -> cat a b -> cat a c

    instance Cat * (->) where
      catId   = id
      catComp = (.)

    newtype Fun a b = Fun (a -> b) deriving (Cat k)

Even though we requested a derived instance of the form (Cat k Fun), the
kind unification will actually generate (Cat * Fun) (i.e., the same thing as if
the user wrote deriving (Cat *)).

What happens with DerivingVia, when you have yet another type? Consider:

  newtype Foo (a :: Type) = MkFoo (Proxy a)
    deriving Functor via Proxy

As before, we unify the kind of Foo (* -> *) with the kind of the argument to
Functor (* -> *). But that's not enough: the `via` type, Proxy, has the kind
(k -> *), which is more general than what we want. So we must additionally
unify (k -> *) with (* -> *).

Currently, all of this unification is implemented kludgily with the pure
unifier, which is rather tiresome. #14331 lays out a plan for how this
might be made cleaner.



Note [Unification of two kind variables in deriving]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
As a special case of the Note above, it is possible to derive an instance of
a poly-kinded typeclass for a poly-kinded datatype. For example:

    class Category (cat :: k -> k -> *) where
    newtype T (c :: k -> k -> *) a b = MkT (c a b) deriving Category

This case is suprisingly tricky. To see why, let's write out what instance GHC
will attempt to derive (using -fprint-explicit-kinds syntax):

    instance Category k1 (T k2 c) where ...

GHC will attempt to unify k1 and k2, which produces a substitution (kind_subst)
that looks like [k2 :-> k1]. Importantly, we need to apply this substitution to
the type variable binder for c, since its kind is (k2 -> k2 -> *).

We used to accomplish this by doing the following:

    unmapped_tkvs = filter (`notElemTCvSubst` kind_subst) all_tkvs
    (subst, _)    = substTyVarBndrs kind_subst unmapped_tkvs

Where all_tkvs contains all kind variables in the class and instance types (in
this case, all_tkvs = [k1,k2]). But since kind_subst only has one mapping,
this results in unmapped_tkvs being [k1], and as a consequence, k1 gets mapped
to another kind variable in subst! That is, subst = [k2 :-> k1, k1 :-> k_new].
This is bad, because applying that substitution yields the following instance:

   instance Category k_new (T k1 c) where ...

In other words, keeping k1 in unmapped_tvks taints the substitution, resulting
in an ill-kinded instance (this caused #11837).

To prevent this, we need to filter out any variable from all_tkvs which either

1. Appears in the domain of kind_subst. notElemTCvSubst checks this.
2. Appears in the range of kind_subst. To do this, we compute the free
   variable set of the range of kind_subst with getTCvSubstRangeFVs, and check
   if a kind variable appears in that set.



Note [Eta-reducing type synonyms]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
One can instantiate a type in a data family instance with a type synonym that
mentions other type variables:

  type Const a b = a
  data family Fam (f :: * -> *) (a :: *)
  newtype instance Fam f (Const a f) = Fam (f a) deriving Functor

It is also possible to define kind synonyms, and they can mention other types in
a datatype declaration. For example,

  type Const a b = a
  newtype T f (a :: Const * f) = T (f a) deriving Functor

When deriving, we need to perform eta-reduction analysis to ensure that none of
the eta-reduced type variables are mentioned elsewhere in the declaration. But
we need to be careful, because if we don't expand through the Const type
synonym, we will mistakenly believe that f is an eta-reduced type variable and
fail to derive Functor, even though the code above is correct (see #11416,
where this was first noticed). For this reason, we expand the type synonyms in
the eta-reduced types before doing any analysis.


Note [Looking up family instances for deriving]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tcLookupFamInstExact is an auxiliary lookup wrapper which requires
that looked-up family instances exist.  If called with a vanilla
tycon, the old type application is simply returned.

If we have
  data instance F () = ... deriving Eq
  data instance F () = ... deriving Eq
then tcLookupFamInstExact will be confused by the two matches;
but that can't happen because tcInstDecls1 doesn't call tcDeriving
if there are any overlaps.

There are two other things that might go wrong with the lookup.
First, we might see a standalone deriving clause
   deriving Eq (F ())
when there is no data instance F () in scope.

Note that it's OK to have
  data instance F [a] = ...
  deriving Eq (F [(a,b)])
where the match is not exact; the same holds for ordinary data types
with standalone deriving declarations.



Note [Deriving, type families, and partial applications]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When there are no type families, it's quite easy:

    newtype S a = MkS [a]
    -- :CoS :: S  ~ []  -- Eta-reduced

    instance Eq [a] => Eq (S a)         -- by coercion sym (Eq (:CoS a)) : Eq [a] ~ Eq (S a)
    instance Monad [] => Monad S        -- by coercion sym (Monad :CoS)  : Monad [] ~ Monad S

When type familes are involved it's trickier:

    data family T a b
    newtype instance T Int a = MkT [a] deriving( Eq, Monad )
    -- :RT is the representation type for (T Int a)
    --  :Co:RT    :: :RT ~ []          -- Eta-reduced!
    --  :CoF:RT a :: T Int a ~ :RT a   -- Also eta-reduced!

    instance Eq [a] => Eq (T Int a)     -- easy by coercion
       -- d1 :: Eq [a]
       -- d2 :: Eq (T Int a) = d1 |> Eq (sym (:Co:RT a ; :coF:RT a))

    instance Monad [] => Monad (T Int)  -- only if we can eta reduce???
       -- d1 :: Monad []
       -- d2 :: Monad (T Int) = d1 |> Monad (sym (:Co:RT ; :coF:RT))

Note the need for the eta-reduced rule axioms.  After all, we can
write it out
    instance Monad [] => Monad (T Int)  -- only if we can eta reduce???
      return x = MkT [x]
      ... etc ...

See Note [Eta reduction for data families] in FamInstEnv

%************************************************************************
%*                                                                      *
                Deriving data types
*                                                                      *
************************************************************************


Note [Recursive newtypes]
~~~~~~~~~~~~~~~~~~~~~~~~~
Newtype deriving works fine, even if the newtype is recursive.
e.g.    newtype S1 = S1 [T1 ()]
        newtype T1 a = T1 (StateT S1 IO a ) deriving( Monad )
Remember, too, that type families are currently (conservatively) given
a recursive flag, so this also allows newtype deriving to work
for type famillies.

We used to exclude recursive types, because we had a rather simple
minded way of generating the instance decl:
   newtype A = MkA [A]
   instance Eq [A] => Eq A      -- Makes typechecker loop!
But now we require a simple context, so it's ok.



Note [Determining whether newtype-deriving is appropriate]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we see
  newtype NT = MkNT Foo
    deriving C
we have to decide how to perform the deriving. Do we do newtype deriving,
or do we do normal deriving? In general, we prefer to do newtype deriving
wherever possible. So, we try newtype deriving unless there's a glaring
reason not to.

"Glaring reasons not to" include trying to derive a class for which a
coercion-based instance doesn't make sense. These classes are listed in
the definition of non_coercible_class. They include Show (since it must
show the name of the datatype) and Traversable (since a coercion-based
Traversable instance is ill-roled).

However, non_coercible_class is ignored if the user explicitly requests
to derive an instance with GeneralizedNewtypeDeriving using the newtype
deriving strategy. In such a scenario, GHC will unquestioningly try to
derive the instance via coercions (even if the final generated code is
ill-roled!). See Note [Deriving strategies].

Note that newtype deriving might fail, even after we commit to it. This
is because the derived instance uses `coerce`, which must satisfy its
`Coercible` constraint. This is different than other deriving scenarios,
where we're sure that the resulting instance will type-check.



Note [GND and associated type families]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It's possible to use GeneralizedNewtypeDeriving (GND) to derive instances for
classes with associated type families. A general recipe is:

    class C x y z where
      type T y z x
      op :: x -> [y] -> z

    newtype N a = MkN <rep-type> deriving( C )

    =====>

    instance C x y <rep-type> => C x y (N a) where
      type T y (N a) x = T y <rep-type> x
      op = coerce (op :: x -> [y] -> <rep-type>)

However, we must watch out for three things:

(a) The class must not contain any data families. If it did, we'd have to
    generate a fresh data constructor name for the derived data family
    instance, and it's not clear how to do this.

(b) Each associated type family's type variables must mention the last type
    variable of the class. As an example, you wouldn't be able to use GND to
    derive an instance of this class:

      class C a b where
        type T a

    But you would be able to derive an instance of this class:

      class C a b where
        type T b

    The difference is that in the latter T mentions the last parameter of C
    (i.e., it mentions b), but the former T does not. If you tried, e.g.,

      newtype Foo x = Foo x deriving (C a)

    with the former definition of C, you'd end up with something like this:

      instance C a (Foo x) where
        type T a = T ???

    This T family instance doesn't mention the newtype (or its representation
    type) at all, so we disallow such constructions with GND.

(c) UndecidableInstances might need to be enabled. Here's a case where it is
    most definitely necessary:

      class C a where
        type T a
      newtype Loop = Loop MkLoop deriving C

      =====>

      instance C Loop where
        type T Loop = T Loop

    Obviously, T Loop would send the typechecker into a loop. Unfortunately,
    you might even need UndecidableInstances even in cases where the
    typechecker would be guaranteed to terminate. For example:

      instance C Int where
        type C Int = Int
      newtype MyInt = MyInt Int deriving C

      =====>

      instance C MyInt where
        type T MyInt = T Int

    GHC's termination checker isn't sophisticated enough to conclude that the
    definition of T MyInt terminates, so UndecidableInstances is required.

(d) For the time being, we do not allow the last type variable of the class to
    appear in a /kind/ of an associated type family definition. For instance:

    class C a where
      type T1 a        -- OK
      type T2 (x :: a) -- Illegal: a appears in the kind of x
      type T3 y :: a   -- Illegal: a appears in the kind of (T3 y)

    The reason we disallow this is because our current approach to deriving
    associated type family instances—i.e., by unwrapping the newtype's type
    constructor as shown above—is ill-equipped to handle the scenario when
    the last type variable appears as an implicit argument. In the worst case,
    allowing the last variable to appear in a kind can result in improper Core
    being generated (see #14728).

    There is hope for this feature being added some day, as one could
    conceivably take a newtype axiom (which witnesses a coercion between a
    newtype and its representation type) at lift that through each associated
    type at the Core level. See #14728, comment:3 for a sketch of how this
    might work. Until then, we disallow this featurette wholesale.

The same criteria apply to DerivingVia.



Note [Bindings for Generalised Newtype Deriving]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
  class Eq a => C a where
     f :: a -> a
  newtype N a = MkN [a] deriving( C )
  instance Eq (N a) where ...

The 'deriving C' clause generates, in effect
  instance (C [a], Eq a) => C (N a) where
     f = coerce (f :: [a] -> [a])

This generates a cast for each method, but allows the superclasse to
be worked out in the usual way.  In this case the superclass (Eq (N
a)) will be solved by the explicit Eq (N a) instance.  We do *not*
create the superclasses by casting the superclass dictionaries for the
representation type.

See the paper "Safe zero-cost coercions for Haskell".



Note [DeriveAnyClass and default family instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When a class has a associated type family with a default instance, e.g.:

  class C a where
    type T a
    type T a = Char

then there are a couple of scenarios in which a user would expect T a to
default to Char. One is when an instance declaration for C is given without
an implementation for T:

  instance C Int

Another scenario in which this can occur is when the -XDeriveAnyClass extension
is used:

  data Example = Example deriving (C, Generic)

In the latter case, we must take care to check if C has any associated type
families with default instances, because -XDeriveAnyClass will never provide
an implementation for them. We "fill in" the default instances using the
tcATDefault function from TcClassDcl (which is also used in TcInstDcls to
handle the empty instance declaration case).



Note [Deriving strategies]
~~~~~~~~~~~~~~~~~~~~~~~~~~
GHC has a notion of deriving strategies, which allow the user to explicitly
request which approach to use when deriving an instance (enabled with the
-XDerivingStrategies language extension). For more information, refer to the
original issue (#10598) or the associated wiki page:
https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/DerivingStrategies

A deriving strategy can be specified in a deriving clause:

    newtype Foo = MkFoo Bar
      deriving newtype C

Or in a standalone deriving declaration:

    deriving anyclass instance C Foo

-XDerivingStrategies also allows the use of multiple deriving clauses per data
declaration so that a user can derive some instance with one deriving strategy
and other instances with another deriving strategy. For example:

    newtype Baz = Baz Quux
      deriving          (Eq, Ord)
      deriving stock    (Read, Show)
      deriving newtype  (Num, Floating)
      deriving anyclass C

Currently, the deriving strategies are:

* stock: Have GHC implement a "standard" instance for a data type, if possible
  (e.g., Eq, Ord, Generic, Data, Functor, etc.)

* anyclass: Use -XDeriveAnyClass

* newtype: Use -XGeneralizedNewtypeDeriving

* via: Use -XDerivingVia

The latter two strategies (newtype and via) are referred to as the
"coerce-based" strategies, since they generate code that relies on the `coerce`
function. The former two strategies (stock and anyclass), in contrast, are
referred to as the "originative" strategies, since they create "original"
instances instead of "reusing" old instances (by way of `coerce`).

If an explicit deriving strategy is not given, GHC has an algorithm it uses to
determine which strategy it will actually use. The algorithm is quite long,
so it lives in the Haskell wiki at
https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/DerivingStrategies
("The deriving strategy resolution algorithm" section).

Internally, GHC uses the DerivStrategy datatype to denote a user-requested
deriving strategy, and it uses the DerivSpecMechanism datatype to denote what
GHC will use to derive the instance after taking the above steps. In other
words, GHC will always settle on a DerivSpecMechnism, even if the user did not
ask for a particular DerivStrategy (using the algorithm linked to above).



Note [Deriving instances for classes themselves]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Much of the code in TcDeriv assumes that deriving only works on data types.
But this assumption doesn't hold true for DeriveAnyClass, since it's perfectly
reasonable to do something like this:

  {-# LANGUAGE DeriveAnyClass #-}
  class C1 (a :: Constraint) where
  class C2 where
  deriving instance C1 C2
    -- This is equivalent to `instance C1 C2`

If DeriveAnyClass isn't enabled in the code above (i.e., it defaults to stock
deriving), we throw a special error message indicating that DeriveAnyClass is
the only way to go. We don't bother throwing this error if an explicit 'stock'
or 'newtype' keyword is used, since both options have their own perfectly
sensible error messages in the case of the above code (as C1 isn't a stock
derivable class, and C2 isn't a newtype).

