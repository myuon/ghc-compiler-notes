Note [FamInsts and CoAxioms]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
* CoAxioms and FamInsts are just like
  DFunIds  and ClsInsts

* A CoAxiom is a System-FC thing: it can relate any two types

* A FamInst is a Haskell source-language thing, corresponding
  to a type/data family instance declaration.
    - The FamInst contains a CoAxiom, which is the evidence
      for the instance

    - The LHS of the CoAxiom is always of form F ty1 .. tyn
      where F is a type family


Note [Arity of data families]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Data family instances might legitimately be over- or under-saturated.

Under-saturation has two potential causes:
 U1) Eta reduction. See Note [Eta reduction for data families].
 U2) When the user has specified a return kind instead of written out patterns.
     Example:

       data family Sing (a :: k)
       data instance Sing :: Bool -> Type

     The data family tycon Sing has an arity of 2, the k and the a. But
     the data instance has only one pattern, Bool (standing in for k).
     This instance is equivalent to `data instance Sing (a :: Bool)`, but
     without the last pattern, we have an under-saturated data family instance.
     On its own, this example is not compelling enough to add support for
     under-saturation, but U1 makes this feature more compelling.

Over-saturation is also possible:
  O1) If the data family's return kind is a type variable (see also #12369),
      an instance might legitimately have more arguments than the family.
      Example:

        data family Fix :: (Type -> k) -> k
        data instance Fix f = MkFix1 (f (Fix f))
        data instance Fix f x = MkFix2 (f (Fix f x) x)

      In the first instance here, the k in the data family kind is chosen to
      be Type. In the second, it's (Type -> Type).

      However, we require that any over-saturation is eta-reducible. That is,
      we require that any extra patterns be bare unrepeated type variables;
      see Note [Eta reduction for data families]. Accordingly, the FamInst
      is never over-saturated.

Why can we allow such flexibility for data families but not for type families?
Because data families can be decomposed -- that is, they are generative and
injective. A Type family is neither and so always must be applied to all its
arguments.
Obtain the axiom of a family instance


Note [Lazy axiom match]
~~~~~~~~~~~~~~~~~~~~~~~
It is Vitally Important that mkImportedFamInst is *lazy* in its axiom
parameter. The axiom is loaded lazily, via a forkM, in TcIface. Sometime
later, mkImportedFamInst is called using that axiom. However, the axiom
may itself depend on entities which are not yet loaded as of the time
of the mkImportedFamInst. Thus, if mkImportedFamInst eagerly looks at the
axiom, a dependency loop spontaneously appears and GHC hangs. The solution
is simply for mkImportedFamInst never, ever to look inside of the axiom
until everything else is good and ready to do so. We can assume that this
readiness has been achieved when some other code pulls on the axiom in the
FamInst. Thus, we pattern match on the axiom lazily (in the where clause,
not in the parameter list) and we assert the consistency of names there
also.
Make a family instance representation from the information found in an
interface file.  In particular, we get the rough match info from the iface
(instead of computing it here).


Note [FamInstEnv]
~~~~~~~~~~~~~~~~~
A FamInstEnv maps a family name to the list of known instances for that family.

The same FamInstEnv includes both 'data family' and 'type family' instances.
Type families are reduced during type inference, but not data families;
the user explains when to use a data family instance by using constructors
and pattern matching.

Nevertheless it is still useful to have data families in the FamInstEnv:

 - For finding overlaps and conflicts

 - For finding the representation type...see FamInstEnv.topNormaliseType
   and its call site in Simplify

 - In standalone deriving instance Eq (T [Int]) we need to find the
   representation type for T [Int]



Note [Varying number of patterns for data family axioms]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For data families, the number of patterns may vary between instances.
For example
   data family T a b
   data instance T Int a = T1 a | T2
   data instance T Bool [a] = T3 a

Then we get a data type for each instance, and an axiom:
   data TInt a = T1 a | T2
   data TBoolList a = T3 a

   axiom ax7   :: T Int ~ TInt   -- Eta-reduced
   axiom ax8 a :: T Bool [a] ~ TBoolList a

These two axioms for T, one with one pattern, one with two;
see Note [Eta reduction for data families]



Note [FamInstEnv determinism]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We turn FamInstEnvs into a list in some places that don't directly affect
the ABI. That happens in family consistency checks and when producing output
for `:info`. Unfortunately that nondeterminism is nonlocal and it's hard
to tell what it affects without following a chain of functions. It's also
easy to accidentally make that nondeterminism affect the ABI. Furthermore
the envs should be relatively small, so it should be free to use deterministic
maps here. Testing with nofib and validate detected no difference between
UniqFM and UniqDFM.
See Note [Deterministic UniqFM].


Note [Apartness]
~~~~~~~~~~~~~~~~
In dealing with closed type families, we must be able to check that one type
will never reduce to another. This check is called /apartness/. The check
is always between a target (which may be an arbitrary type) and a pattern.
Here is how we do it:

apart(target, pattern) = not (unify(flatten(target), pattern))

where flatten (implemented in flattenTys, below) converts all type-family
applications into fresh variables. (See Note [Flattening].)



Note [Compatibility]
~~~~~~~~~~~~~~~~~~~~
Two patterns are /compatible/ if either of the following conditions hold:
1) The patterns are apart.
2) The patterns unify with a substitution S, and their right hand sides
equal under that substitution.

For open type families, only compatible instances are allowed. For closed
type families, the story is slightly more complicated. Consider the following:

type family F a where
  F Int = Bool
  F a   = Int

g :: Show a => a -> F a
g x = length (show x)

Should that type-check? No. We need to allow for the possibility that 'a'
might be Int and therefore 'F a' should be Bool. We can simplify 'F a' to Int
only when we can be sure that 'a' is not Int.

To achieve this, after finding a possible match within the equations, we have to
go back to all previous equations and check that, under the
substitution induced by the match, other branches are surely apart. (See
Note [Apartness].) This is similar to what happens with class
instance selection, when we need to guarantee that there is only a match and
no unifiers. The exact algorithm is different here because the
potentially-overlapping group is closed.

As another example, consider this:

type family G x where
  G Int = Bool
  G a   = Double

type family H y
-- no instances

Now, we want to simplify (G (H Char)). We can't, because (H Char) might later
simplify to be Int. So, (G (H Char)) is stuck, for now.

While everything above is quite sound, it isn't as expressive as we'd like.
Consider this:

type family J a where
  J Int = Int
  J a   = a

Can we simplify (J b) to b? Sure we can. Yes, the first equation matches if
b is instantiated with Int, but the RHSs coincide there, so it's all OK.

So, the rule is this: when looking up a branch in a closed type family, we
find a branch that matches the target, but then we make sure that the target
is apart from every previous *incompatible* branch. We don't check the
branches that are compatible with the matching branch, because they are either
irrelevant (clause 1 of compatible) or benign (clause 2 of compatible).



Note [Compatibility of eta-reduced axioms]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In newtype instances of data families we eta-reduce the axioms,
See Note [Eta reduction for data families] in FamInstEnv. This means that
we sometimes need to test compatibility of two axioms that were eta-reduced to
different degrees, e.g.:


data family D a b c
newtype instance D a Int c = DInt (Maybe a)
  -- D a Int ~ Maybe
  -- lhs = [a, Int]
newtype instance D Bool Int Char = DIntChar Float
  -- D Bool Int Char ~ Float
  -- lhs = [Bool, Int, Char]

These are obviously incompatible. We could detect this by saturating
(eta-expanding) the shorter LHS with fresh tyvars until the lists are of
equal length, but instead we can just remove the tail of the longer list, as
those types will simply unify with the freshly introduced tyvars.

By doing this, in case the LHS are unifiable, the yielded substitution won't
mention the tyvars that appear in the tail we dropped off, and we might try
to test equality RHSes of different kinds, but that's fine since this case
occurs only for data families, where the RHS is a unique tycon and the equality
fails anyway.
See Note [Compatibility]


Note [Tidy axioms when we build them]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Like types and classes, we build axioms fully quantified over all
their variables, and tidy them when we build them. For example,
we print out axioms and don't want to print stuff like
    F k k a b = ...
Instead we must tidy those kind variables.  See #7524.

We could instead tidy when we print, but that makes it harder to get
things like injectivity errors to come out right. Danger of
     Type family equation violates injectivity annotation.
     Kind variable ‘k’ cannot be inferred from the right-hand side.
     In the type family equation:
        PolyKindVars @[k1] @[k2] ('[] @k1) = '[] @k2



Note [Always number wildcard types in CoAxBranch]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider the following example (from the DataFamilyInstanceLHS test case):

  data family Sing (a :: k)
  data instance Sing (_ :: MyKind) where
      SingA :: Sing A
      SingB :: Sing B

If we're not careful during tidying, then when this program is compiled with
-ddump-types, we'll get the following information:

  COERCION AXIOMS
    axiom DataFamilyInstanceLHS.D:R:SingMyKind_0 ::
      Sing _ = DataFamilyInstanceLHS.R:SingMyKind_ _

It's misleading to have a wildcard type appearing on the RHS like
that. To avoid this issue, when building a CoAxiom (which is what eventually
gets printed above), we tidy all the variables in an env that already contains
'_'. Thus, any variable named '_' will be renamed, giving us the nicer output
here:

  COERCION AXIOMS
    axiom DataFamilyInstanceLHS.D:R:SingMyKind_0 ::
      Sing _1 = DataFamilyInstanceLHS.R:SingMyKind_ _1

Which is at least legal syntax.

See also Note [CoAxBranch type variables] in CoAxiom; note that we
are tidying (changing OccNames only), not freshening, in accordance with
that Note.
all axiom roles are Nominal, as this is only used with type families


Note [Verifying injectivity annotation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Injectivity means that the RHS of a type family uniquely determines the LHS (see
Note [Type inference for type families with injectivity]).  User informs about
injectivity using an injectivity annotation and it is GHC's task to verify that
that annotation is correct wrt. to type family equations. Whenever we see a new
equation of a type family we need to make sure that adding this equation to
already known equations of a type family does not violate injectivity annotation
supplied by the user (see Note [Injectivity annotation]).  Of course if the type
family has no injectivity annotation then no check is required.  But if a type
family has injectivity annotation we need to make sure that the following
conditions hold:

1. For each pair of *different* equations of a type family, one of the following
   conditions holds:

   A:  RHSs are different.

   B1: OPEN TYPE FAMILIES: If the RHSs can be unified under some substitution
       then it must be possible to unify the LHSs under the same substitution.
       Example:

          type family FunnyId a = r | r -> a
          type instance FunnyId Int = Int
          type instance FunnyId a = a

       RHSs of these two equations unify under [ a |-> Int ] substitution.
       Under this substitution LHSs are equal therefore these equations don't
       violate injectivity annotation.

   B2: CLOSED TYPE FAMILIES: If the RHSs can be unified under some
       substitution then either the LHSs unify under the same substitution or
       the LHS of the latter equation is overlapped by earlier equations.
       Example 1:

          type family SwapIntChar a = r | r -> a where
              SwapIntChar Int  = Char
              SwapIntChar Char = Int
              SwapIntChar a    = a

       Say we are checking the last two equations. RHSs unify under [ a |->
       Int ] substitution but LHSs don't. So we apply the substitution to LHS
       of last equation and check whether it is overlapped by any of previous
       equations. Since it is overlapped by the first equation we conclude
       that pair of last two equations does not violate injectivity
       annotation.

   A special case of B is when RHSs unify with an empty substitution ie. they
   are identical.

   If any of the above two conditions holds we conclude that the pair of
   equations does not violate injectivity annotation. But if we find a pair
   of equations where neither of the above holds we report that this pair
   violates injectivity annotation because for a given RHS we don't have a
   unique LHS. (Note that (B) actually implies (A).)

   Note that we only take into account these LHS patterns that were declared
   as injective.

2. If a RHS of a type family equation is a bare type variable then
   all LHS variables (including implicit kind variables) also have to be bare.
   In other words, this has to be a sole equation of that type family and it has
   to cover all possible patterns.  So for example this definition will be
   rejected:

      type family W1 a = r | r -> a
      type instance W1 [a] = a

   If it were accepted we could call `W1 [W1 Int]`, which would reduce to
   `W1 Int` and then by injectivity we could conclude that `[W1 Int] ~ Int`,
   which is bogus.

3. If a RHS of a type family equation is a type family application then the type
   family is rejected as not injective.

4. If a LHS type variable that is declared as injective is not mentioned on
   injective position in the RHS then the type family is rejected as not
   injective.  "Injective position" means either an argument to a type
   constructor or argument to a type family on injective position.

See also Note [Injective type families] in TyCon


Note [Family instance overlap conflicts]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
- In the case of data family instances, any overlap is fundamentally a
  conflict (as these instances imply injective type mappings).

- In the case of type family instances, overlap is admitted as long as
  the right-hand sides of the overlapping rules coincide under the
  overlap substitution.  eg
       type instance F a Int = a
       type instance F Int b = b
  These two overlap on (F Int Int) but then both RHSs are Int,
  so all is well. We require that they are syntactically equal;
  anything else would be difficult to test for at this stage.
----------------------------------------------------------
 Might be a one-way match or a unifier


Note [Over-saturated matches]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It's ok to look up an over-saturated type constructor.  E.g.
     type family F a :: * -> *
     type instance F (a,b) = Either (a->b)

The type instance gives rise to a newtype TyCon (at a higher kind
which you can't do in Haskell!):
     newtype FPair a b = FP (Either (a->b))

Then looking up (F (Int,Bool) Char) will return a FamInstMatch
     (FPair, [Int,Bool,Char])
The "extra" type argument [Char] just stays on the end.

We handle data families and type families separately here:

 * For type families, all instances of a type family must have the
   same arity, so we can precompute the split between the match_tys
   and the overflow tys. This is done in pre_rough_split_tys.

 * For data family instances, though, we need to re-split for each
   instance, because the breakdown might be different for each
   instance.  Why?  Because of eta reduction; see
   Note [Eta reduction for data families].
checks if one LHS is dominated by a list of other branches
in other words, if an application would match the first LHS, it is guaranteed
to match at least one of the others. The RHSs are ignored.
This algorithm is conservative:
  True -> the LHS is definitely covered by the others
  False -> no information
It is currently (Oct 2012) used only for generating errors for
inaccessible branches. If these errors go unreported, no harm done.
This is defined here to avoid a dependency from CoAxiom to Unify


Note [Normalising types]
~~~~~~~~~~~~~~~~~~~~~~~~
The topNormaliseType function removes all occurrences of type families
and newtypes from the top-level structure of a type. normaliseTcApp does
the type family lookup and is fairly straightforward. normaliseType is
a little more involved.

The complication comes from the fact that a type family might be used in the
kind of a variable bound in a forall. We wish to remove this type family
application, but that means coming up with a fresh variable (with the new
kind). Thus, we need a substitution to be built up as we recur through the
type. However, an ordinary TCvSubst just won't do: when we hit a type variable
whose kind has changed during normalisation, we need both the new type
variable *and* the coercion. We could conjure up a new VarEnv with just this
property, but a usable substitution environment already exists:
LiftingContexts from the liftCoSubst family of functions, defined in Coercion.
A LiftingContext maps a type variable to a coercion and a coercion variable to
a pair of coercions. Let's ignore coercion variables for now. Because the
coercion a type variable maps to contains the destination type (via
coercionKind), we don't need to store that destination type separately. Thus,
a LiftingContext has what we need: a map from type variables to (Coercion,
Type) pairs.

We also benefit because we can piggyback on the liftCoSubstVarBndr function to
deal with binders. However, I had to modify that function to work with this
application. Thus, we now have liftCoSubstVarBndrUsing, which takes
a function used to process the kind of the binder. We don't wish
to lift the kind, but instead normalise it. So, we pass in a callback function
that processes the kind of the binder.

After that brilliant explanation of all this, I'm sure you've forgotten the
dangling reference to coercion variables. What do we do with those? Nothing at
all. The point of normalising types is to remove type family applications, but
there's no sense in removing these from coercions. We would just get back a
new coercion witnessing the equality between the same types as the original
coercion. Because coercions are irrelevant anyway, there is no point in doing
this. So, whenever we encounter a coercion, we just say that it won't change.
That's what the CoercionTy case is doing within normalise_type.



Note [Normalisation and type synonyms]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We need to be a bit careful about normalising in the presence of type
synonyms (#13035).  Suppose S is a type synonym, and we have
   S t1 t2
If S is family-free (on its RHS) we can just normalise t1 and t2 and
reconstruct (S t1' t2').   Expanding S could not reveal any new redexes
because type families are saturated.

But if S has a type family on its RHS we expand /before/ normalising
the args t1, t2.  If we normalise t1, t2 first, we'll re-normalise them
after expansion, and that can lead to /exponential/ behavour; see #13035.

Notice, though, that expanding first can in principle duplicate t1,t2,
which might contain redexes. I'm sure you could conjure up an exponential
case by that route too, but it hasn't happened in practice yet!


Note [Flattening]
~~~~~~~~~~~~~~~~~
As described in "Closed type families with overlapping equations"
http://research.microsoft.com/en-us/um/people/simonpj/papers/ext-f/axioms-extended.pdf
we need to flatten core types before unifying them, when checking for "surely-apart"
against earlier equations of a closed type family.
Flattening means replacing all top-level uses of type functions with
fresh variables, *taking care to preserve sharing*. That is, the type
(Either (F a b) (F a b)) should flatten to (Either c c), never (Either
c d).

Here is a nice example of why it's all necessary:

  type family F a b where
    F Int Bool = Char
    F a   b    = Double
  type family G a         -- open, no instances

How do we reduce (F (G Float) (G Float))? The first equation clearly doesn't match,
while the second equation does. But, before reducing, we must make sure that the
target can never become (F Int Bool). Well, no matter what G Float becomes, it
certainly won't become *both* Int and Bool, so indeed we're safe reducing
(F (G Float) (G Float)) to Double.

This is necessary not only to get more reductions (which we might be
willing to give up on), but for substitutivity. If we have (F x x), we
can see that (F x x) can reduce to Double. So, it had better be the
case that (F blah blah) can reduce to Double, no matter what (blah)
is!  Flattening as done below ensures this.

flattenTys is defined here because of module dependencies.
