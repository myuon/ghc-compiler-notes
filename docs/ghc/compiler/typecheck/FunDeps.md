[[src]](https://github.com/ghc/ghc/tree/master/compiler/typecheck/FunDeps.hs)

(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 2000


FunDeps - functional dependencies

It's better to read it as: "if we know these, then we're going to know these"


# \subsection{Generate equations from functional dependencies}



Each functional dependency with one variable in the RHS is responsible
for generating a single equality. For instance:
     class C a b | a -> b
The constraints ([Wanted] C Int Bool) and [Wanted] C Int alpha
will generate the following FunDepEqn
     FDEqn { fd_qtvs = []
           , fd_eqs  = [Pair Bool alpha]
           , fd_pred1 = C Int Bool
           , fd_pred2 = C Int alpha
           , fd_loc = ... }
However notice that a functional dependency may have more than one variable
in the RHS which will create more than one pair of types in fd_eqs. Example:
     class C a b c | a -> b c
     [Wanted] C Int alpha alpha
     [Wanted] C Int Bool beta
Will generate:
     FDEqn { fd_qtvs = []
           , fd_eqs  = [Pair Bool alpha, Pair alpha beta]
           , fd_pred1 = C Int Bool
           , fd_pred2 = C Int alpha
           , fd_loc = ... }

INVARIANT: Corresponding types aren't already equal
That is, there exists at least one non-identity equality in FDEqs.

Assume:
       class C a b c | a -> b c
       instance C Int x x
And:   [Wanted] C Int Bool alpha
We will /match/ the LHS of fundep equations, producing a matching substitution
and create equations for the RHS sides. In our last example we'd have generated:
      ({x}, [fd1,fd2])
where
       fd1 = FDEq 1 Bool x
       fd2 = FDEq 2 alpha x
To ``execute'' the equation, make fresh type variable for each tyvar in the set,
instantiate the two types with these fresh variables, and then unify or generate
a new constraint. In the above example we would generate a new unification
variable 'beta' for x and produce the following constraints:
     [Wanted] (Bool ~ beta)
     [Wanted] (alpha ~ beta)

Notice the subtle difference between the above class declaration and:
       class C a b c | a -> b, a -> c
where we would generate:
      ({x},[fd1]),({x},[fd2])
This means that the template variable would be instantiated to different
unification variables when producing the FD constraints.

Finally, the position parameters will help us rewrite the wanted constraint ``on the spot''



Given a bunch of predicates that must hold, such as

        C Int t1, C Int t2, C Bool t3, ?x::t4, ?x::t5

improve figures out what extra equations must hold.
For example, if we have

        class C a b | a->b where ...

then improve will return

        [(t1,t2), (t4,t5)]

NOTA BENE:

  * improve does not iterate.  It's possible that when we make
    t1=t2, for example, that will in turn trigger a new equation.
    This would happen if we also had
        C t1 t7, C t2 t8
    If t1=t2, we also get t7=t8.

    improve does *not* do this extra step.  It relies on the caller
    doing so.

  * The equations unify types that are not already equal.  So there
    is no effect iff the result of improve is empty


# 

### Note: Coverage condition

Example
      class C a b | a -> b
      instance theta => C t1 t2

For the coverage condition, we check
   (normal)    fv(t2) `subset` fv(t1)
   (liberal)   fv(t2) `subset` oclose(fv(t1), theta)

The liberal version  ensures the self-consistency of the instance, but
it does not guarantee termination. Example:

   class Mul a b c | a b -> c where
        (.*.) :: a -> b -> c

   instance Mul Int Int Int where (.*.) = (*)
   instance Mul Int Float Float where x .*. y = fromIntegral x * y
   instance Mul a b c => Mul a [b] [c] where x .*. v = map (x.*.) v

In the third instance, it's not the case that fv([c]) `subset` fv(a,[b]).
But it is the case that fv([c]) `subset` oclose( theta, fv(a,[b]) )

But it is a mistake to accept the instance because then this defn:
        f = \ b x y -> if b then x .*. [y] else y
makes instance inference go into a loop, because it requires the constraint
        Mul a [b] b


### Note: Closing over kinds in coverage

Suppose we have a fundep  (a::k) -> b
Then if 'a' is instantiated to (x y), where x:k2->*, y:k2,
then fixing x really fixes k2 as well, and so k2 should be added to
the lhs tyvars in the fundep check.

Example (Trac #8391), using liberal coverage
      data Foo a = ...  -- Foo :: forall k. k -> *
      class Bar a b | a -> b
      instance Bar a (Foo a)

    In the instance decl, (a:k) does fix (Foo k a), but only if we notice
    that (a:k) fixes k.  Trac #10109 is another example.

Here is a more subtle example, from HList-0.4.0.0 (Trac #10564)

  class HasFieldM (l :: k) r (v :: Maybe *)
        | l r -> v where ...
  class HasFieldM1 (b :: Maybe [*]) (l :: k) r v
        | b l r -> v where ...
  class HMemberM (e1 :: k) (l :: [k]) (r :: Maybe [k])
        | e1 l -> r

  data Label :: k -> *
  type family LabelsOf (a :: [*]) ::  *

  instance (HMemberM (Label {k} (l::k)) (LabelsOf xs) b,
            HasFieldM1 b l (r xs) v)
         => HasFieldM l (r xs) v where

Is the instance OK? Does {l,r,xs} determine v?  Well:

  * From the instance constraint HMemberM (Label k l) (LabelsOf xs) b,
    plus the fundep "| el l -> r" in class HMameberM,
    we get {l,k,xs} -> b

  * Note the 'k'!! We must call closeOverKinds on the seed set
    ls_tvs = {l,r,xs}, BEFORE doing oclose, else the {l,k,xs}->b
    fundep won't fire.  This was the reason for #10564.

  * So starting from seeds {l,r,xs,k} we do oclose to get
    first {l,r,xs,k,b}, via the HMemberM constraint, and then
    {l,r,xs,k,b,v}, via the HasFieldM1 constraint.

  * And that fixes v.

However, we must closeOverKinds whenever augmenting the seed set
in oclose!  Consider Trac #10109:

  data Succ a   -- Succ :: forall k. k -> *
  class Add (a :: k1) (b :: k2) (ab :: k3) | a b -> ab
  instance (Add a b ab) => Add (Succ {k1} (a :: k1))
                               b
                               (Succ {k3} (ab :: k3})

We start with seed set {a:k1,b:k2} and closeOverKinds to {a,k1,b,k2}.
Now use the fundep to extend to {a,k1,b,k2,ab}.  But we need to
closeOverKinds *again* now to {a,k1,b,k2,ab,k3}, so that we fix all
the variables free in (Succ {k3} ab).

Bottom line:
  * closeOverKinds on initial seeds (done automatically
    by tyCoVarsOfTypes in checkInstCoverage)
  * and closeOverKinds whenever extending those seeds (in oclose)

### Note: The liberal coverage condition

(oclose preds tvs) closes the set of type variables tvs,
wrt functional dependencies in preds.  The result is a superset
of the argument set.  For example, if we have
        class C a b | a->b where ...
then
        oclose [C (x,y) z, C (x,p) q] {x,y} = {x,y,z}
because if we know x and y then that fixes z.

We also use equality predicates in the predicates; if we have an
assumption `t1 ~ t2`, then we use the fact that if we know `t1` we
also know `t2` and the other way.
  eg    oclose [C (x,y) z, a ~ x] {a,y} = {a,y,z,x}

oclose is used (only) when checking the coverage condition for
an instance declaration

### Note: Equality superclasses

Suppose we have
  class (a ~ [b]) => C a b

### Note: The equality types story

So when oclose expands superclasses we'll get a (a ~# [b]) superclass.
But that's an EqPred not a ClassPred, and we jolly well do want to
account for the mutual functional dependencies implied by (t1 ~# t2).
Hence the EqPred handling in oclose.  See Trac #10778.

### Note: Care with type functions

Consider (Trac #12803)
  class C x y | x -> y
  type family F a b
  type family G c d = r | r -> d

Now consider
  oclose (C (F a b) (G c d)) {a,b}

Knowing {a,b} fixes (F a b) regardless of the injectivity of F.
But knowing (G c d) fixes only {d}, because G is only injective
in its second parameter.

Hence the tyCoVarsOfTypes/injTyVarsOfTypes dance in tv_fds.


# Check that a new instance decl is OK wrt fundeps


Here is the bad case:
        class C a b | a->b where ...
        instance C Int Bool where ...
        instance C Int Char where ...

The point is that a->b, so Int in the first parameter must uniquely
determine the second.  In general, given the same class decl, and given

        instance C s1 s2 where ...
        instance C t1 t2 where ...

Then the criterion is: if U=unify(s1,t1) then U(s2) = U(t2).

Matters are a little more complicated if there are free variables in
the s2/t2.

        class D a b c | a -> b
        instance D a b => D [(a,a)] [b] Int
        instance D a b => D [a]     [b] Bool

The instance decls don't overlap, because the third parameter keeps
them separate.  But we want to make sure that given any constraint
        D s1 s2 s3
if s1 matches

### Note: Bogus consistency check

In checkFunDeps we check that a new ClsInst is consistent with all the
ClsInsts in the environment.

The bogus aspect is discussed in Trac #10675. Currenty it if the two
types are *contradicatory*, using (isNothing . tcUnifyTys).  But all
the papers say we should check if the two types are *equal* thus
   not (substTys subst rtys1 `eqTypes` substTys subst rtys2)
For now I'm leaving the bogus form because that's the way it has
been for years.
