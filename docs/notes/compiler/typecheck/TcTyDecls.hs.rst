`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcTyDecls.hs>`_

Note [Superclass cycle check]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The superclass cycle check for C decides if we can statically
guarantee that expanding C's superclass cycles transitively is
guaranteed to terminate.  This is a Haskell98 requirement,
but one that we lift with -XUndecidableSuperClasses.

The worry is that a superclass cycle could make the type checker loop.
More precisely, with a constraint (Given or Wanted)
    C ty1 .. tyn
one approach is to instantiate all of C's superclasses, transitively.
We can only do so if that set is finite.

This potential loop occurs only through superclasses.  This, for
example, is fine
  class C a where
    op :: C b => a -> b -> b
even though C's full definition uses C.

Making the check static also makes it conservative.  Eg
  type family F a
  class F a => C a
Here an instance of (F a) might mention C:
  type instance F [a] = C a
and now we'd have a loop.

The static check works like this, starting with C
  * Look at C's superclass predicates
  * If any is a type-function application,
    or is headed by a type variable, fail
  * If any has C at the head, fail
  * If any has a type class D at the head,
    make the same test with D

A tricky point is: what if there is a type variable at the head?
Consider this:
   class f (C f) => C f
   class c       => Id c
and now expand superclasses for constraint (C Id):
     C Id
 --> Id (C Id)
 --> C Id
 --> ....
Each step expands superclasses one layer, and clearly does not terminate.


Note [Role inference]
~~~~~~~~~~~~~~~~~~~~~
The role inference algorithm datatype definitions to infer the roles on the
parameters. Although these roles are stored in the tycons, we can perform this
algorithm on the built tycons, as long as we don't peek at an as-yet-unknown
roles field! Ah, the magic of laziness.

First, we choose appropriate initial roles. For families and classes, roles
(including initial roles) are N. For datatypes, we start with the role in the
role annotation (if any), or otherwise use Phantom. This is done in
initialRoleEnv1.

The function irGroup then propagates role information until it reaches a
fixpoint, preferring N over (R or P) and R over P. To aid in this, we have a
monad RoleM, which is a combination reader and state monad. In its state are
the current RoleEnv, which gets updated by role propagation, and an update
bit, which we use to know whether or not we've reached the fixpoint. The
environment of RoleM contains the tycon whose parameters we are inferring, and
a VarEnv from parameters to their positions, so we can update the RoleEnv.
Between tycons, this reader information is missing; it is added by
addRoleInferenceInfo.

There are two kinds of tycons to consider: algebraic ones (excluding classes)
and type synonyms. (Remember, families don't participate -- all their parameters
are N.) An algebraic tycon processes each of its datacons, in turn. Note that
a datacon's universally quantified parameters might be different from the parent
tycon's parameters, so we use the datacon's univ parameters in the mapping from
vars to positions. Note also that we don't want to infer roles for existentials
(they're all at N, too), so we put them in the set of local variables. As an
optimisation, we skip any tycons whose roles are already all Nominal, as there
nowhere else for them to go. For synonyms, we just analyse their right-hand sides.

irType walks through a type, looking for uses of a variable of interest and
propagating role information. Because anything used under a phantom position
is at phantom and anything used under a nominal position is at nominal, the
irType function can assume that anything it sees is at representational. (The
other possibilities are pruned when they're encountered.)

The rest of the code is just plumbing.

How do we know that this algorithm is correct? It should meet the following
specification:

Let Z be a role context -- a mapping from variables to roles. The following
rules define the property (Z |- t : r), where t is a type and r is a role:

Z(a) = r'        r' <= r
------------------------- RCVar
Z |- a : r

---------- RCConst
Z |- T : r               -- T is a type constructor

Z |- t1 : r
Z |- t2 : N
-------------- RCApp
Z |- t1 t2 : r

forall i<=n. (r_i is R or N) implies Z |- t_i : r_i
roles(T) = r_1 .. r_n
---------------------------------------------------- RCDApp
Z |- T t_1 .. t_n : R

Z, a:N |- t : r
---------------------- RCAll
Z |- forall a:k.t : r


We also have the following rules:

For all datacon_i in type T, where a_1 .. a_n are universally quantified
and b_1 .. b_m are existentially quantified, and the arguments are t_1 .. t_p,
then if forall j<=p, a_1 : r_1 .. a_n : r_n, b_1 : N .. b_m : N |- t_j : R,
then roles(T) = r_1 .. r_n

roles(->) = R, R
roles(~#) = N, N

With -dcore-lint on, the output of this algorithm is checked in checkValidRoles,
called from checkValidTycon.



Note [Role-checking data constructor arguments]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
  data T a where
    MkT :: Eq b => F a -> (a->a) -> T (G a)

Then we want to check the roles at which 'a' is used
in MkT's type.  We want to work on the user-written type,
so we need to take into account
  * the arguments:   (F a) and (a->a)
  * the context:     C a b
  * the result type: (G a)   -- this is in the eq_spec




Note [Coercions in role inference]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Is (t |> co1) representationally equal to (t |> co2)? Of course they are! Changing
the kind of a type is totally irrelevant to the representation of that type. So,
we want to totally ignore coercions when doing role inference. This includes omitting
any type variables that appear in nominal positions but only within coercions.


Note [Default roles for abstract TyCons in hs-boot/hsig]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
What should the default role for an abstract TyCon be?

Originally, we inferred phantom role for abstract TyCons
in hs-boot files, because the type variables were never used.

This was silly, because the role of the abstract TyCon
was required to match the implementation, and the roles of
data types are almost never phantom.  Thus, in ticket #9204,
the default was changed so be representational (the most common case).  If
the implementing data type was actually nominal, you'd get an easy
to understand error, and add the role annotation yourself.

Then Backpack was added, and with it we added role *subtyping*
the matching judgment: if an abstract TyCon has a nominal
parameter, it's OK to implement it with a representational
parameter.  But now, the representational default is not a good
one, because you should *only* request representational if
you're planning to do coercions. To be maximally flexible
with what data types you will accept, you want the default
for hsig files is nominal.  We don't allow role subtyping
with hs-boot files (it's good practice to give an exactly
accurate role here, because any types that use the abstract
type will propagate the role information.)


Note [Default method Ids and Template Haskell]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this (#4169):
   class Numeric a where
     fromIntegerNum :: a
     fromIntegerNum = ...

   ast :: Q [Dec]
   ast = [d| instance Numeric Int |]

When we typecheck 'ast' we have done the first pass over the class decl
(in tcTyClDecls), but we have not yet typechecked the default-method
declarations (because they can mention value declarations).  So we
must bring the default method Ids into scope first (so they can be seen
when typechecking the [d| .. |] quote, and typecheck them later.


Note [Polymorphic selectors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We take care to build the type of a polymorphic selector in the right
order, so that visible type application works.

  data Ord a => T a = MkT { field :: forall b. (Num a, Show b) => (a, b) }

We want

  field :: forall a. Ord a => T a -> forall b. (Num a, Show b) => (a, b)



Note [Naughty record selectors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A "naughty" field is one for which we can't define a record
selector, because an existential type variable would escape.  For example:
        data T = forall a. MkT { x,y::a }
We obviously can't define
        x (MkT v _) = v
Nevertheless we *do* put a RecSelId into the type environment
so that if the user tries to use 'x' as a selector we can bleat
helpfully, rather than saying unhelpfully that 'x' is not in scope.
Hence the sel_naughty flag, to identify record selectors that don't really exist.

In general, a field is "naughty" if its type mentions a type variable that
isn't in the result type of the constructor.  Note that this *allows*
GADT record selectors (Note [GADT record selectors]) whose types may look
like     sel :: T [a] -> a

For naughty selectors we make a dummy binding
   sel = ()
so that the later type-check will add them to the environment, and they'll be
exported.  The function is never called, because the typechecker spots the
sel_naughty field.



Note [GADT record selectors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For GADTs, we require that all constructors with a common field 'f' have the same
result type (modulo alpha conversion).  [Checked in TcTyClsDecls.checkValidTyCon]
E.g.
        data T where
          T1 { f :: Maybe a } :: T [a]
          T2 { f :: Maybe a, y :: b  } :: T [a]
          T3 :: T Int

and now the selector takes that result type as its argument:
   f :: forall a. T [a] -> Maybe a

Details: the "real" types of T1,T2 are:
   T1 :: forall r a.   (r~[a]) => a -> T r
   T2 :: forall r a b. (r~[a]) => a -> b -> T r

So the selector loooks like this:
   f :: forall a. T [a] -> Maybe a
   f (a:*) (t:T [a])
     = case t of
         T1 c   (g:[a]~[c]) (v:Maybe c)       -> v `cast` Maybe (right (sym g))
         T2 c d (g:[a]~[c]) (v:Maybe c) (w:d) -> v `cast` Maybe (right (sym g))
         T3 -> error "T3 does not have field f"

Note the forall'd tyvars of the selector are just the free tyvars
of the result type; there may be other tyvars in the constructor's
type (e.g. 'b' in T2).

Note the need for casts in the result!



Note [Selector running example]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It's OK to combine GADTs and type families.  Here's a running example:

        data instance T [a] where
          T1 { fld :: b } :: T [Maybe b]

The representation type looks like this
        data :R7T a where
          T1 { fld :: b } :: :R7T (Maybe b)

and there's coercion from the family type to the representation type
        :CoR7T a :: T [a] ~ :R7T a

The selector we want for fld looks like this:

        fld :: forall b. T [Maybe b] -> b
        fld = /\b. \(d::T [Maybe b]).
              case d `cast` :CoR7T (Maybe b) of
                T1 (x::b) -> x

The scrutinee of the case has type :R7T (Maybe b), which can be
gotten by appying the eq_spec to the univ_tvs of the data con.


