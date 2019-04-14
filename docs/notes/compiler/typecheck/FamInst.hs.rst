`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/FamInst.hs>`_

compiler/typecheck/FamInst.hs
=============================


Note [The type family instance consistency story]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/FamInst.hs#L49>`__

To preserve type safety we must ensure that for any given module, all
the type family instances used either in that module or in any module
it directly or indirectly imports are consistent. For example, consider

  module F where
    type family F a

::

  module A where
    import F( F )
    type instance F Int = Bool
    f :: F Int -> Bool
    f x = x

..

::

  module B where
    import F( F )
    type instance F Int = Char
    g :: Char -> F Int
    g x = x

..

::

  module Bad where
    import A( f )
    import B( g )
    bad :: Char -> Int
    bad c = f (g c)

..

Even though module Bad never mentions the type family F at all, by
combining the functions f and g that were type checked in contradictory
type family instance environments, the function bad is able to coerce
from one type to another. So when we type check Bad we must verify that
the type family instances defined in module A are consistent with those
defined in module B.

How do we ensure that we maintain the necessary consistency?

* Call a module which defines at least one type family instance a
  "family instance module". This flag `mi_finsts` is recorded in the
  interface file.

* For every module we calculate the set of all of its direct and
  indirect dependencies that are family instance modules. This list
  `dep_finsts` is also recorded in the interface file so we can compute
  this list for a module from the lists for its direct dependencies.

* When type checking a module M we check consistency of all the type
  family instances that are either provided by its `dep_finsts` or
  defined in the module M itself. This is a pairwise check, i.e., for
  every pair of instances we must check that they are consistent.

  - For family instances coming from `dep_finsts`, this is checked in
    checkFamInstConsistency, called from tcRnImports. See Note
    [Checking family instance consistency] for details on this check
    (and in particular how we avoid having to do all these checks for
    every module we compile).

  - That leaves checking the family instances defined in M itself
    against instances defined in either M or its `dep_finsts`. This is
    checked in `tcExtendLocalFamInstEnv'.

There are four subtle points in this scheme which have not been
addressed yet.

* We have checked consistency of the family instances *defined* by M
  or its imports, but this is not by definition the same thing as the
  family instances *used* by M or its imports.  Specifically, we need to
  ensure when we use a type family instance while compiling M that this
  instance was really defined from either M or one of its imports,
  rather than being an instance that we happened to know about from
  reading an interface file in the course of compiling an unrelated
  module. Otherwise, we'll end up with no record of the fact that M
  depends on this family instance and type safety will be compromised.
  See #13102.

* It can also happen that M uses a function defined in another module
  which is not transitively imported by M. Examples include the
  desugaring of various overloaded constructs, and references inserted
  by Template Haskell splices. If that function's definition makes use
  of type family instances which are not checked against those visible
  from M, type safety can again be compromised. See #13251.

* When a module C imports a boot module B.hs-boot, we check that C's
  type family instances are compatible with those visible from
  B.hs-boot. However, C will eventually be linked against a different
  module B.hs, which might define additional type family instances which
  are inconsistent with C's. This can also lead to loss of type safety.
  See #9562.

* The call to checkFamConsistency for imported functions occurs very
  early (in tcRnImports) and that causes problems if the imported
  instances use type declared in the module being compiled.
  See Note [Loading your own hi-boot file] in LoadIface.



Note [Checking family instance consistency]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/FamInst.hs#L213>`__

For any two family instance modules that we import directly or indirectly, we
check whether the instances in the two modules are consistent, *unless* we can
be certain that the instances of the two modules have already been checked for
consistency during the compilation of modules that we import.

Why do we need to check?  Consider
   module X1 where                module X2 where
    data T1                         data T2
    type instance F T1 b = Int      type instance F a T2 = Char
    f1 :: F T1 a -> Int             f2 :: Char -> F a T2
    f1 x = x                        f2 x = x

Now if we import both X1 and X2 we could make (f2 . f1) :: Int -> Char.
Notice that neither instance is an orphan.

How do we know which pairs of modules have already been checked? For each
module M we directly import, we look up the family instance modules that M
imports (directly or indirectly), say F1, ..., FN. For any two modules
among M, F1, ..., FN, we know that the family instances defined in those
two modules are consistent--because we checked that when we compiled M.

For every other pair of family instance modules we import (directly or
indirectly), we check that they are consistent now. (So that we can be
certain that the modules in our `HscTypes.dep_finsts' are consistent.)

There is some fancy footwork regarding hs-boot module loops, see
Note [Don't check hs-boot type family instances too early]



Note [Checking family instance optimization]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/FamInst.hs#L243>`__

As explained in Note [Checking family instance consistency]
we need to ensure that every pair of transitive imports that define type family
instances is consistent.

Let's define df(A) = transitive imports of A that define type family instances
+ A, if A defines type family instances

Then for every direct import A, df(A) is already consistent.

Let's name the current module M.

We want to make sure that df(M) is consistent.
df(M) = df(D_1) U df(D_2) U ... U df(D_i) where D_1 .. D_i are direct imports.

We perform the check iteratively, maintaining a set of consistent modules 'C'
and trying to add df(D_i) to it.

The key part is how to ensure that the union C U df(D_i) is consistent.

Let's consider two modules: A and B from C U df(D_i).
There are nine possible ways to choose A and B from C U df(D_i):

             | A in C only      | A in C and B in df(D_i) | A in df(D_i) only
--------------------------------------------------------------------------------
B in C only  | Already checked  | Already checked         | Needs to be checked
             | when checking C  | when checking C         |
--------------------------------------------------------------------------------
B in C and   | Already checked  | Already checked         | Already checked when
B in df(D_i) | when checking C  | when checking C         | checking df(D_i)
--------------------------------------------------------------------------------
B in df(D_i) | Needs to be      | Already checked         | Already checked when
only         | checked          | when checking df(D_i)   | checking df(D_i)

That means to ensure that C U df(D_i) is consistent we need to check every
module from C - df(D_i) against every module from df(D_i) - C and
every module from df(D_i) - C against every module from C - df(D_i).
But since the checks are symmetric it suffices to pick A from C - df(D_i)
and B from df(D_i) - C.

In other words these are the modules we need to check:
  [ (m1, m2) | m1 <- C, m1 not in df(D_i)
             , m2 <- df(D_i), m2 not in C ]

One final thing to note here is that if there's lot of overlap between
subsequent df(D_i)'s then we expect those set differences to be small.
That situation should be pretty common in practice, there's usually
a set of utility modules that every module imports directly or indirectly.

This is basically the idea from #13092, comment:14.

This function doesn't check ALL instances for consistency,
only ones that aren't involved in recursive knot-tying
loops; see Note [Don't check hs-boot type family instances too early].
We don't need to check the current module, this is done in
tcExtendLocalFamInstEnv.
See Note [The type family instance consistency story].



Note [Home package family instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/FamInst.hs#L637>`__

Optimization: If we're only defining type family instances
for type families *defined in the home package*, then we
only have to load interface files that belong to the home
package. The reason is that there's no recursion between
packages, so modules in other packages can't possibly define
instances for our type families.

(Within the home package, we could import a module M that
imports us via an hs-boot file, and thereby defines an
instance of a type family defined in this module. So we can't
apply the same logic to avoid reading any interface files at
all, when we define an instances for type family defined in
the current module.

Check that the proposed new instance is OK,
and then add it to the home inst env
This must be lazy in the fam_inst arguments, see Note [Lazy axiom match]
in FamInstEnv.hs

