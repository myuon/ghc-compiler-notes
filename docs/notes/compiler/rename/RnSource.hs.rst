`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/rename/RnSource.hs>`_

compiler/rename/RnSource.hs
===========================


Note [Wildcards in family instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/rename/RnSource.hs#L856>`__

Wild cards can be used in type/data family instance declarations to indicate
that the name of a type variable doesn't matter. Each wild card will be
replaced with a new unique type variable. For instance:

::

    type family F a b :: *
    type instance F Int _ = Int

is the same as

::

    type family F a b :: *
    type instance F Int b = Int

This is implemented as follows: Unnamed wildcards remain unchanged after
the renamer, and then given fresh meta-variables during typechecking, and
it is handled pretty much the same way as the ones in partial type signatures.
We however don't want to emit hole constraints on wildcards in family
instances, so we turn on PartialTypeSignatures and turn off warning flag to
let typechecker know this.
See related Note [Wildcards in visible kind application] in TcHsType.hs



Note [Unused type variables in family instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/rename/RnSource.hs#L878>`__

When the flag -fwarn-unused-type-patterns is on, the compiler reports
warnings about unused type variables in type-family instances. A
tpye variable is considered used (i.e. cannot be turned into a wildcard)
when

 * it occurs on the RHS of the family instance
   e.g.   type instance F a b = a    -- a is used on the RHS

 * it occurs multiple times in the patterns on the LHS
   e.g.   type instance F a a = Int  -- a appears more than once on LHS

 * it is one of the instance-decl variables, for associated types
   e.g.   instance C (a,b) where
            type T (a,b) = a
   Here the type pattern in the type instance must be the same as that
   for the class instance, so
            type T (a,_) = a
   would be rejected.  So we should not complain about an unused variable b

As usual, the warnings are not reported for type variables with names
beginning with an underscore.

Extra-constraints wild cards are not supported in type/data family
instance declarations.

Relevant tickets: #3699, #10586, #10982 and #11451.



Note [Renaming associated types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/rename/RnSource.hs#L907>`__

Check that the RHS of the decl mentions only type variables that are explicitly
bound on the LHS.  For example, this is not ok
   class C a b where
      type F a x :: *
   instance C (p,q) r where
      type F (p,q) x = (x, r)   -- BAD: mentions 'r'
c.f. #5515

Kind variables, on the other hand, are allowed to be implicitly or explicitly
bound. As examples, this (#9574) is acceptable:
   class Funct f where
      type Codomain f :: *
   instance Funct ('KProxy :: KProxy o) where
      -- o is implicitly bound by the kind signature
      -- of the LHS type pattern ('KProxy)
      type Codomain 'KProxy = NatTr (Proxy :: o -> *)
And this (#14131) is also acceptable:
    data family Nat :: k -> k -> *
    -- k is implicitly bound by an invisible kind pattern
    newtype instance Nat :: (k -> *) -> (k -> *) -> * where
      Nat :: (forall xx. f xx -> g xx) -> Nat f g
We could choose to disallow this, but then associated type families would not
be able to be as expressive as top-level type synonyms. For example, this type
synonym definition is allowed:
    type T = (Nothing :: Maybe a)
So for parity with type synonyms, we also allow:
    type family   T :: Maybe a
    type instance T = (Nothing :: Maybe a)

All this applies only for *instance* declarations.  In *class*
declarations there is no RHS to worry about, and the class variables
can all be in scope (#5862):
    class Category (x :: k -> k -> *) where
      type Ob x :: k -> Constraint
      id :: Ob x a => x a a
      (.) :: (Ob x a, Ob x b, Ob x c) => x b c -> x a b -> x a c
Here 'k' is in scope in the kind signature, just like 'x'.

Although type family equations can bind type variables with explicit foralls,
it need not be the case that all variables that appear on the RHS must be bound
by a forall. For instance, the following is acceptable:

::

   class C a where
     type T a b
   instance C (Maybe a) where
     type forall b. T (Maybe a) b = Either a b

Even though `a` is not bound by the forall, this is still accepted because `a`
was previously bound by the `instance C (Maybe a)` part. (see #16116).

In each case, the function which detects improperly bound variables on the RHS
is TcValidity.checkValidFamPats.



Note [Rule LHS validity checking]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/rename/RnSource.hs#L1071>`__

Check the shape of a transformation rule LHS.  Currently we only allow
LHSs of the form @(f e1 .. en)@, where @f@ is not one of the
@forall@'d variables.

We used restrict the form of the 'ei' to prevent you writing rules
with LHSs with a complicated desugaring (and hence unlikely to match);
(e.g. a case expression is not allowed: too elaborate.)

But there are legitimate non-trivial args ei, like sections and
lambdas.  So it seems simmpler not to check at all, and that is why
check_e is commented out.



Note [Role annotations in the renamer]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/rename/RnSource.hs#L1439>`__

We must ensure that a type's role annotation is put in the same group as the
proper type declaration. This is because role annotations are needed during
type-checking when creating the type's TyCon. So, rnRoleAnnots builds a
NameEnv (LRoleAnnotDecl Name) that maps a name to a role annotation for that
type, if any. Then, this map can be used to add the role annotations to the
groups after dependency analysis.

This process checks for duplicate role annotations, where we must be careful
to do the check *before* renaming to avoid calling all unbound names duplicates
of one another.

The renaming process, as usual, might identify and report errors for unbound
names. We exclude the annotations for unbound names in the annotation
environment to avoid spurious errors for orphaned annotations.

We then (in rnTyClDecls) do a check for orphan role annotations (role
annotations without an accompanying type decl). The check works by folding
over components (of type [[Either (TyClDecl Name) (InstDecl Name)]]), selecting
out the relevant role declarations for each group, as well as diminishing the
annotation environment. After the fold is complete, anything left over in the
name environment must be an orphan, and errors are generated.

An earlier version of this algorithm short-cut the orphan check by renaming
only with names declared in this module. But, this check is insufficient in
the case of staged module compilation (Template Haskell, GHCi).
See #8485. With the new lookup process (which includes types declared in other
modules), we get better error messages, too.



Note [Floating `via` type variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/rename/RnSource.hs#L1807>`__

Imagine the following `deriving via` clause:

::

    data Quux
      deriving Eq via (Const a Quux)

This should be rejected. Why? Because it would generate the following instance:

::

    instance Eq Quux where
      (==) = coerce @(Quux         -> Quux         -> Bool)
                    @(Const a Quux -> Const a Quux -> Bool)
                    (==) :: Const a Quux -> Const a Quux -> Bool

This instance is ill-formed, as the `a` in `Const a Quux` is unbound. The
problem is that `a` is never used anywhere in the derived class `Eq`. Since
`a` is bound but has no use sites, we refer to it as "floating".

We use the rnAndReportFloatingViaTvs function to check that any type renamed
within the context of the `via` deriving strategy actually uses all bound
`via` type variables, and if it doesn't, it throws an error.



Note [Renaming injectivity annotation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/rename/RnSource.hs#L1928>`__

During renaming of injectivity annotation we have to make several checks to
make sure that it is well-formed.  At the moment injectivity annotation
consists of a single injectivity condition, so the terms "injectivity
annotation" and "injectivity condition" might be used interchangeably.  See
Note [Injectivity annotation] for a detailed discussion of currently allowed
injectivity annotations.

Checking LHS is simple because the only type variable allowed on the LHS of
injectivity condition is the variable naming the result in type family head.
Example of disallowed annotation:

::

    type family Foo a b = r | b -> a

Verifying RHS of injectivity consists of checking that:

 1. only variables defined in type family head appear on the RHS (kind
    variables are also allowed).  Example of disallowed annotation:

::

       type family Foo a = r | r -> b

 2. for associated types the result variable does not shadow any of type
    class variables. Example of disallowed annotation:

::

       class Foo a b where
          type F a = b | b -> a

Breaking any of these assumptions results in an error.



Note [Stupid theta]
~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/rename/RnSource.hs#L2022>`__

#3850 complains about a regression wrt 6.10 for
     data Show a => T a
There is no reason not to allow the stupid theta if there are no data
constructors.  It's still stupid, but does no harm, and I don't want
to cause programs to break unnecessarily (notably HList).  So if there
are no data constructors we allow h98_style = True

