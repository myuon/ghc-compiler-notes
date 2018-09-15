[[src]](https://github.com/ghc/ghc/tree/master/compiler/rename/RnSource.hs)

(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

# Main pass of renamer

 | @rnSourceDecl@ "renames" declarations.
It simultaneously performs dependency analysis and precedence parsing.
It also does the following error checks:

* Checks that tyvars are used properly. This includes checking
  for undefined tyvars, and tyvars in contexts that are ambiguous.
  (Some of this checking has now been moved to module @TcMonoType@,
  since we don't have functional dependency information at this point.)

* Checks that all variable occurrences are defined.

* Checks the @(..)@ etc constraints in the export list.

Brings the binders of the group into scope in the appropriate places;
does NOT assume that anything is in scope already


# HsDoc stuff


# Source-code deprecations declarations


Check that the deprecated names are defined, are defined locally, and
that there are no duplicate deprecations.

It's only imported deprecations, dealt with in RnIfaces, that we
gather them together.


# \subsection{Annotation declarations}


# \subsection{Default declarations}


# \subsection{Foreign declarations}


# \subsection{Instance declarations}


### Note: Wildcards in family instances

Wild cards can be used in type/data family instance declarations to indicate
that the name of a type variable doesn't matter. Each wild card will be
replaced with a new unique type variable. For instance:

    type family F a b :: *
    type instance F Int _ = Int

is the same as

    type family F a b :: *
    type instance F Int b = Int

This is implemented as follows: during renaming anonymous wild cards
'_' are given freshly generated names. These names are collected after
renaming (rnFamInstEqn) and used to make new type variables during
type checking (tc_fam_ty_pats). One should not confuse these wild
cards with the ones from partial type signatures. The latter generate
fresh meta-variables whereas the former generate fresh skolems.

### Note: Unused type variables in family instances

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

As usual, the warnings are not reported for for type variables with names
beginning with an underscore.

Extra-constraints wild cards are not supported in type/data family
instance declarations.

Relevant tickets: #3699, #10586, #10982 and #11451.

### Note: Renaming associated types

Check that the RHS of the decl mentions only type variables that are explicitly
bound on the LHS.  For example, this is not ok
   class C a b where
      type F a x :: *
   instance C (p,q) r where
      type F (p,q) x = (x, r)   -- BAD: mentions 'r'
c.f. Trac #5515

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
can all be in scope (Trac #5862):
    class Category (x :: k -> k -> *) where
      type Ob x :: k -> Constraint
      id :: Ob x a => x a a
      (.) :: (Ob x a, Ob x b, Ob x c) => x b c -> x a b -> x a c
Here 'k' is in scope in the kind signature, just like 'x'.


# \subsection{Stand-alone deriving declarations}


# \subsection{Rules}


### Note: Rule LHS validity checking

Check the shape of a transformation rule LHS.  Currently we only allow
LHSs of the form @(f e1 .. en)@, where @f@ is not one of the
@forall@'d variables.

We used restrict the form of the 'ei' to prevent you writing rules
with LHSs with a complicated desugaring (and hence unlikely to match);
(e.g. a case expression is not allowed: too elaborate.)

But there are legitimate non-trivial args ei, like sections and
lambdas.  So it seems simmpler not to check at all, and that is why
check_e is commented out.


### Note: Rule LHS validity checking

    check_e (OpApp e1 op _ e2)   = checkl_e e1 `mplus` checkl_e op `mplus` checkl_e e2
    check_e (HsApp e1 e2)        = checkl_e e1 `mplus` checkl_e e2
    check_e (NegApp e _)         = checkl_e e
    check_e (ExplicitList _ es)  = checkl_es es
    check_e other                = Just other   -- Fails

    checkl_es es = foldr (mplus . checkl_e) Nothing es


# \subsection{Vectorisation declarations}


# Renaming type, class, instance and role declarations


@rnTyDecl@ uses the `global name function' to create a new type
declaration in which local names have been replaced by their original
names, reporting any unknown names.

Renaming type variables is a pain. Because they now contain uniques,
it is necessary to pass in an association list which maps a parsed
tyvar to its @Name@ representation.
In some cases (type signatures of values),
it is even necessary to go over the type first
in order to get the set of tyvars used by it, make an assoc list,
and then go over it again to rename the tyvars!
However, we can also do some scoping checks at the same time.

### Note: Dependency analysis of type, class, and instance decls

A TyClGroup represents a strongly connected components of
type/class/instance decls, together with the role annotations for the
type/class declarations.  The renamer uses strongly connected
comoponent analysis to build these groups.  We do this for a number of
reasons:

* Improve kind error messages. Consider

     data T f a = MkT f a
     data S f a = MkS f (T f a)

  This has a kind error, but the error message is better if you
  check T first, (fixing its kind) and *then* S.  If you do kind
  inference together, you might get an error reported in S, which
  is jolly confusing.  See Trac #4875

### Note: Grouping of type and class declarations

Why do the instance declarations participate?  At least two reasons

* Consider (Trac #11348)

     type family F a
     type instance F Int = Bool

     data R = MkR (F Int)

     type Foo = 'MkR 'True

  For Foo to kind-check we need to know that (F Int) ~ Bool.  But we won't
  know that unless we've looked at the type instance declaration for F
  before kind-checking Foo.

* Another example is this (Trac #3990).

# Role annotations


### Note: Role annotations in the renamer

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


# Dependency info for instances


# Renaming a type or class declaration


### Note: Stupid theta

Trac #3850 complains about a regression wrt 6.10 for
     data Show a => T a
There is no reason not to allow the stupid theta if there are no data
constructors.  It's still stupid, but does no harm, and I don't want
to cause programs to break unnecessarily (notably HList).  So if there
are no data constructors we allow h98_style = True


# Support code for type/data declarations


# \subsection{Support code to rename types}


# findSplice


This code marches down the declarations, looking for the first
Template Haskell splice.  As it does so it
        a) groups the declarations into a HsGroup
        b) runs any top-level quasi-quotes
