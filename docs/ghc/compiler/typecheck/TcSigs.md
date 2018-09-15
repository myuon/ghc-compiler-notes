[[src]](https://github.com/ghc/ghc/tree/master/compiler/typecheck/TcSigs.hs)

(c) The University of Glasgow 2006-2012
(c) The GRASP Project, Glasgow University, 1992-2002



### Note: Overview of type signatures

    f :: forall a. [a] -> [a]
    g :: forall b. _ -> b

    f = ...g...
    g = ...f...

* HsSyn: a signature in a binding starts of as a TypeSig, in
  type HsBinds.Sig

* When starting a mutually recursive group, like f/g above, we
  call tcTySig on each signature in the group.

* tcTySig: Sig -> TcIdSigInfo
  - For a /complete/ signature, like 'f' above, tcTySig kind-checks
    the HsType, producing a Type, and wraps it in a CompleteSig, and
    extend the type environment with this polymorphic 'f'.

  - For a /partial/signature, like 'g' above, tcTySig does nothing
    Instead it just wraps the pieces in a PartialSig, to be handled
    later.

* tcInstSig: TcIdSigInfo -> TcIdSigInst
  In tcMonoBinds, when looking at an individual binding, we use
  tcInstSig to instantiate the signature forall's in the signature,
  and attribute that instantiated (monomorphic) type to the
  binder.  You can see this in TcBinds.tcLhsId.

  The instantiation does the obvious thing for complete signatures,
  but for /partial/ signatures it starts from the HsSyn, so it
  has to kind-check it etc: tcHsPartialSigType.  It's convenient
  to do this at the same time as instantiation, because we can
  make the wildcards into unification variables right away, raather
  than somehow quantifying over them.  And the "TcLevel" of those
  unification variables is correct because we are in tcMonoBinds.

### Note: Scoped tyvars

The -XScopedTypeVariables flag brings lexically-scoped type variables
into scope for any explicitly forall-quantified type variables:
        f :: forall a. a -> a
        f x = e
Then 'a' is in scope inside 'e'.

However, we do *not* support this
  - For pattern bindings e.g
        f :: forall a. a->a
        (f,g) = e

### Note: Binding scoped type variables

The type variables *brought into lexical scope* by a type signature
may be a subset of the *quantified type variables* of the signatures,
for two reasons:

* With kind polymorphism a signature like
    f :: forall f a. f a -> f a
  may actually give rise to
    f :: forall k. forall (f::k -> *) (a:k). f a -> f a
  So the sig_tvs will be [k,f,a], but only f,a are scoped.
  NB: the scoped ones are not necessarily the *inital* ones!

* Even aside from kind polymorphism, there may be more instantiated
  type variables than lexically-scoped ones.  For example:
        type T a = forall b. b -> (a,b)
        f :: forall c. T c
  Here, the signature for f will have one scoped type variable, c,
  but two instantiated type variables, c' and b'.

However, all of this only applies to the renamer.  The typechecker
just puts all of them into the type environment; any lexical-scope
errors were dealt with by the renamer.



# Utility functions for TcSigInfo


# Typechecking user signatures


### Note: Fail eagerly on bad signatures

If a type signature is wrong, fail immediately:

 * the type sigs may bind type variables, so proceeding without them
   can lead to a cascade of errors

 * the type signature might be ambiguous, in which case checking
   the code against the signature will give a very similar error
   to the ambiguity error.

ToDo: this means we fall over if any type sig
is wrong (eg at the top level of the module),
which is over-conservative


# Type checking a pattern synonym signature


### Note: Pattern synonym signatures

Pattern synonym signatures are surprisingly tricky (see Trac #11224 for example).
In general they look like this:

   pattern P :: forall univ_tvs. req_theta
             => forall ex_tvs. prov_theta
             => arg1 -> .. -> argn -> res_ty

For parsing and renaming we treat the signature as an ordinary LHsSigType.

Once we get to type checking, we decompose it into its parts, in tcPatSynSig.

* Note that 'forall univ_tvs' and 'req_theta =>'
        and 'forall ex_tvs'   and 'prov_theta =>'
  are all optional.  We gather the pieces at the the top of tcPatSynSig

* Initially the implicitly-bound tyvars (added by the renamer) include both
  universal and existential vars.

* After we kind-check the pieces and convert to Types, we do kind generalisation.

### Note: The pattern-synonym signature splitting rule

Given a pattern signature, we must split
     the kind-generalised variables, and
     the implicitly-bound variables
into universal and existential.  The rule is this
(see discussion on Trac #11224):

     The universal tyvars are the ones mentioned in
          - univ_tvs: the user-specified (forall'd) universals
          - req_theta
          - res_ty
     The existential tyvars are all the rest

For example

   pattern P :: () => b -> T a
   pattern P x = ...

Here 'a' is universal, and 'b' is existential.  But there is a wrinkle:
how do we split the arg_tys from req_ty?  Consider

   pattern Q :: () => b -> S c -> T a
   pattern Q x = ...

This is an odd example because Q has only one syntactic argument, and
so presumably is defined by a view pattern matching a function.  But
it can happen (Trac #11977, #12108).

We don't know Q's arity from the pattern signature, so we have to wait
until we see the pattern declaration itself before deciding res_ty is,
and hence which variables are existential and which are universal.

And that in turn is why TcPatSynInfo has a separate field,
patsig_implicit_bndrs, to capture the implicitly bound type variables,
because we don't yet know how to split them up.

It's a slight compromise, because it means we don't really know the
pattern synonym's real signature until we see its declaration.  So,
for example, in hs-boot file, we may need to think what to do...
(eg don't have any implicitly-bound variables).


# Instantiating user signatures


### Note: Pattern bindings and complete signatures

Consider
      data T a = MkT a a
      f :: forall a. a->a
      g :: forall b. b->b
      MkT f g = MkT (\x->x) (\y->y)
Here we'll infer a type from the pattern of 'T a', but if we feed in
the signature types for f and g, we'll end up unifying 'a' and 'b'

So we instantiate f and g's signature with SigTv skolems
(newMetaSigTyVars) that can unify with each other.  If too much
unification takes place, we'll find out when we do the final
impedance-matching check in TcBinds.mkExport

### Note: Signature skolems

None of this applies to a function binding with a complete
signature, which doesn't use tcInstSig.  See TcBinds.tcPolyCheck.


# Pragmas and PragEnv


# SPECIALISE pragmas


### Note: Handling SPECIALISE pragmas

The basic idea is this: