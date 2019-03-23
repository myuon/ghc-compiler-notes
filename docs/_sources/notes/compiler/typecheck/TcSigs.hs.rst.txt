Note [Scoped tyvars]
~~~~~~~~~~~~~~~~~~~~
The -XScopedTypeVariables flag brings lexically-scoped type variables
into scope for any explicitly forall-quantified type variables:
        f :: forall a. a -> a
        f x = e
Then 'a' is in scope inside 'e'.

However, we do *not* support this
  - For pattern bindings e.g
        f :: forall a. a->a
        (f,g) = e



Note [Binding scoped type variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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



Note [Fail eagerly on bad signatures]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If a type signature is wrong, fail immediately:

 * the type sigs may bind type variables, so proceeding without them
   can lead to a cascade of errors

 * the type signature might be ambiguous, in which case checking
   the code against the signature will give a very similar error
   to the ambiguity error.

ToDo: this means we fall over if any top-level type signature in the
module is wrong, because we typecheck all the signatures together
(see TcBinds.tcValBinds).  Moreover, because of top-level
captureTopConstraints, only insoluble constraints will be reported.
We typecheck all signatures at the same time because a signature
like   f,g :: blah   might have f and g from different SCCs.

So it's a bit awkward to get better error recovery, and no one
has complained!


Note [Pattern bindings and complete signatures]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
      data T a = MkT a a
      f :: forall a. a->a
      g :: forall b. b->b
      MkT f g = MkT (\x->x) (\y->y)
Here we'll infer a type from the pattern of 'T a', but if we feed in
the signature types for f and g, we'll end up unifying 'a' and 'b'

So we instantiate f and g's signature with TyVarTv skolems
(newMetaTyVarTyVars) that can unify with each other.  If too much
unification takes place, we'll find out when we do the final
impedance-matching check in TcBinds.mkExport

See Note [Signature skolems] in TcType

None of this applies to a function binding with a complete
signature, which doesn't use tcInstSig.  See TcBinds.tcPolyCheck.
