`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/deSugar/DsMeta.hs>`_

compiler/deSugar/DsMeta.hs
==========================


Note [Scoped type variables in bindings]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/deSugar/DsMeta.hs#L216>`__

Consider
   f :: forall a. a -> a
   f x = x::a
Here the 'forall a' brings 'a' into scope over the binding group.
To achieve this we

::

  a) Gensym a binding for 'a' at the same time as we do one for 'f'
     collecting the relevant binders with hsScopedTvBinders

::

  b) When processing the 'forall', don't gensym

The relevant places are signposted with references to this Note



Note [Scoped type variables in class and instance declarations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/deSugar/DsMeta.hs#L231>`__

Scoped type variables may occur in default methods and default
signatures. We need to bring the type variables in 'foralls'
into the scope of the method bindings.

Consider
   class Foo a where
     foo :: forall (b :: k). a -> Proxy b -> Proxy b
     foo _ x = (x :: Proxy b)

We want to ensure that the 'b' in the type signature and the default
implementation are the same, so we do the following:

::

  a) Before desugaring the signature and binding of 'foo', use
     get_scoped_tvs to collect type variables in 'forall' and
     create symbols for them.
  b) Use 'addBinds' to bring these symbols into the scope of the type
     signatures and bindings.
  c) Use these symbols to generate Core for the class/instance declaration.

Note that when desugaring the signatures, we lookup the type variables
from the scope rather than recreate symbols for them. See more details
in "rep_ty_sig" and in Trac#14885.



Note [Binders and occurrences]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/deSugar/DsMeta.hs#L256>`__

When we desugar [d| data T = MkT |]
we want to get
        Data "T" [] [Con "MkT" []] []
and *not*
        Data "Foo:T" [] [Con "Foo:MkT" []] []
That is, the new data decl should fit into whatever new module it is
asked to fit in.   We do *not* clone, though; no need for this:
        Data "T79" ....

But if we see this:
        data T = MkT
        foo = reifyDecl T

then we must desugar to
        foo = Data "Foo:T" [] [Con "Foo:MkT" []] []

So in repTopDs we bring the binders into scope with mkGenSyms and addBinds.
And we use lookupOcc, rather than lookupBinder
in repTyClD and repC.



Note [Don't quantify implicit type variables in quotes]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/deSugar/DsMeta.hs#L278>`__

If you're not careful, it's suprisingly easy to take this quoted declaration:

::

  [d| idProxy :: forall proxy (b :: k). proxy b -> proxy b
      idProxy x = x
    |]

and have Template Haskell turn it into this:

::

  idProxy :: forall k proxy (b :: k). proxy b -> proxy b
  idProxy x = x

Notice that we explicitly quantified the variable `k`! The latter declaration
isn't what the user wrote in the first place.

Usually, the culprit behind these bugs is taking implicitly quantified type
variables (often from the hsib_vars field of HsImplicitBinders) and putting
them into a `ForallT` or `ForallC`. Doing so caused #13018 and #13123.
represent associated family instances

