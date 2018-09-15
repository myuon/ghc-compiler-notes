[[src]](https://github.com/ghc/ghc/tree/master/compiler/deSugar/DsMeta.hs)
 -------------- Examples --------------------

  [| \x -> x |]
====>
  gensym (unpackString "x"#) `bindQ` \ x1::String ->
  lam (pvar x1) (var x1)


  [| \x -> $(f [| x |]) |]
====>
  gensym (unpackString "x"#) `bindQ` \ x1::String ->
  lam (pvar x1) (f (var x1))


 Notes

### Note: Scoped type variables in bindings

Consider
   f :: forall a. a -> a
   f x = x::a
Here the 'forall a' brings 'a' into scope over the binding group.
To achieve this we

  a) Gensym a binding for 'a' at the same time as we do one for 'f'
     collecting the relevant binders with hsSigTvBinders

  b) When processing the 'forall', don't gensym

The relevant places are signposted with references to this Note

### Note: Binders and occurrences

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

### Note: Don't quantify implicit type variables in quotes

If you're not careful, it's suprisingly easy to take this quoted declaration:

  [d| idProxy :: forall proxy (b :: k). proxy b -> proxy b
      idProxy x = x
    |]

and have Template Haskell turn it into this:

  idProxy :: forall k proxy (b :: k). proxy b -> proxy b
  idProxy x = x

Notice that we explicitly quantified the variable `k`! This is quite bad, as the
latter declaration requires -XTypeInType, while the former does not. Not to
mention that the latter declaration isn't even what the user wrote in the
first place.

Usually, the culprit behind these bugs is taking implicitly quantified type
variables (often from the hsib_vars field of HsImplicitBinders) and putting
them into a `ForallT` or `ForallC`. Doing so caused #13018 and #13123.
