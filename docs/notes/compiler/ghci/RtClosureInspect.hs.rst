`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/ghci/RtClosureInspect.hs>`_

compiler/ghci/RtClosureInspect.hs
=================================


Note [Constructor arg types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/ghci/RtClosureInspect.hs#L1057>`__

Consider a GADT (cf #7386)
   data family D a b
   data instance D [a] a where
     MkT :: a -> D [a] (Maybe a)
     ...

In getDataConArgTys
* con_app_ty is the known type (from outside) of the constructor application,
  say D [Int] Int

* The data constructor MkT has a (representation) dataConTyCon = DList,
  say where
    data DList a where
      MkT :: a -> DList a (Maybe a)
      ...

So the dataConTyCon of the data constructor, DList, differs from
the "outside" type, D. So we can't straightforwardly decompose the
"outside" type, and we end up in the "_" branch of the case.

Then we match the dataConOrigResTy of the data constructor against the
outside type, hoping to get a substitution that tells how to instantiate
the *representation* type constructor.   This looks a bit delicate to
me, but it seems to work.

Soundness checks

