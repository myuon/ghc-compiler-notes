[[src]](https://github.com/ghc/ghc/tree/master/compiler/typecheck/TcTypeable.hs)

(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1999


### Note: Grand plan for Typeable

The overall plan is this:

1. Generate a binding for each module p:M
   (done in TcTypeable by mkModIdBindings)
       M.$trModule :: GHC.Types.Module
       M.$trModule = Module "p" "M"
   ("tr" is short for "type representation"; see GHC.Types)

   We might want to add the filename too.
   This can be used for the lightweight stack-tracing stuff too

   Record the Name M.$trModule in the tcg_tr_module field of TcGblEnv

2. Generate a binding for every data type declaration T in module M,
       M.$tcT :: GHC.Types.TyCon
       M.$tcT = TyCon ...fingerprint info...
                      $trModule
                      "T"
                      0#
                      kind_rep

### Note: Representing TyCon kinds: KindRep

   We define (in TyCon)

        type TyConRepName = Name

### Note: Handling never-exported TyThings under Backpack

3. Record the TyConRepName in T's TyCon, including for promoted
   data and type constructors, and kinds like * and #.

   The TyConRepName is not an "implicit Id".  It's more like a record
   selector: the TyCon knows its name but you have to go to the
   interface file to find its type, value, etc

4. Solve Typeable constraints.  This is done by a custom Typeable solver,
   currently in TcInteract, that use M.$tcT so solve (Typeable T).

There are many wrinkles:

* The timing of when we produce this bindings is rather important: they must be
  defined after the rest of the module has been typechecked since we need to be
  able to lookup Module and TyCon in the type environment and we may be
  currently compiling GHC.Types (where they are defined).

* GHC.Prim doesn't have any associated object code, so we need to put the
  representations for types defined in this module elsewhere. We chose this
  place to be GHC.Types. TcTypeable.mkPrimTypeableBinds is responsible for
  injecting the bindings for the GHC.Prim representions when compiling
  GHC.Types.

* TyCon.tyConRepModOcc is responsible for determining where to find
  the representation binding for a given type. This is where we handle
  the special case for GHC.Prim.

### Note: Runtime representation of modules and tycons

* The KindReps can unfortunately get quite large. Moreover, the simplifier will
  float out various pieces of them, resulting in numerous top-level bindings.
  Consequently we mark the KindRep bindings as noinline, ensuring that the
  float-outs don't make it into the interface file. This is important since
  there is generally little benefit to inlining KindReps and they would
  otherwise strongly affect compiler performance.

* In general there are lots of things of kind *, * -> *, and * -> * -> *. To
  reduce the number of bindings we need to produce, we generate their KindReps
  once in GHC.Types. These are referred to as "built-in" KindReps below.

* Even though KindReps aren't inlined, this scheme still has more of an effect on
  compilation time than I'd like. This is especially true in the case of
  families of type constructors (e.g. tuples and unboxed sums). The problem is
  particularly bad in the case of sums, since each arity-N tycon brings with it
  N promoted datacons, each with a KindRep whose size also scales with N.
  Consequently we currently simply don't allow sums to be Typeable.

  In general we might consider moving some or all of this generation logic back
  to the solver since the performance hit we take in doing this at
  type-definition time is non-trivial and Typeable isn't very widely used. This
  is discussed in #13261.



# Building top-level binding for $trModule


# Building type-representation bindings


### Note: Representing TyCon kinds: KindRep

One of the operations supported by Typeable is typeRepKind,

    typeRepKind :: TypeRep (a :: k) -> TypeRep k

Implementing this is a bit tricky for poly-kinded types like

    data Proxy (a :: k) :: Type
    -- Proxy :: forall k. k -> Type

The TypeRep encoding of `Proxy Type Int` looks like this:

    $tcProxy :: GHC.Types.TyCon
    $trInt   :: TypeRep Int
    TrType   :: TypeRep Type

    $trProxyType :: TypeRep (Proxy Type :: Type -> Type)
    $trProxyType = TrTyCon $tcProxy
                           [TrType]  -- kind variable instantiation
                           (tyConKind $tcProxy [TrType]) -- The TypeRep of
                                                         -- Type -> Type

    $trProxy :: TypeRep (Proxy Type Int)
    $trProxy = TrApp $trProxyType $trInt TrType

    $tkProxy :: GHC.Types.KindRep
    $tkProxy = KindRepFun (KindRepVar 0)
                          (KindRepTyConApp (KindRepTYPE LiftedRep) [])

Note how $trProxyType cannot use 'TrApp', because TypeRep cannot represent
polymorphic types.  So instead

 * $trProxyType uses 'TrTyCon' to apply Proxy to (the representations)
   of all its kind arguments. We can't represent a tycon that is
   applied to only some of its kind arguments.

 * In $tcProxy, the GHC.Types.TyCon structure for Proxy, we store a
   GHC.Types.KindRep, which represents the polymorphic kind of Proxy
       Proxy :: forall k. k->Type

 * A KindRep is just a recipe that we can instantiate with the
   argument kinds, using Data.Typeable.Internal.tyConKind and
   store in the relevant 'TypeRep' constructor.

   Data.Typeable.Internal.typeRepKind looks up the stored kinds.

 * In a KindRep, the kind variables are represented by 0-indexed
   de Bruijn numbers:

    type KindBndr = Int   -- de Bruijn index

    data KindRep = KindRepTyConApp TyCon [KindRep]
                 | KindRepVar !KindBndr
                 | KindRepApp KindRep KindRep
                 | KindRepFun KindRep KindRep
                 ...
