[[src]](https://github.com/ghc/ghc/tree/master/compiler/typecheck/TcClassDcl.hs)

(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998


Typechecking class declarations


# Dictionary handling

Every class implicitly declares a new data type, corresponding to dictionaries
of that class. So, for example:

        class (D a) => C a where
          op1 :: a -> a
          op2 :: forall b. Ord b => a -> b -> b

would implicitly declare

        data CDict a = CDict (D a)
                             (a -> a)
                             (forall b. Ord b => a -> b -> b)

(We could use a record decl, but that means changing more of the existing apparatus.
One step at at time!)

For classes with just one superclass+method, we use a newtype decl instead:

        class C a where
          op :: forallb. a -> b -> b

generates

        newtype CDict a = CDict (forall b. a -> b -> b)

Now DictTy in Type is just a form of type synomym:
        DictTy c t = TyConTy CDict `AppTy` t

Death to "ExpandingDicts".

# Type-checking the class op signatures


# Class Declarations


### Note: Polymorphic methods

Consider
    class Foo a where
        op :: forall b. Ord b => a -> b -> b -> b
    instance Foo c => Foo [c] where
        op = e

When typechecking the binding 'op = e', we'll have a meth_id for op
whose type is
      op :: forall c. Foo c => forall b. Ord b => [c] -> b -> b -> b

So tcPolyBinds must be capable of dealing with nested polytypes;
and so it is. See TcBinds.tcMonoBinds (with type-sig case).

### Note: Silly default-method bind

When we pass the default method binding to the type checker, it must
look like    op2 = e
not          $dmop2 = e
otherwise the "$dm" stuff comes out error messages.  But we want the
"$dm" to come out in the interface file.  So we typecheck the former,
and wrap it in a let, thus
          $dmop2 = let op2 = e in op2
This makes the error messages right.

# Error messages



badGenericInstanceType :: LHsBinds Name -> SDoc
badGenericInstanceType binds
  = vcat [text "Illegal type pattern in the generic bindings",
          nest 2 (ppr binds)]

missingGenericInstances :: [Name] -> SDoc
missingGenericInstances missing
  = text "Missing type patterns for" <+> pprQuotedList missing

dupGenericInsts :: [(TyCon, InstInfo a)] -> SDoc
dupGenericInsts tc_inst_infos
  = vcat [text "More than one type pattern for a single generic type constructor:",
          nest 2 (vcat (map ppr_inst_ty tc_inst_infos)),
          text "All the type patterns for a generic type constructor must be identical"
    ]
  where
    ppr_inst_ty (_,inst) = ppr (simpleInstInfoTy inst)
