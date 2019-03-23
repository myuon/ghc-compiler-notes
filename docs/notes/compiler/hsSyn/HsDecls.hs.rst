Note [The Naming story]
~~~~~~~~~~~~~~~~~~~~~~~
Here is the story about the implicit names that go with type, class,
and instance decls.  It's a bit tricky, so pay attention!

"Implicit" (or "system") binders
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  Each data type decl defines
        a worker name for each constructor
        to-T and from-T convertors
  Each class decl defines
        a tycon for the class
        a data constructor for that tycon
        the worker for that constructor
        a selector for each superclass

All have occurrence names that are derived uniquely from their parent
declaration.

None of these get separate definitions in an interface file; they are
fully defined by the data or class decl.  But they may *occur* in
interface files, of course.  Any such occurrence must haul in the
relevant type or class decl.

Plan of attack:
 - Ensure they "point to" the parent data/class decl
   when loading that decl from an interface file
   (See RnHiFiles.getSysBinders)

 - When typechecking the decl, we build the implicit TyCons and Ids.
   When doing so we look them up in the name cache (RnEnv.lookupSysName),
   to ensure correct module and provenance is set

These are the two places that we have to conjure up the magic derived
names.  (The actual magic is in OccName.mkWorkerOcc, etc.)

Default methods
~~~~~~~~~~~~~~~
 - Occurrence name is derived uniquely from the method name
   E.g. $dmmax

 - If there is a default method name at all, it's recorded in
   the ClassOpSig (in HsBinds), in the DefMethInfo field.
   (DefMethInfo is defined in Class.hs)

Source-code class decls and interface-code class decls are treated subtly
differently, which has given me a great deal of confusion over the years.
Here's the deal.  (We distinguish the two cases because source-code decls
have (Just binds) in the tcdMeths field, whereas interface decls have Nothing.

In *source-code* class declarations:

 - When parsing, every ClassOpSig gets a DefMeth with a suitable RdrName
   This is done by RdrHsSyn.mkClassOpSigDM

 - The renamer renames it to a Name

 - During typechecking, we generate a binding for each $dm for
   which there's a programmer-supplied default method:
        class Foo a where
          op1 :: <type>
          op2 :: <type>
          op1 = ...
   We generate a binding for $dmop1 but not for $dmop2.
   The Class for Foo has a Nothing for op2 and
                         a Just ($dm_op1, VanillaDM) for op1.
   The Name for $dmop2 is simply discarded.

In *interface-file* class declarations:
  - When parsing, we see if there's an explicit programmer-supplied default method
    because there's an '=' sign to indicate it:
        class Foo a where
          op1 = :: <type>       -- NB the '='
          op2   :: <type>
    We use this info to generate a DefMeth with a suitable RdrName for op1,
    and a NoDefMeth for op2
  - The interface file has a separate definition for $dmop1, with unfolding etc.
  - The renamer renames it to a Name.
  - The renamer treats $dmop1 as a free variable of the declaration, so that
    the binding for $dmop1 will be sucked in.  (See RnHsSyn.tyClDeclFVs)
    This doesn't happen for source code class decls, because they *bind* the default method.

Dictionary functions
~~~~~~~~~~~~~~~~~~~~
Each instance declaration gives rise to one dictionary function binding.

The type checker makes up new source-code instance declarations
(e.g. from 'deriving' or generic default methods --- see
TcInstDcls.tcInstDecls1).  So we can't generate the names for
dictionary functions in advance (we don't know how many we need).

On the other hand for interface-file instance declarations, the decl
specifies the name of the dictionary function, and it has a binding elsewhere
in the interface file:
        instance {Eq Int} = dEqInt
        dEqInt :: {Eq Int} <pragma info>

So again we treat source code and interface file code slightly differently.

Source code:
  - Source code instance decls have a Nothing in the (Maybe name) field
    (see data InstDecl below)

  - The typechecker makes up a Local name for the dict fun for any source-code
    instance decl, whether it comes from a source-code instance decl, or whether
    the instance decl is derived from some other construct (e.g. 'deriving').

  - The occurrence name it chooses is derived from the instance decl (just for
    documentation really) --- e.g. dNumInt.  Two dict funs may share a common
    occurrence name, but will have different uniques.  E.g.
        instance Foo [Int]  where ...
        instance Foo [Bool] where ...
    These might both be dFooList

  - The CoreTidy phase externalises the name, and ensures the occurrence name is
    unique (this isn't special to dict funs).  So we'd get dFooList and dFooList1.

  - We can take this relaxed approach (changing the occurrence name later)
    because dict fun Ids are not captured in a TyCon or Class (unlike default
    methods, say).  Instead, they are kept separately in the InstEnv.  This
    makes it easy to adjust them after compiling a module.  (Once we've finished
    compiling that module, they don't change any more.)


Interface file code:
  - The instance decl gives the dict fun name, so the InstDecl has a (Just name)
    in the (Maybe name) field.

  - RnHsSyn.instDeclFVs treats the dict fun name as free in the decl, so that we
    suck in the dfun binding


Note [TyVar binders for associated decls]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For an /associated/ data, newtype, or type-family decl, the LHsQTyVars
/includes/ outer binders.  For example
    class T a where
       data D a c
       type F a b :: *
       type F a b = a -> a
Here the data decl for 'D', and type-family decl for 'F', both include 'a'
in their LHsQTyVars (tcdTyVars and fdTyVars resp).

Ditto any implicit binders in the hsq_implicit field of the LHSQTyVars.

The idea is that the associated type is really a top-level decl in its
own right.  However we are careful to use the same name 'a', so that
we can match things up.

c.f. Note [Associated type tyvar names] in Class.hs
     Note [Family instance declaration binders]


Note [CUSKs: complete user-supplied kind signatures]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We kind-check declarations differently if they have a complete, user-supplied
kind signature (CUSK). This is because we can safely generalise a CUSKed
declaration before checking all of the others, supporting polymorphic recursion.
See ghc.haskell.org/trac/ghc/wiki/GhcKinds/KindInference#Proposednewstrategy
and #9200 for lots of discussion of how we got here.

PRINCIPLE:
  a type declaration has a CUSK iff we could produce a separate kind signature
  for it, just like a type signature for a function,
  looking only at the header of the declaration.

Examples:
  * data T1 (a :: *->*) (b :: *) = ....
    -- Has CUSK; equivalant to   T1 :: (*->*) -> * -> *

 * data T2 a b = ...
   -- No CUSK; we do not want to guess T2 :: * -> * -> *
   -- because the full decl might be   data T a b = MkT (a b)

  * data T3 (a :: k -> *) (b :: *) = ...
    -- CUSK; equivalent to   T3 :: (k -> *) -> * -> *
    -- We lexically generalise over k to get
    --    T3 :: forall k. (k -> *) -> * -> *
    -- The generalisation is here is purely lexical, just like
    --    f3 :: a -> a
    -- means
    --    f3 :: forall a. a -> a

  * data T4 (a :: j k) = ...
     -- CUSK; equivalent to   T4 :: j k -> *
     -- which we lexically generalise to  T4 :: forall j k. j k -> *
     -- and then, if PolyKinds is on, we further generalise to
     --   T4 :: forall kk (j :: kk -> *) (k :: kk). j k -> *
     -- Again this is exactly like what happens as the term level
     -- when you write
     --    f4 :: forall a b. a b -> Int

NOTE THAT
  * A CUSK does /not/ mean that everything about the kind signature is
    fully specified by the user.  Look at T4 and f4: we had do do kind
    inference to figure out the kind-quantification.  But in both cases
    (T4 and f4) that inference is done looking /only/ at the header of T4
    (or signature for f4), not at the definition thereof.

  * The CUSK completely fixes the kind of the type constructor, forever.

  * The precise rules, for each declaration form, for whethher a declaration
    has a CUSK are given in the user manual section "Complete user-supplied
    kind signatures and polymorphic recursion".  BUt they simply implement
    PRINCIPLE above.

  * Open type families are interesting:
      type family T5 a b :: *
    There simply /is/ no accompanying declaration, so that info is all
    we'll ever get.  So we it has a CUSK by definition, and we default
    any un-fixed kind variables to *.

  * Associated types are a bit tricker:
      class C6 a where
         type family T6 a b :: *
         op :: a Int -> Int
    Here C6 does not have a CUSK (in fact we ultimately discover that
    a :: * -> *).  And hence neither does T6, the associated family,
    because we can't fix its kind until we have settled C6.  Another
    way to say it: unlike a top-level, we /may/ discover more about
    a's kind from C6's definition.

  * A data definition with a top-level :: must explicitly bind all
    kind variables to the right of the ::. See test
    dependent/should_compile/KindLevels, which requires this
    case. (Naturally, any kind variable mentioned before the :: should
    not be bound after it.)

    This last point is much more debatable than the others; see
    #15142 comment:22


Note [TyClGroups and dependency analysis]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A TyClGroup represents a strongly connected components of type/class/instance
decls, together with the role annotations for the type/class declarations.

The hs_tyclds :: [TyClGroup] field of a HsGroup is a dependency-order
sequence of strongly-connected components.

Invariants
 * The type and class declarations, group_tyclds, may depend on each
   other, or earlier TyClGroups, but not on later ones

 * The role annotations, group_roles, are role-annotations for some or
   all of the types and classes in group_tyclds (only).

 * The instance declarations, group_instds, may (and usually will)
   depend on group_tyclds, or on earlier TyClGroups, but not on later
   ones.

See Note [Dependency analsis of type, class, and instance decls]
in RnSource for more info.


Note [FamilyResultSig]
~~~~~~~~~~~~~~~~~~~~~~~~~

This data type represents the return signature of a type family.  Possible
values are:

 * NoSig - the user supplied no return signature:
      type family Id a where ...

 * KindSig - the user supplied the return kind:
      type family Id a :: * where ...

 * TyVarSig - user named the result with a type variable and possibly
   provided a kind signature for that variable:
      type family Id a = r where ...
      type family Id a = (r :: *) where ...

   Naming result of a type family is required if we want to provide
   injectivity annotation for a type family:
      type family Id a = r | r -> a where ...

See also: Note [Injectivity annotation]



Note [Injectivity annotation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A user can declare a type family to be injective:

   type family Id a = r | r -> a where ...

 * The part after the "|" is called "injectivity annotation".
 * "r -> a" part is called "injectivity condition"; at the moment terms
   "injectivity annotation" and "injectivity condition" are synonymous
   because we only allow a single injectivity condition.
 * "r" is the "LHS of injectivity condition". LHS can only contain the
   variable naming the result of a type family.

 * "a" is the "RHS of injectivity condition". RHS contains space-separated
   type and kind variables representing the arguments of a type
   family. Variables can be omitted if a type family is not injective in
   these arguments. Example:
         type family Foo a b c = d | d -> a c where ...

Note that:
 (a) naming of type family result is required to provide injectivity
     annotation
 (b) for associated types if the result was named then injectivity annotation
     is mandatory. Otherwise result type variable is indistinguishable from
     associated type default.

It is possible that in the future this syntax will be extended to support
more complicated injectivity annotations. For example we could declare that
if we know the result of Plus and one of its arguments we can determine the
other argument:

   type family Plus a b = (r :: Nat) | r a -> b, r b -> a where ...

Here injectivity annotation would consist of two comma-separated injectivity
conditions.

See also Note [Injective type families] in TyCon


Note [GADT abstract syntax]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
There's a wrinkle in ConDeclGADT

* For record syntax, it's all uniform.  Given:
      data T a where
        K :: forall a. Ord a => { x :: [a], ... } -> T a
    we make the a ConDeclGADT for K with
       con_qvars  = {a}
       con_mb_cxt = Just [Ord a]
       con_args   = RecCon <the record fields>
       con_res_ty = T a

  We need the RecCon before the reanmer, so we can find the record field
  binders in HsUtils.hsConDeclsBinders.

* However for a GADT constr declaration which is not a record, it can
  be hard parse until we know operator fixities. Consider for example
     C :: a :*: b -> a :*: b -> a :+: b
  Initially this type will parse as
      a :*: (b -> (a :*: (b -> (a :+: b))))
  so it's hard to split up the arguments until we've done the precedence
  resolution (in the renamer).

  So:  - In the parser (RdrHsSyn.mkGadtDecl), we put the whole constr
         type into the res_ty for a ConDeclGADT for now, and use
         PrefixCon []
            con_args   = PrefixCon []
            con_res_ty = a :*: (b -> (a :*: (b -> (a :+: b))))

       - In the renamer (RnSource.rnConDecl), we unravel it afer
         operator fixities are sorted. So we generate. So we end
         up with
            con_args   = PrefixCon [ a :*: b, a :*: b ]
            con_res_ty = a :+: b


Note [Type family instance declarations in HsSyn]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The data type FamEqn represents one equation of a type family instance.
Aside from the pass, it is also parameterised over two fields:
feqn_pats and feqn_rhs.

feqn_pats is either LHsTypes (for ordinary data/type family instances) or
LHsQTyVars (for associated type family default instances). In particular:

 * An ordinary type family instance declaration looks like this in source Haskell
      type instance T [a] Int = a -> a
   (or something similar for a closed family)
   It is represented by a FamInstEqn, with a *type* (LHsType) in the feqn_pats
   field.

 * On the other hand, the *default instance* of an associated type looks like
   this in source Haskell
      class C a where
        type T a b
        type T a b = a -> b   -- The default instance
   It is represented by a TyFamDefltEqn, with *type variables* (LHsQTyVars) in
   the feqn_pats field.

feqn_rhs is either an HsDataDefn (for data family instances) or an LHsType
(for type family instances).
--------------- Type synonym family instances -------------


Note [Family instance declaration binders]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For ordinary data/type family instances, the feqn_pats field of FamEqn stores
the LHS type (and kind) patterns. Any type (and kind) variables contained
in these type patterns are bound in the hsib_vars field of the HsImplicitBndrs
in FamInstEqn depending on whether or not an explicit forall is present. In
the case of an explicit forall, the hsib_vars only includes kind variables not
bound in the forall. Otherwise, all type (and kind) variables are bound in
the hsib_vars. In the latter case, note that in particular

* The hsib_vars *includes* any anonymous wildcards.  For example
     type instance F a _ = a
  The hsib_vars will be {a, _}.  Remember that each separate wildcard
  '_' gets its own unique.  In this context wildcards behave just like
  an ordinary type variable, only anonymous.

* The hsib_vars *includes* type variables that are already in scope

   Eg   class C s t where
          type F t p :: *
        instance C w (a,b) where
          type F (a,b) x = x->a
   The hsib_vars of the F decl are {a,b,x}, even though the F decl
   is nested inside the 'instance' decl.

   However after the renamer, the uniques will match up:
        instance C w7 (a8,b9) where
          type F (a8,b9) x10 = x10->a8
   so that we can compare the type pattern in the 'instance' decl and
   in the associated 'type' decl

For associated type family default instances (TyFamDefltEqn), instead of using
type patterns with binders in a surrounding HsImplicitBndrs, we use raw type
variables (LHsQTyVars) in the feqn_pats field of FamEqn.

c.f. Note [TyVar binders for associated declarations]
