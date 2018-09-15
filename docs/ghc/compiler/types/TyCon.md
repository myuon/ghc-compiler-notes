[[src]](https://github.com/ghc/ghc/tree/master/compiler/types/TyCon.hs)

(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998


The @TyCon@ datatype



-----------------------------------------------
        Notes about type families
-----------------------------------------------

### Note: Type synonym families

* Type synonym families, also known as "type functions", map directly
  onto the type functions in FC:

        type family F a :: *
        type instance F Int = Bool
        ..etc...

* Reply "yes" to isTypeFamilyTyCon, and isFamilyTyCon

* From the user's point of view (F Int) and Bool are simply
  equivalent types.

* A Haskell 98 type synonym is a degenerate form of a type synonym
  family.

* Type functions can't appear in the LHS of a type function:
        type instance F (F Int) = ...   -- BAD!

* Translation of type family decl:
        type family F a :: *
  translates to
    a FamilyTyCon 'F', whose FamTyConFlav is OpenSynFamilyTyCon

        type family G a :: * where
          G Int = Bool
          G Bool = Char
          G a = ()
  translates to
    a FamilyTyCon 'G', whose FamTyConFlav is ClosedSynFamilyTyCon, with the
    appropriate CoAxiom representing the equations

### Note: Injective type families

### Note: Data type families

### Note: Wrappers for data instance tycons

* Data type families are declared thus
        data family T a :: *
        data instance T Int = T1 | T2 Bool

  Here T is the "family TyCon".

* Reply "yes" to isDataFamilyTyCon, and isFamilyTyCon

* The user does not see any "equivalent types" as he did with type
  synonym families.  He just sees constructors with types
        T1 :: T Int
        T2 :: Bool -> T Int

* Here's the FC version of the above declarations:

        data T a
        data R:TInt = T1 | T2 Bool
        axiom ax_ti : T Int ~R R:TInt

  Note that this is a *representational* coercion
  The R:TInt is the "representation TyCons".
  It has an AlgTyConFlav of
        DataFamInstTyCon T [Int] ax_ti

### Note: Eta reduction for data family axioms

### Note: Arity of data families

* The data constructor T2 has a wrapper (which is what the
  source-level "T2" invokes):

        $WT2 :: Bool -> T Int
        $WT2 b = T2 b `cast` sym ax_ti

* A data instance can declare a fully-fledged GADT:

        data instance T (a,b) where
          X1 :: T (Int,Bool)
          X2 :: a -> b -> T (a,b)

  Here's the FC version of the above declaration:

        data R:TPair a where
          X1 :: R:TPair Int Bool
          X2 :: a -> b -> R:TPair a b
        axiom ax_pr :: T (a,b)  ~R  R:TPair a b

        $WX1 :: forall a b. a -> b -> T (a,b)
        $WX1 a b (x::a) (y::b) = X2 a b x y `cast` sym (ax_pr a b)

  The R:TPair are the "representation TyCons".
  We have a bit of work to do, to unpick the result types of the
  data instance declaration for T (a,b), to get the result type in the
  representation; e.g.  T (a,b) --> R:TPair a b

  The representation TyCon R:TList, has an AlgTyConFlav of

        DataFamInstTyCon T [(a,b)] ax_pr

* Notice that T is NOT translated to a FC type function; it just
  becomes a "data type" with no constructors, which can be coerced inot
  into R:TInt, R:TPair by the axioms.  These axioms
  axioms come into play when (and *only* when) you
        - use a data constructor
        - do pattern matching
  Rather like newtype, in fact

  As a result

  - T behaves just like a data type so far as decomposition is concerned

  - (T Int) is not implicitly converted to R:TInt during type inference.
    Indeed the latter type is unknown to the programmer.

  - There *is* an instance for (T Int) in the type-family instance
    environment, but it is only used for overlap checking

  - It's fine to have T in the LHS of a type function:
    type instance F (T a) = [a]

  It was this last point that confused me!  The big thing is that you
  should not think of a data family T as a *type function* at all, not
  even an injective one!  We can't allow even injective type functions
  on the LHS of a type function:
        type family injective G a :: *
        type instance F (G Int) = Bool
  is no good, even if G is injective, because consider
        type instance G Int = Bool
        type instance F Bool = Char

  So a data type family is not an injective type function. It's just a
  data type with some axioms that connect it to other data types.

* The tyConTyVars of the representation tycon are the tyvars that the
  user wrote in the patterns. This is important in TcDeriv, where we
  bring these tyvars into scope before type-checking the deriving
  clause. This fact is arranged for in TcInstDecls.tcDataFamInstDecl.

### Note: Associated families and their parent class

*Associated* families are just like *non-associated* families, except
that they have a famTcParent field of (Just cls), which identifies the
parent class.

However there is an important sharing relationship between
  * the tyConTyVars of the parent Class
  * the tyConTyvars of the associated TyCon

   class C a b where
     data T p a
     type F a q b

Here the 'a' and 'b' are shared with the 'Class'; that is, they have
the same Unique.

This is important. In an instance declaration we expect
  * all the shared variables to be instantiated the same way
  * the non-shared variables of the associated type should not
    be instantiated at all

  instance C [x] (Tree y) where
     data T p [x] = T1 x | T2 p
     type F [x] q (Tree y) = (x,y,q)

### Note: TyCon Role signatures

Every tycon has a role signature, assigning a role to each of the tyConTyVars
(or of equal length to the tyConArity, if there are no tyConTyVars). An
example demonstrates these best: say we have a tycon T, with parameters a at
nominal, b at representational, and c at phantom. Then, to prove
representational equality between T a1 b1 c1 and T a2 b2 c2, we need to have
nominal equality between a1 and a2, representational equality between b1 and
b2, and nothing in particular (i.e., phantom equality) between c1 and c2. This
might happen, say, with the following declaration:

  data T a b c where
    MkT :: b -> T Int b c

Data and class tycons have their roles inferred (see inferRoles in TcTyDecls),
as do vanilla synonym tycons. Family tycons have all parameters at role N,
though it is conceivable that we could relax this restriction. (->)'s and
tuples' parameters are at role R. Each primitive tycon declares its roles;
it's worth noting that (~#)'s parameters are at role N. Promoted data
constructors' type arguments are at role R. All kind arguments are at role
N.

### Note: Unboxed tuple RuntimeRep vars

The contents of an unboxed tuple may have any representation. Accordingly,
the kind of the unboxed tuple constructor is runtime-representation
polymorphic. For example,

   (#,#) :: forall (q :: RuntimeRep) (r :: RuntimeRep). TYPE q -> TYPE r -> #

These extra tyvars (v and w) cause some delicate processing around tuples,
where we used to be able to assume that the tycon arity and the
datacon arity were the same.

### Note: Injective type families

We allow injectivity annotations for type families (both open and closed):

  type family F (a :: k) (b :: k) = r | r -> a
  type family G a b = res | res -> a b where ...

Injectivity information is stored in the `famTcInj` field of `FamilyTyCon`.
`famTcInj` maybe stores a list of Bools, where each entry corresponds to a
single element of `tyConTyVars` (both lists should have identical length). If no
injectivity annotation was provided `famTcInj` is Nothing. From this follows an
invariant that if `famTcInj` is a Just then at least one element in the list
must be True.

See also:
 * [Injectivity annotation] in HsDecls
 * [Renaming injectivity annotation] in RnSource
 * [Verifying injectivity annotation] in FamInstEnv
 * [Type inference for type families with injectivity] in TcInteract

# TyConBinder


### Note: Building TyVarBinders from TyConBinders

We sometimes need to build the quantified type of a value from
the TyConBinders of a type or class.  For that we need not
TyConBinders but TyVarBinders (used in forall-type)  E.g:

 *  From   data T a = MkT (Maybe a)
    we are going to make a data constructor with type
           MkT :: forall a. Maybe a -> T a
    See the TyVarBinders passed to buildDataCon

 * From    class C a where { op :: a -> Maybe a }
   we are going to make a default method
           $dmop :: forall a. C a => a -> Maybe a
   See the TyVarBindres passed to mkSigmaTy in mkDefaultMethodType

Both of these are user-callable.  (NB: default methods are not callable
directly by the user but rather via the code generated by 'deriving',
which uses visible type application; see mkDefMethBind.)

Since they are user-callable we must get their type-argument visibility
information right; and that info is in the TyConBinders.
Here is an example:

  data App a b = MkApp (a b) -- App :: forall {k}. (k->*) -> k -> *

The TyCon has

  tyConTyBinders = [ Named (TvBndr (k :: *) Inferred), Anon (k->*), Anon k ]

The TyConBinders for App line up with App's kind, given above.

But the DataCon MkApp has the type
  MkApp :: forall {k} (a:k->*) (b:k). a b -> App k a b

That is, its TyVarBinders should be

  dataConUnivTyVarBinders = [ TvBndr (k:*)    Inferred
                            , TvBndr (a:k->*) Specified
                            , TvBndr (b:k)    Specified ]

So tyConTyVarBinders converts TyCon's TyConBinders into TyVarBinders:
  - variable names from the TyConBinders
  - but changing Anon/Required to Specified

### Note: No Required TyBinder in terms

### Note: The binders/kind/arity fields of a TyCon

All TyCons have this group of fields
  tyConBinders :: [TyConBinder]
  tyConResKind :: Kind
  tyConTyVars  :: [TyVar] -- Cached = binderVars tyConBinders
  tyConKind    :: Kind    -- Cached = mkTyConKind tyConBinders tyConResKind
  tyConArity   :: Arity   -- Cached = length tyConBinders

They fit together like so:

* tyConBinders gives the telescope of type variables on the LHS of the
  type declaration.  For example:

    type App a (b :: k) = a b

  tyConBinders = [ TvBndr (k::*)   (NamedTCB Inferred)
                 , TvBndr (a:k->*) AnonTCB
                 , TvBndr (b:k)    AnonTCB ]

  Note that that are three binders here, including the
  kind variable k.

### Note: TyVarBndrs, TyVarBinders, TyConBinders, and visibility

* Each TyConBinder tyConBinders has a TyVar, and that TyVar may
  scope over some other part of the TyCon's definition. Eg
      type T a = a->a
  we have
      tyConBinders = [ TvBndr (a:*) AnonTCB ]
      synTcRhs     = a->a
  So the 'a' scopes over the synTcRhs

* From the tyConBinders and tyConResKind we can get the tyConKind
  E.g for our App example:
      App :: forall k. (k->*) -> k -> *

  We get a 'forall' in the kind for each NamedTCB, and an arrow
  for each AnonTCB

  tyConKind is the full kind of the TyCon, not just the result kind

* tyConArity is the arguments this TyCon must be applied to, to be
  considered saturated.  Here we mean "applied to in the actual Type",
  not surface syntax; i.e. including implicit kind variables.
  So it's just (length tyConBinders)


# The TyCon type


### Note: Closed type families

* In an open type family you can add new instances later.  This is the
  usual case.

* In a closed type family you can only put equations where the family
  is defined.

A non-empty closed type family has a single axiom with multiple
branches, stored in the 'ClosedSynFamilyTyCon' constructor.  A closed
type family with no equations does not have an axiom, because there is
nothing for the axiom to prove!

### Note: Promoted data constructors

All data constructors can be promoted to become a type constructor,
via the PromotedDataCon alternative in TyCon.

* The TyCon promoted from a DataCon has the *same* Name and Unique as
  the DataCon.  Eg. If the data constructor Data.Maybe.Just(unique 78,
  say) is promoted to a TyCon whose name is Data.Maybe.Just(unique 78)

# PrimRep


### Note: rep swamp

GHC has a rich selection of types that represent "primitive types" of
one kind or another.  Each of them makes a different set of
distinctions, and mostly the differences are for good reasons,
although it's probably true that we could merge some of these.

Roughly in order of "includes more information":

 - A Width (cmm/CmmType) is simply a binary value with the specified
   number of bits.  It may represent a signed or unsigned integer, a
   floating-point value, or an address.

    data Width = W8 | W16 | W32 | W64 | W80 | W128

 - Size, which is used in the native code generator, is Width +
   floating point information.

   data Size = II8 | II16 | II32 | II64 | FF32 | FF64 | FF80

   it is necessary because e.g. the instruction to move a 64-bit float
   on x86 (movsd) is different from the instruction to move a 64-bit
   integer (movq), so the mov instruction is parameterised by Size.

 - CmmType wraps Width with more information: GC ptr, float, or
   other value.

    data CmmType = CmmType CmmCat Width

    data CmmCat     -- "Category" (not exported)
       = GcPtrCat   -- GC pointer
       | BitsCat    -- Non-pointer
       | FloatCat   -- Float

   It is important to have GcPtr information in Cmm, since we generate
   info tables containing pointerhood for the GC from this.  As for
   why we have float (and not signed/unsigned) here, see Note [Signed
   vs unsigned].

 - ArgRep makes only the distinctions necessary for the call and
   return conventions of the STG machine.  It is essentially CmmType
   + void.

 - PrimRep makes a few more distinctions than ArgRep: it divides
   non-GC-pointers into signed/unsigned and addresses, information
   that is necessary for passing these values to foreign functions.

There's another tension here: whether the type encodes its size in
bytes, or whether its size depends on the machine word size.  Width
and CmmType have the size built-in, whereas ArgRep and PrimRep do not.

This means to turn an ArgRep/PrimRep into a CmmType requires DynFlags.

On the other hand, CmmType includes some "nonsense" values, such as
CmmType GcPtrCat W32 on a 64-bit machine.


# Field labels


# \subsection{TyCon Construction}


Note: the TyCon constructors all take a Kind as one argument, even though
they could, in principle, work out their Kind from their other arguments.
But to do so they need functions from Types, and that makes a nasty
module mutual-recursion.  And they aren't called from many places.
So we compromise, and move their Kind calculation to the call site.


 no scoped vars 

### Note: Product types

A product type is
 * A data type (not a newtype)
 * With one, boxed data constructor
 * That binds no existential type variables

The main point is that product types are amenable to unboxing for
  * Strict function calls; we can transform
        f (D a b) = e
    to
        fw a b = e
    via the worker/wrapper transformation.  (Question: couldn't this
    work for existentials too?)

Note that the data constructor /can/ have evidence arguments: equality
constraints, type classes etc.  So it can be GADT.  These evidence
arguments are simply value arguments, and should not get in the way.



-----------------------------------------------
--      Expand type-constructor applications
-----------------------------------------------


# \subsection[TyCon-instances]{Instance declarations for @TyCon@}


@TyCon@s are compared by comparing their @Unique@s.


# Walking over recursive TyCons


### Note: Expanding newtypes and products

When expanding a type to expose a data-type constructor, we need to be
careful about newtypes, lest we fall into an infinite loop. Here are
the key examples:

  newtype Id  x = MkId x
  newtype Fix f = MkFix (f (Fix f))
  newtype T     = MkT (T -> T)

  Type           Expansion
 --------------------------
  T              T -> T
  Fix Maybe      Maybe (Fix Maybe)
  Id (Id Int)    Int
  Fix Id         NO NO NO

Notice that
 * We can expand T, even though it's recursive.
 * We can expand Id (Id Int), even though the Id shows up
   twice at the outer level, because Id is non-recursive

So, when expanding, we keep track of when we've seen a recursive
newtype at outermost level; and bail out if we see it again.

We sometimes want to do the same for product types, so that the
strictness analyser doesn't unbox infinitely deeply.

More precisely, we keep a *count* of how many times we've seen it.
This is to account for
   data instance T (a,b) = MkT (T a) (T b)
Then (Trac #10482) if we have a type like
        T (Int,(Int,(Int,(Int,Int))))
we can still unbox deeply enough during strictness analysis.
We have to treat T as potentially recursive, but it's still
good to be able to unwrap multiple layers.

The function that manages all this is checkRecTc.
