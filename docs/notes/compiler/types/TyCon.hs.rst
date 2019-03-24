`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/types/TyCon.hs>`_

====================
compiler/types/TyCon.hs.rst
====================

Note [Type synonym families]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
* Type synonym families, also known as "type functions", map directly
  onto the type functions in FC:

.. code-block:: haskell

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

.. code-block:: haskell

        type family G a :: * where
          G Int = Bool
          G Bool = Char
          G a = ()
  translates to
    a FamilyTyCon 'G', whose FamTyConFlav is ClosedSynFamilyTyCon, with the
    appropriate CoAxiom representing the equations

We also support injective type families -- see Note [Injective type families]



Note [Data type families]
~~~~~~~~~~~~~~~~~~~~~~~~~
See also Note [Wrappers for data instance tycons] in MkId.hs

* Data type families are declared thus
        data family T a :: *
        data instance T Int = T1 | T2 Bool

.. code-block:: haskell

  Here T is the "family TyCon".

* Reply "yes" to isDataFamilyTyCon, and isFamilyTyCon

* The user does not see any "equivalent types" as he did with type
  synonym families.  He just sees constructors with types
        T1 :: T Int
        T2 :: Bool -> T Int

* Here's the FC version of the above declarations:

.. code-block:: haskell

        data T a
        data R:TInt = T1 | T2 Bool
        axiom ax_ti : T Int ~R R:TInt

.. code-block:: haskell

  Note that this is a *representational* coercion
  The R:TInt is the "representation TyCons".
  It has an AlgTyConFlav of
        DataFamInstTyCon T [Int] ax_ti

* The axiom ax_ti may be eta-reduced; see
  Note [Eta reduction for data families] in FamInstEnv

* Data family instances may have a different arity than the data family.
  See Note [Arity of data families] in FamInstEnv

* The data constructor T2 has a wrapper (which is what the
  source-level "T2" invokes):

.. code-block:: haskell

        $WT2 :: Bool -> T Int
        $WT2 b = T2 b `cast` sym ax_ti

* A data instance can declare a fully-fledged GADT:

.. code-block:: haskell

        data instance T (a,b) where
          X1 :: T (Int,Bool)
          X2 :: a -> b -> T (a,b)

.. code-block:: haskell

  Here's the FC version of the above declaration:

.. code-block:: haskell

        data R:TPair a b where
          X1 :: R:TPair Int Bool
          X2 :: a -> b -> R:TPair a b
        axiom ax_pr :: T (a,b)  ~R  R:TPair a b

.. code-block:: haskell

        $WX1 :: forall a b. a -> b -> T (a,b)
        $WX1 a b (x::a) (y::b) = X2 a b x y `cast` sym (ax_pr a b)

.. code-block:: haskell

  The R:TPair are the "representation TyCons".
  We have a bit of work to do, to unpick the result types of the
  data instance declaration for T (a,b), to get the result type in the
  representation; e.g.  T (a,b) --> R:TPair a b

.. code-block:: haskell

  The representation TyCon R:TList, has an AlgTyConFlav of

.. code-block:: haskell

        DataFamInstTyCon T [(a,b)] ax_pr

* Notice that T is NOT translated to a FC type function; it just
  becomes a "data type" with no constructors, which can be coerced
  into R:TInt, R:TPair by the axioms.  These axioms
  axioms come into play when (and *only* when) you
        - use a data constructor
        - do pattern matching
  Rather like newtype, in fact

.. code-block:: haskell

  As a result

  - T behaves just like a data type so far as decomposition is concerned

  - (T Int) is not implicitly converted to R:TInt during type inference.
    Indeed the latter type is unknown to the programmer.

  - There *is* an instance for (T Int) in the type-family instance
    environment, but it is only used for overlap checking

  - It's fine to have T in the LHS of a type function:
    type instance F (T a) = [a]

.. code-block:: haskell

  It was this last point that confused me!  The big thing is that you
  should not think of a data family T as a *type function* at all, not
  even an injective one!  We can't allow even injective type functions
  on the LHS of a type function:
        type family injective G a :: *
        type instance F (G Int) = Bool
  is no good, even if G is injective, because consider
        type instance G Int = Bool
        type instance F Bool = Char

.. code-block:: haskell

  So a data type family is not an injective type function. It's just a
  data type with some axioms that connect it to other data types.

* The tyConTyVars of the representation tycon are the tyvars that the
  user wrote in the patterns. This is important in TcDeriv, where we
  bring these tyvars into scope before type-checking the deriving
  clause. This fact is arranged for in TcInstDecls.tcDataFamInstDecl.



Note [Associated families and their parent class]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*Associated* families are just like *non-associated* families, except
that they have a famTcParent field of (Just cls_tc), which identifies the
parent class.

However there is an important sharing relationship between
  * the tyConTyVars of the parent Class
  * the tyConTyVars of the associated TyCon

.. code-block:: haskell

   class C a b where
     data T p a
     type F a q b

Here the 'a' and 'b' are shared with the 'Class'; that is, they have
the same Unique.

This is important. In an instance declaration we expect
  * all the shared variables to be instantiated the same way
  * the non-shared variables of the associated type should not
    be instantiated at all

.. code-block:: haskell

  instance C [x] (Tree y) where
     data T p [x] = T1 x | T2 p
     type F [x] q (Tree y) = (x,y,q)



Note [TyCon Role signatures]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Every tycon has a role signature, assigning a role to each of the tyConTyVars
(or of equal length to the tyConArity, if there are no tyConTyVars). An
example demonstrates these best: say we have a tycon T, with parameters a at
nominal, b at representational, and c at phantom. Then, to prove
representational equality between T a1 b1 c1 and T a2 b2 c2, we need to have
nominal equality between a1 and a2, representational equality between b1 and
b2, and nothing in particular (i.e., phantom equality) between c1 and c2. This
might happen, say, with the following declaration:

.. code-block:: haskell

  data T a b c where
    MkT :: b -> T Int b c

Data and class tycons have their roles inferred (see inferRoles in TcTyDecls),
as do vanilla synonym tycons. Family tycons have all parameters at role N,
though it is conceivable that we could relax this restriction. (->)'s and
tuples' parameters are at role R. Each primitive tycon declares its roles;
it's worth noting that (~#)'s parameters are at role N. Promoted data
constructors' type arguments are at role R. All kind arguments are at role
N.



Note [Unboxed tuple RuntimeRep vars]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The contents of an unboxed tuple may have any representation. Accordingly,
the kind of the unboxed tuple constructor is runtime-representation
polymorphic. For example,

.. code-block:: haskell

   (#,#) :: forall (q :: RuntimeRep) (r :: RuntimeRep). TYPE q -> TYPE r -> #

These extra tyvars (v and w) cause some delicate processing around tuples,
where we used to be able to assume that the tycon arity and the
datacon arity were the same.



Note [Injective type families]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We allow injectivity annotations for type families (both open and closed):

.. code-block:: haskell

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



Note [AnonTCB InivsArg]
~~~~~~~~~~~~~~~~~~~~~~~~~~
It's pretty rare to have an (AnonTCB InvisArg) binder.  The
only way it can occur is in a PromotedDataCon whose
kind has an equality constraint:
  'MkT :: forall a b. (a~b) => blah
See Note [Constraints in kinds] in TyCoRep, and
Note [Promoted data constructors] in this module.

When mapping an (AnonTCB InvisArg) to an ArgFlag, in
tyConBndrVisArgFlag, we use "Inferred" to mean "the user cannot
specify this arguments, even with visible type/kind application;
instead the type checker must fill it in.

We map (AnonTCB VisArg) to Required, of course: the user must
provide it. It would be utterly wrong to do this for constraint
arguments, which is why AnonTCB must have the AnonArgFlag in
the first place.



Note [Building TyVarBinders from TyConBinders]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We sometimes need to build the quantified type of a value from
the TyConBinders of a type or class.  For that we need not
TyConBinders but TyVarBinders (used in forall-type)  E.g:

 *  From   data T a = MkT (Maybe a)
    we are going to make a data constructor with type
           MkT :: forall a. Maybe a -> T a
    See the TyCoVarBinders passed to buildDataCon

 * From    class C a where { op :: a -> Maybe a }
   we are going to make a default method
           $dmop :: forall a. C a => a -> Maybe a
   See the TyCoVarBinders passed to mkSigmaTy in mkDefaultMethodType

Both of these are user-callable.  (NB: default methods are not callable
directly by the user but rather via the code generated by 'deriving',
which uses visible type application; see mkDefMethBind.)

Since they are user-callable we must get their type-argument visibility
information right; and that info is in the TyConBinders.
Here is an example:

.. code-block:: haskell

  data App a b = MkApp (a b) -- App :: forall {k}. (k->*) -> k -> *

The TyCon has

.. code-block:: haskell

  tyConTyBinders = [ Named (Bndr (k :: *) Inferred), Anon (k->*), Anon k ]

The TyConBinders for App line up with App's kind, given above.

But the DataCon MkApp has the type
  MkApp :: forall {k} (a:k->*) (b:k). a b -> App k a b

That is, its TyCoVarBinders should be

.. code-block:: haskell

  dataConUnivTyVarBinders = [ Bndr (k:*)    Inferred
                            , Bndr (a:k->*) Specified
                            , Bndr (b:k)    Specified ]

So tyConTyVarBinders converts TyCon's TyConBinders into TyVarBinders:
  - variable names from the TyConBinders
  - but changing Anon/Required to Specified

The last part about Required->Specified comes from this:
  data T k (a:k) b = MkT (a b)
Here k is Required in T's kind, but we don't have Required binders in
the TyCoBinders for a term (see Note [No Required TyCoBinder in terms]
in TyCoRep), so we change it to Specified when making MkT's TyCoBinders


Note [The binders/kind/arity fields of a TyCon]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
All TyCons have this group of fields
  tyConBinders   :: [TyConBinder/TyConTyCoBinder]
  tyConResKind   :: Kind
  tyConTyVars    :: [TyVar]   -- Cached = binderVars tyConBinders
                              --   NB: Currently (Aug 2018), TyCons that own this
                              --   field really only contain TyVars. So it is
                              --   [TyVar] instead of [TyCoVar].
  tyConKind      :: Kind      -- Cached = mkTyConKind tyConBinders tyConResKind
  tyConArity     :: Arity     -- Cached = length tyConBinders

They fit together like so:

* tyConBinders gives the telescope of type/coercion variables on the LHS of the
  type declaration.  For example:

.. code-block:: haskell

    type App a (b :: k) = a b

.. code-block:: haskell

  tyConBinders = [ Bndr (k::*)   (NamedTCB Inferred)
                 , Bndr (a:k->*) AnonTCB
                 , Bndr (b:k)    AnonTCB ]

.. code-block:: haskell

  Note that that are three binders here, including the
  kind variable k.

* See Note [VarBndrs, TyCoVarBinders, TyConBinders, and visibility] in TyCoRep
  for what the visibility flag means.

* Each TyConBinder tyConBinders has a TyVar (sometimes it is TyCoVar), and
  that TyVar may scope over some other part of the TyCon's definition. Eg
      type T a = a -> a
  we have
      tyConBinders = [ Bndr (a:*) AnonTCB ]
      synTcRhs     = a -> a
  So the 'a' scopes over the synTcRhs

* From the tyConBinders and tyConResKind we can get the tyConKind
  E.g for our App example:
      App :: forall k. (k->*) -> k -> *

.. code-block:: haskell

  We get a 'forall' in the kind for each NamedTCB, and an arrow
  for each AnonTCB

.. code-block:: haskell

  tyConKind is the full kind of the TyCon, not just the result kind

* For type families, tyConArity is the arguments this TyCon must be
  applied to, to be considered saturated.  Here we mean "applied to in
  the actual Type", not surface syntax; i.e. including implicit kind
  variables.  So it's just (length tyConBinders)

* For an algebraic data type, or data instance, the tyConResKind is
  always (TYPE r); that is, the tyConBinders are enough to saturate
  the type constructor.  I'm not quite sure why we have this invariant,
  but it's enforced by etaExpandAlgTyCon


Note [Closed type families]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
* In an open type family you can add new instances later.  This is the
  usual case.

* In a closed type family you can only put equations where the family
  is defined.

A non-empty closed type family has a single axiom with multiple
branches, stored in the 'ClosedSynFamilyTyCon' constructor.  A closed
type family with no equations does not have an axiom, because there is
nothing for the axiom to prove!




Note [Promoted data constructors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
All data constructors can be promoted to become a type constructor,
via the PromotedDataCon alternative in TyCon.

* The TyCon promoted from a DataCon has the *same* Name and Unique as
  the DataCon.  Eg. If the data constructor Data.Maybe.Just(unique 78,
  say) is promoted to a TyCon whose name is Data.Maybe.Just(unique 78)

* We promote the *user* type of the DataCon.  Eg
     data T = MkT {-# UNPACK #-} !(Bool, Bool)
  The promoted kind is
     'MkT :: (Bool,Bool) -> T
  *not*
     'MkT :: Bool -> Bool -> T

* Similarly for GADTs:
     data G a where
       MkG :: forall b. b -> G [b]
  The promoted data constructor has kind
       'MkG :: forall b. b -> G [b]
  *not*
       'MkG :: forall a b. (a ~# [b]) => b -> G a



Note [Enumeration types]
~~~~~~~~~~~~~~~~~~~~~~~~
We define datatypes with no constructors to *not* be
enumerations; this fixes trac #2578,  Otherwise we
end up generating an empty table for
  <mod>_<type>_closure_tbl
which is used by tagToEnum# to map Int# to constructors
in an enumeration. The empty table apparently upset
the linker.

Moreover, all the data constructor must be enumerations, meaning
they have type  (forall abc. T a b c).  GADTs are not enumerations.
For example consider
    data T a where
      T1 :: T Int
      T2 :: T Bool
      T3 :: T a
What would [T1 ..] be?  [T1,T3] :: T Int? Easiest thing is to exclude them.
See #4528.



Note [Newtype coercions]
~~~~~~~~~~~~~~~~~~~~~~~~
The NewTyCon field nt_co is a CoAxiom which is used for coercing from
the representation type of the newtype, to the newtype itself. For
example,

.. code-block:: haskell

   newtype T a = MkT (a -> a)

the NewTyCon for T will contain nt_co = CoT where CoT t : T t ~ t -> t.

In the case that the right hand side is a type application
ending with the same type variables as the left hand side, we
"eta-contract" the coercion.  So if we had

.. code-block:: haskell

   newtype S a = MkT [a]

then we would generate the arity 0 axiom CoS : S ~ [].  The
primary reason we do this is to make newtype deriving cleaner.

In the paper we'd write
        axiom CoT : (forall t. T t) ~ (forall t. [t])
and then when we used CoT at a particular type, s, we'd say
        CoT @ s
which encodes as (TyConApp instCoercionTyCon [TyConApp CoT [], s])



Note [Newtype eta]
~~~~~~~~~~~~~~~~~~
Consider
        newtype Parser a = MkParser (IO a) deriving Monad
Are these two types equal (to Core)?
        Monad Parser
        Monad IO
which we need to make the derived instance for Monad Parser.

Well, yes.  But to see that easily we eta-reduce the RHS type of
Parser, in this case to ([], Froogle), so that even unsaturated applications
of Parser will work right.  This eta reduction is done when the type
constructor is built, and cached in NewTyCon.

Here's an example that I think showed up in practice
Source code:
        newtype T a = MkT [a]
        newtype Foo m = MkFoo (forall a. m a -> Int)

.. code-block:: haskell

        w1 :: Foo []
        w1 = ...

.. code-block:: haskell

        w2 :: Foo T
        w2 = MkFoo (\(MkT x) -> case w1 of MkFoo f -> f x)

After desugaring, and discarding the data constructors for the newtypes,
we get:
        w2 = w1 `cast` Foo CoT
so the coercion tycon CoT must have
        kind:    T ~ []
 and    arity:   0

This eta-reduction is implemented in BuildTyCl.mkNewTyConRhs.




Note [Product types]
~~~~~~~~~~~~~~~~~~~~~~~
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

  * CPR for function results; we can transform
        f x y = let ... in D a b
    to
        fw x y = let ... in (# a, b #)

Note that the data constructor /can/ have evidence arguments: equality
constraints, type classes etc.  So it can be GADT.  These evidence
arguments are simply value arguments, and should not get in the way.


Note [Constructor tag allocation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When typechecking we need to allocate constructor tags to constructors.
They are allocated based on the position in the data_cons field of TyCon,
with the first constructor getting fIRST_TAG.

We used to pay linear cost per constructor, with each constructor looking up
its relative index in the constructor list. That was quadratic and prohibitive
for large data types with more than 10k constructors.

The current strategy is to build a NameEnv with a mapping from costructor's
Name to ConTag and pass it down to buildDataCon for efficient lookup.

Relevant ticket: #14657


Note [Expanding newtypes and products]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When expanding a type to expose a data-type constructor, we need to be
careful about newtypes, lest we fall into an infinite loop. Here are
the key examples:

.. code-block:: haskell

  newtype Id  x = MkId x
  newtype Fix f = MkFix (f (Fix f))
  newtype T     = MkT (T -> T)

.. code-block:: haskell

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
Then (#10482) if we have a type like
        T (Int,(Int,(Int,(Int,Int))))
we can still unbox deeply enough during strictness analysis.
We have to treat T as potentially recursive, but it's still
good to be able to unwrap multiple layers.

The function that manages all this is checkRecTc.


Note [Skolem abstract data]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
Skolem abstract data arises from data declarations in an hsig file.

The best analogy is to interpret the types declared in signature files as
elaborating to universally quantified type variables; e.g.,

.. code-block:: haskell

   unit p where
       signature H where
           data T
           data S
       module M where
           import H
           f :: (T ~ S) => a -> b
           f x = x

elaborates as (with some fake structural types):

.. code-block:: haskell

   p :: forall t s. { f :: forall a b. t ~ s => a -> b }
   p = { f = \x -> x } -- ill-typed

It is clear that inside p, t ~ s is not provable (and
if we tried to write a function to cast t to s, that
would not work), but if we call p @Int @Int, clearly Int ~ Int
is provable.  The skolem variables are all distinct from
one another, but we can't make assumptions like "f is
inaccessible", because the skolem variables will get
instantiated eventually!

Skolem abstractness can apply to "non-abstract" data as well):

.. code-block:: haskell

   unit p where
       signature H1 where
           data T = MkT
       signature H2 where
           data T = MkT
       module M where
           import qualified H1
           import qualified H2
           f :: (H1.T ~ H2.T) => a -> b
           f x = x

This is why the test is on the original name of the TyCon,
not whether it is abstract or not.

