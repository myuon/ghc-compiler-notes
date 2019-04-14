`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/prelude/TysPrim.hs>`_

compiler/prelude/TysPrim.hs
===========================


Note [TYPE and RuntimeRep]
~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/prelude/TysPrim.hs#L400>`__

All types that classify values have a kind of the form (TYPE rr), where

    data RuntimeRep     -- Defined in ghc-prim:GHC.Types
      = LiftedRep
      | UnliftedRep
      | IntRep
      | FloatRep
      .. etc ..

::

    rr :: RuntimeRep

..

::

    TYPE :: RuntimeRep -> TYPE 'LiftedRep  -- Built in

..

So for example:
    Int        :: TYPE 'LiftedRep
    Array# Int :: TYPE 'UnliftedRep
    Int#       :: TYPE 'IntRep
    Float#     :: TYPE 'FloatRep
    Maybe      :: TYPE 'LiftedRep -> TYPE 'LiftedRep
    (# , #)    :: TYPE r1 -> TYPE r2 -> TYPE (TupleRep [r1, r2])

We abbreviate '*' specially:
    type * = TYPE 'LiftedRep

The 'rr' parameter tells us how the value is represented at runime.

Generally speaking, you can't be polymorphic in 'rr'.  E.g
   f :: forall (rr:RuntimeRep) (a:TYPE rr). a -> [a]
   f = /\(rr:RuntimeRep) (a:rr) \(a:rr). ...
This is no good: we could not generate code code for 'f', because the
calling convention for 'f' varies depending on whether the argument is
a a Int, Int#, or Float#.  (You could imagine generating specialised
code, one for each instantiation of 'rr', but we don't do that.)

Certain functions CAN be runtime-rep-polymorphic, because the code
generator never has to manipulate a value of type 'a :: TYPE rr'.

* error :: forall (rr:RuntimeRep) (a:TYPE rr). String -> a
  Code generator never has to manipulate the return value.

* unsafeCoerce#, defined in MkId.unsafeCoerceId:
  Always inlined to be a no-op
     unsafeCoerce# :: forall (r1 :: RuntimeRep) (r2 :: RuntimeRep)
                             (a :: TYPE r1) (b :: TYPE r2).
                             a -> b

* Unboxed tuples, and unboxed sums, defined in TysWiredIn
  Always inlined, and hence specialised to the call site
     (#,#) :: forall (r1 :: RuntimeRep) (r2 :: RuntimeRep)
                     (a :: TYPE r1) (b :: TYPE r2).
                     a -> b -> TYPE ('TupleRep '[r1, r2])



Note [PrimRep and kindPrimRep]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/prelude/TysPrim.hs#L454>`__

As part of its source code, in TyCon, GHC has
  data PrimRep = LiftedRep | UnliftedRep | IntRep | FloatRep | ...etc...

Notice that
 * RuntimeRep is part of the syntax tree of the program being compiled
     (defined in a library: ghc-prim:GHC.Types)
 * PrimRep is part of GHC's source code.
     (defined in TyCon)

We need to get from one to the other; that is what kindPrimRep does.
Suppose we have a value
   (v :: t) where (t :: k)
Given this kind
    k = TyConApp "TYPE" [rep]
GHC needs to be able to figure out how 'v' is represented at runtime.
It expects 'rep' to be form
    TyConApp rr_dc args
where 'rr_dc' is a promoteed data constructor from RuntimeRep. So
now we need to go from 'dc' to the corresponding PrimRep.  We store this
PrimRep in the promoted data constructor itself: see TyCon.promDcRepInfo.



Note [The equality types story]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/prelude/TysPrim.hs#L648>`__

GHC sports a veritable menagerie of equality types:

         Type or  Lifted?  Hetero?  Role      Built in         Defining module
         class?    L/U                        TyCon
-----------------------------------------------------------------------------------------
~#         T        U      hetero   nominal   eqPrimTyCon      GHC.Prim
~~         C        L      hetero   nominal   heqTyCon         GHC.Types
~          C        L      homo     nominal   eqTyCon          GHC.Types
:~:        T        L      homo     nominal   (not built-in)   Data.Type.Equality
:~~:       T        L      hetero   nominal   (not built-in)   Data.Type.Equality

~R#        T        U      hetero   repr      eqReprPrimTy     GHC.Prim
Coercible  C        L      homo     repr      coercibleTyCon   GHC.Types
Coercion   T        L      homo     repr      (not built-in)   Data.Type.Coercion
~P#        T        U      hetero   phantom   eqPhantPrimTyCon GHC.Prim

Recall that "hetero" means the equality can related types of different
kinds. Knowing that (t1 ~# t2) or (t1 ~R# t2) or even that (t1 ~P# t2)
also means that (k1 ~# k2), where (t1 :: k1) and (t2 :: k2).

To produce less confusion for end users, when not dumping and without
-fprint-equality-relations, each of these groups is printed as the bottommost
listed equality. That is, (~#) and (~~) are both rendered as (~) in
error messages, and (~R#) is rendered as Coercible.

Let's take these one at a time:

    --------------------------
    (~#) :: forall k1 k2. k1 -> k2 -> #
    --------------------------
This is The Type Of Equality in GHC. It classifies nominal coercions.
This type is used in the solver for recording equality constraints.
It responds "yes" to Type.isEqPrimPred and classifies as an EqPred in
Type.classifyPredType.

All wanted constraints of this type are built with coercion holes.
(See Note [Coercion holes] in TyCoRep.) But see also
Note [Deferred errors for coercion holes] in TcErrors to see how
equality constraints are deferred.

Within GHC, ~# is called eqPrimTyCon, and it is defined in TysPrim.


    --------------------------
    (~~) :: forall k1 k2. k1 -> k2 -> Constraint
    --------------------------
This is (almost) an ordinary class, defined as if by
  class a ~# b => a ~~ b
  instance a ~# b => a ~~ b
Here's what's unusual about it:

 * We can't actually declare it that way because we don't have syntax for ~#.
   And ~# isn't a constraint, so even if we could write it, it wouldn't kind
   check.

 * Users cannot write instances of it.

 * It is "naturally coherent". This means that the solver won't hesitate to
   solve a goal of type (a ~~ b) even if there is, say (Int ~~ c) in the
   context. (Normally, it waits to learn more, just in case the given
   influences what happens next.) See Note [Naturally coherent classes]
   in TcInteract.

 * It always terminates. That is, in the UndecidableInstances checks, we
   don't worry if a (~~) constraint is too big, as we know that solving
   equality terminates.

On the other hand, this behaves just like any class w.r.t. eager superclass
unpacking in the solver. So a lifted equality given quickly becomes an unlifted
equality given. This is good, because the solver knows all about unlifted
equalities. There is some special-casing in TcInteract.matchClassInst to
pretend that there is an instance of this class, as we can't write the instance
in Haskell.

Within GHC, ~~ is called heqTyCon, and it is defined in TysWiredIn.


    --------------------------
    (~) :: forall k. k -> k -> Constraint
    --------------------------
This is /exactly/ like (~~), except with a homogeneous kind.
It is an almost-ordinary class defined as if by
  class a ~# b => (a :: k) ~ (b :: k)
  instance a ~# b => a ~ b

 * All the bullets for (~~) apply

 * In addition (~) is magical syntax, as ~ is a reserved symbol.
   It cannot be exported or imported.

Within GHC, ~ is called eqTyCon, and it is defined in TysWiredIn.

Historical note: prior to July 18 (~) was defined as a
  more-ordinary class with (~~) as a superclass.  But that made it
  special in different ways; and the extra superclass selections to
  get from (~) to (~#) via (~~) were tiresome.  Now it's defined
  uniformly with (~~) and Coercible; much nicer.)


    --------------------------
    (:~:) :: forall k. k -> k -> *
    (:~~:) :: forall k1 k2. k1 -> k2 -> *
    --------------------------
These are perfectly ordinary GADTs, wrapping (~) and (~~) resp.
They are not defined within GHC at all.


    --------------------------
    (~R#) :: forall k1 k2. k1 -> k2 -> #
    --------------------------
The is the representational analogue of ~#. This is the type of representational
equalities that the solver works on. All wanted constraints of this type are
built with coercion holes.

Within GHC, ~R# is called eqReprPrimTyCon, and it is defined in TysPrim.


    --------------------------
    Coercible :: forall k. k -> k -> Constraint
    --------------------------
This is quite like (~~) in the way it's defined and treated within GHC, but
it's homogeneous. Homogeneity helps with type inference (as GHC can solve one
kind from the other) and, in my (Richard's) estimation, will be more intuitive
for users.

An alternative design included HCoercible (like (~~)) and Coercible (like (~)).
One annoyance was that we want `coerce :: Coercible a b => a -> b`, and
we need the type of coerce to be fully wired-in. So the HCoercible/Coercible
split required that both types be fully wired-in. Instead of doing this,
I just got rid of HCoercible, as I'm not sure who would use it, anyway.

Within GHC, Coercible is called coercibleTyCon, and it is defined in
TysWiredIn.


    --------------------------
    Coercion :: forall k. k -> k -> *
    --------------------------
This is a perfectly ordinary GADT, wrapping Coercible. It is not defined
within GHC at all.


    --------------------------
    (~P#) :: forall k1 k2. k1 -> k2 -> #
    --------------------------
This is the phantom analogue of ~# and it is barely used at all.
(The solver has no idea about this one.) Here is the motivation:

    data Phant a = MkPhant
    type role Phant phantom

::

    Phant <Int, Bool>_P :: Phant Int ~P# Phant Bool

..

We just need to have something to put on that last line. You probably
don't need to worry about it.



Note [The State# TyCon]
~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/prelude/TysPrim.hs#L808>`__

State# is the primitive, unlifted type of states.  It has one type parameter,
thus
        State# RealWorld
or
        State# s

where s is a type variable. The only purpose of the type parameter is to
keep different state threads separate.  It is represented by nothing at all.

The type parameter to State# is intended to keep separate threads separate.
Even though this parameter is not used in the definition of State#, it is
given role Nominal to enforce its intended use.

