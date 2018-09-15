[[src]](https://github.com/ghc/ghc/tree/master/compiler/types/Type.hs)
# Type representation


### Note: coreView vs tcView

So far as the typechecker is concerned, 'Constraint' and 'TYPE LiftedRep' are distinct kinds.

But in Core these two are treated as identical.

We implement this by making 'coreView' convert 'Constraint' to 'TYPE LiftedRep' on the fly.
The function tcView (used in the type checker) does not do this.

See also Trac #11715, which tracks removing this inconsistency.



# Analyzing types


These functions do a map-like operation over types, performing some operation
on all variables and binding sites. Primarily used for zonking.

### Note: Efficiency for mapCoercion ForAllCo case

### Note: Forall coercions

The problem is that tcm_tybinder will affect the TyVar's kind and
mapCoercion will affect the Coercion, and we hope that the results will be
the same. Even if they are the same (which should generally happen with
correct algorithms), then there is an efficiency issue. In particular,
this problem seems to make what should be a linear algorithm into a potentially
exponential one. But it's only going to be bad in the case where there's
lots of foralls in the kinds of other foralls. Like this:

  forall a : (forall b : (forall c : ...). ...). ...

This construction seems unlikely. So we'll do the inefficient, easy way
for now.

### Note: Specialising mappers

These INLINABLE pragmas are indispensable. mapType/mapCoercion are used
to implement zonking, and it's vital that they get specialised to the TcM
monad. This specialisation happens automatically (that is, without a
SPECIALISE pragma) as long as the definitions are INLINABLE. For example,
this one change made a 20% allocation difference in perf/compiler/T5030.



# \subsection{Constructor-specific functions}



---------------------------------------------------------------------
                                TyVarTy
                                ~~~~~~~




---------------------------------------------------------------------
                                AppTy
                                ~~~~~

We need to be pretty careful with AppTy to make sure we obey the
invariant that a TyConApp is always visibly so.  mkAppTy maintains the
invariant: use it.

### Note: Decomposing fat arrow c=>t

Can we unify (a b) with (Eq a => ty)?   If we do so, we end up with
a partial application like ((=>) Eq a) which doesn't make sense in
source Haskell.  In contrast, we *can* unify (a b) with (t1 -> t2).
Here's an example (Trac #9858) of how you might do it:
   i :: (Typeable a, Typeable b) => Proxy (a b) -> TypeRep
   i p = typeRep p

   j = i (Proxy :: Proxy (Eq Int => Int))
The type (Proxy (Eq Int => Int)) is only accepted with -XImpredicativeTypes,
but suppose we want that.  But then in the call to 'i', we end
up decomposing (Eq Int => Int), and we definitely don't want that.

This really only applies to the type checker; in Core, '=>' and '->'
are the same, as are 'Constraint' and '*'.  But for now I've put
the test in repSplitAppTy_maybe, which applies throughout, because
the other calls to splitAppTy are in Unify, which is also used by
the type checker (e.g. when matching type-function equations).




                      LitTy
                      ~~~~~




---------------------------------------------------------------------
                                FunTy
                                ~~~~~

### Note: Representation of function types


Functions (e.g. Int -> Char) are can be thought of as being applications
of funTyCon (known in Haskell surface syntax as (->)),

    (->) :: forall (r1 :: RuntimeRep) (r2 :: RuntimeRep)
                   (a :: TYPE r1) (b :: TYPE r2).
            a -> b -> Type

However, for efficiency's sake we represent saturated applications of (->)
with FunTy. For instance, the type,

    (->) r1 r2 a b

is equivalent to,

    FunTy (Anon a) b

Note how the RuntimeReps are implied in the FunTy representation. For this
reason we must be careful when recontructing the TyConApp representation (see,
for instance, splitTyConApp_maybe).

In the compiler we maintain the invariant that all saturated applications of
(->) are represented with FunTy.

See #11714.



---------------------------------------------------------------------
                                TyConApp
                                ~~~~~~~~




---------------------------------------------------------------------
                           CastTy
                           ~~~~~~

A casted type has its *kind* casted into something new.

### Note: No reflexive casts in types

As far as possible, we would like to maintain the following property:

  (*) If (t1 `eqType` t2), then t1 and t2 are treated identically within GHC.

The (*) property is very useful, because we have a tendency to compare two
types to see if they're equal, and then arbitrarily choose one. We don't
want this arbitrary choice to then matter later on. Maintaining (*) means
that every function that looks at a structure of a type must think about
casts. In places where we directly pattern-match, this consideration is
forced by consideration of the CastTy constructor.

But, when we call a splitXXX function, it's easy to ignore the possibility
of casts. In particular, splitTyConApp is used extensively, and we don't
want it to fail on (T a b c |> co). Happily, if we have
  (T a b c |> co) `eqType` (T d e f)
then co must be reflexive. Why? eqType checks that the kinds are equal, as
well as checking that (a `eqType` d), (b `eqType` e), and (c `eqType` f).
By the kind check, we know that (T a b c |> co) and (T d e f) have the same
kind. So the only way that co could be non-reflexive is for (T a b c) to have
a different kind than (T d e f). But because T's kind is closed (all tycon kinds
are closed), the only way for this to happen is that one of the arguments has
to differ, leading to a contradiction. Thus, co is reflexive.

Accordingly, by eliminating reflexive casts, splitTyConApp need not worry
about outermost casts to uphold (*).

Unfortunately, that's not the end of the story. Consider comparing
  (T a b c)      =?       (T a b |> (co -> <Type>)) (c |> sym co)
These two types have the same kind (Type), but the left type is a TyConApp
while the right type is not. To handle this case, we will have to implement
some variant of the dreaded KPush algorithm (c.f. CoreOpt.pushCoDataCon).
This stone is left unturned for now, meaning that we don't yet uphold (*).

The other place where (*) will be hard to guarantee is in splitAppTy, because
I (Richard E) can't think of a way to push coercions into AppTys. The good
news here is that splitAppTy is not used all that much, and so all clients of
that function can simply be told to use splitCastTy as well, in order to
uphold (*). This, too, is left undone, for now.




--------------------------------------------------------------------
                            CoercionTy
                            ~~~~~~~~~~

CoercionTy allows us to inject coercions into types. A CoercionTy
should appear only in the right-hand side of an application.



---------------------------------------------------------------------
                                SynTy
                                ~~~~~

# Notes on type synonyms

The various "split" functions (splitFunTy, splitRhoTy, splitForAllTy) try
to return type synonyms wherever possible. Thus

        type Foo a = a -> a

we want
        splitFunTys (a -> Foo a) = ([a], Foo a)
not                                ([a], a -> a)

The reason is that we then get better (shorter) type signatures in
interfaces.  Notably this plays a role in tcTySigs in TcBinds.hs.


---------------------------------------------------------------------
                                ForAllTy
                                ~~~~~~~~



# 

# 

Predicates on PredType

### Note: isPredTy complications

You would think that we could define
  isPredTy ty = isConstraintKind (typeKind ty)
But there are a number of complications:

* isPredTy is used when printing types, which can happen in debug
  printing during type checking of not-fully-zonked types.  So it's
  not cool to say isConstraintKind (typeKind ty) because, absent
  zonking, the type might be ill-kinded, and typeKind crashes. Hence the
  rather tiresome story here

* isPredTy must return "True" to *unlifted* coercions, such as (t1 ~# t2)
  and (t1 ~R# t2), which are not of kind Constraint!  Currently they are
  of kind #.

* If we do form the type '(C a => C [a]) => blah', then we'd like to
  print it as such. But that means that isPredTy must return True for
  (C a => C [a]).  Admittedly that type is illegal in Haskell, but we
  want to print it nicely in error messages.



Make PredTypes

--------------------- Equality types ---------------------------------


### Note: Dictionary-like types

Being "dictionary-like" means either a dictionary type or a tuple thereof.
In GHC 6.10 we build implication constraints which construct such tuples,
and if we land up with a binding
    t :: (C [a], Eq [a])
    t = blah
then we want to treat t as cheap under "-fdicts-cheap" for example.
(Implication constraints are normally inlined, but sadly not if the
occurrence is itself inside an INLINE function!  Until we revise the
handling of implication constraints, that is.)  This turned out to
be important in getting good arities in DPH code.  Example:

    class C a
    class D a where { foo :: a -> a }
    instance C a => D (Maybe a) where { foo x = x }

# 

# \subsection{Type families}


# \subsection{Liftedness}


# \subsection{Join points}


# \subsection{Sequencing on types}


# Comparison for types
        (We don't use instances so that we know where it happens)


### Note: Equality on AppTys

In our cast-ignoring equality, we want to say that the following two
are equal:

  (Maybe |> co) (Int |> co')   ~?       Maybe Int

But the left is an AppTy while the right is a TyConApp. The solution is
to use repSplitAppTy_maybe to break up the TyConApp into its pieces and
then continue. Easy to do, but also easy to forget to do.



### Note: nonDetCmpType nondeterminism

### Note: Unique Determinism

# The kind of a type


# 

