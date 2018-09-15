[[src]](https://github.com/ghc/ghc/tree/master/compiler/basicTypes/DataCon.hs)

(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1998

# @DataCon@: Data Constructors

# Data constructor representation

Consider the following Haskell data type declaration

        data T = T !Int ![Int]

Using the strictness annotations, GHC will represent this as

        data T = T Int# [Int]

That is, the Int has been unboxed.  Furthermore, the Haskell source construction

        T e1 e2

is translated to

        case e1 of { I# x ->
        case e2 of { r ->
        T x r }}

That is, the first argument is unboxed, and the second is evaluated.  Finally,
pattern matching is translated too:

        case e of { T a b -> ... }

becomes

        case e of { T a' b -> let a = I# a' in ... }

To keep ourselves sane, we name the different versions of the data constructor
differently, as follows.

### Note: Data Constructor Naming

Each data constructor C has two, and possibly up to four, Names associated with it:

                   OccName   Name space   Name of   Notes
 ---------------------------------------------------------------------------
 The "data con itself"   C     DataName   DataCon   In dom( GlobalRdrEnv )
 The "worker data con"   C     VarName    Id        The worker
 The "wrapper data con"  $WC   VarName    Id        The wrapper
 The "newtype coercion"  :CoT  TcClsName  TyCon

### Note: The need for a wrapper

Each of these three has a distinct Unique.  The "data con itself" name
appears in the output of the renamer, and names the Haskell-source
data constructor.  The type checker translates it into either the wrapper Id
(if it exists) or worker Id (otherwise).

The data con has one or two Ids associated with it:

The "worker Id", is the actual data constructor.
* Every data constructor (newtype or data type) has a worker

* The worker is very like a primop, in that it has no binding.

* For a *data* type, the worker *is* the data constructor;
  it has no unfolding

* For a *newtype*, the worker has a compulsory unfolding which
  does a cast, e.g.
        newtype T = MkT Int
        The worker for MkT has unfolding
                \\(x:Int). x `cast` sym CoT
  Here CoT is the type constructor, witnessing the FC axiom
        axiom CoT : T = Int

The "wrapper Id", \$WC, goes as follows

* Its type is exactly what it looks like in the source program.

* It is an ordinary function, and it gets a top-level binding
  like any other function.

* The wrapper Id isn't generated for a data type if there is
  nothing for the wrapper to do.  That is, if its defn would be
        \$wC = C

### Note: The need for a wrapper

Why might the wrapper have anything to do?  Two reasons:

* Unboxing strict fields (with -funbox-strict-fields)
        data T = MkT !(Int,Int)
        \$wMkT :: (Int,Int) -> T
        \$wMkT (x,y) = MkT x y
  Notice that the worker has two fields where the wapper has
  just one.  That is, the worker has type
                MkT :: Int -> Int -> T

* Equality constraints for GADTs
        data T a where { MkT :: a -> T [a] }

  The worker gets a type with explicit equality
  constraints, thus:
        MkT :: forall a b. (a=[b]) => b -> T a

  The wrapper has the programmer-specified type:
        \$wMkT :: a -> T [a]
        \$wMkT a x = MkT [a] a [a] x
  The third argument is a coercion
        [a] :: [a]~[a]

INVARIANT: the dictionary constructor for a class
           never has a wrapper.

# A note about the stupid context

Data types can have a context:

        data (Eq a, Ord b) => T a b = T1 a b | T2 a

and that makes the constructors have a context too
(notice that T2's context is "thinned"):

        T1 :: (Eq a, Ord b) => a -> b -> T a b
        T2 :: (Eq a) => a -> T a b

Furthermore, this context pops up when pattern matching
(though GHC hasn't implemented this, but it is in H98, and
I've fixed GHC so that it now does):

        f (T2 x) = x
gets inferred type
        f :: Eq a => T a b -> a

I say the context is "stupid" because the dictionaries passed
are immediately discarded -- they do nothing and have no benefit.
It's a flaw in the language.

        Up to now [March 2002] I have put this stupid context into the
        type of the "wrapper" constructors functions, T1 and T2, but
        that turned out to be jolly inconvenient for generics, and
        record update, and other functions that build values of type T
        (because they don't have suitable dictionaries available).

        So now I've taken the stupid context out.  I simply deal with
        it separately in the type checker on occurrences of a
        constructor, either in an expression or in a pattern.

        [May 2003: actually I think this decision could easily be
        reversed now, and probably should be.  Generics could be
        disabled for types with a stupid context; record updates now
        (H98) needs the context too; etc.  It's an unforced change, so
        I'm leaving it for now --- but it does seem odd that the
        wrapper doesn't include the stupid context.]

[July 04] With the advent of generalised data types, it's less obvious
what the "stupid context" is.  Consider
        C :: forall a. Ord a => a -> a -> T (Foo a)
Does the C constructor in Core contain the Ord dictionary?  Yes, it must:

        f :: T b -> Ordering
        f = /\b. \x:T b.
            case x of
                C a (d:Ord a) (p:a) (q:a) -> compare d p q

Note that (Foo a) might not be an instance of Ord.

# \subsection{Data constructors}


### Note: TyVarBinders in DataCons

For the TyVarBinders in a DataCon and PatSyn:

### Note: No Required TyBinder in terms

Why do we need the TyVarBinders, rather than just the TyVars?  So that
we can construct the right type for the DataCon with its foralls
attributed the correct visibility.  That in turn governs whether you
can use visible type application at a call of the data constructor.

See also [DataCon user type variable binders] for an extended discussion on the
order in which TyVarBinders appear in a DataCon.

### Note: DataCon arities

dcSourceArity does not take constraints into account,
but dcRepArity does.  For example:
   MkT :: Ord a => a -> T a
    dcSourceArity = 1
    dcRepArity    = 2

### Note: DataCon user type variable binders

In System FC, data constructor type signatures always quantify over all of
their universal type variables, followed by their existential type variables.
Normally, this isn't a problem, as most datatypes naturally quantify their type
variables in this order anyway. For example:

  data T a b = forall c. MkT b c

Here, we have `MkT :: forall {k} (a :: k) (b :: *) (c :: *). b -> c -> T a b`,
where k, a, and b are universal and c is existential. (The inferred variable k
isn't available for TypeApplications, hence why it's in braces.) This is a
perfectly reasonable order to use, as the syntax of H98-style datatypes
(+ ExistentialQuantification) suggests it.

Things become more complicated when GADT syntax enters the picture. Consider
this example:

  data X a where
    MkX :: forall b a. b -> Proxy a -> X a

If we adopt the earlier approach of quantifying all the universal variables
followed by all the existential ones, GHC would come up with this type
signature for MkX:

  MkX :: forall {k} (a :: k) (b :: *). b -> Proxy a -> X a

But this is not what we want at all! After all, if a user were to use
TypeApplications on MkX, they would expect to instantiate `b` before `a`,
as that's the order in which they were written in the `forall`. (See #11721.)
Instead, we'd like GHC to come up with this type signature:

  MkX :: forall {k} (b :: *) (a :: k). b -> Proxy a -> X a

In fact, even if we left off the explicit forall:

  data X a where
    MkX :: b -> Proxy a -> X a

Then a user should still expect `b` to be quantified before `a`, since
according to the rules of TypeApplications, in the absence of `forall` GHC
performs a stable topological sort on the type variables in the user-written
type signature, which would place `b` before `a`.

But as noted above, enacting this behavior is not entirely trivial, as System
FC demands the variables go in universal-then-existential order under the hood.
Our solution is thus to equip DataCon with two different sets of type
variables:

* dcUnivTyVars and dcExTyVars, for the universal and existential type
  variables, respectively. Their order is irrelevant for the purposes of
  TypeApplications, and as a consequence, they do not come equipped with
  visibilities (that is, they are TyVars instead of TyVarBinders).
* dcUserTyVarBinders, for the type variables binders in the order in which they
  originally arose in the user-written type signature. Their order *does*
  matter for TypeApplications, so they are full TyVarBinders, complete
  with visibilities.

This encoding has some redundancy. The set of tyvars in dcUserTyVarBinders
consists precisely of:

* The set of tyvars in dcUnivTyVars whose type variables do not appear in
  dcEqSpec, unioned with:
* The set of tyvars in dcExTyVars

The word "set" is used above because the order in which the tyvars
appear in dcUserTyVarBinders can be completely different from the order in
dcUnivTyVars or dcExTyVars. That is, the tyvars in dcUserTyVarBinders are a
permutation of (dcExTyVars + a subset of dcUnivTyVars). But aside from the
ordering, they in fact share the same type variables (with the same Uniques).
We sometimes refer to this as "the dcUserTyVarBinders invariant".

dcUserTyVarBinders, as the name suggests, is the one that users will see most
of the time. It's used when computing the type signature of a data constructor
(see dataConUserType), and as a result, it's what matters from a
TypeApplications perspective.


### Note: Bangs on data constructor arguments

# \subsection{Construction}


# 



# \subsection{Splitting products}


# Building an algebraic data type


buildAlgTyCon is here because it is called from TysWiredIn, which can
depend on this module, but not on BuildTyCl.
