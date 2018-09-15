[[src]](https://github.com/ghc/ghc/tree/master/compiler/hsSyn/HsUtils.hs)

(c) The University of Glasgow, 1992-2006


Here we collect a variety of helper functions that construct or
analyse HsSyn.  All these functions deal with generic HsSyn; functions
which deal with the instantiated versions are located elsewhere:

   Parameterised by          Module
   ----------------          -------------
   GhcPs/RdrName             parser/RdrHsSyn
   GhcRn/Name                rename/RnHsSyn
   GhcTc/Id                  typecheck/TcHsSyn


# Some useful helpers for constructing syntax


These functions attempt to construct a not-completely-useless SrcSpan
from their components, compared with the nl* functions below which
just attach noSrcSpan to everything.


# Constructing syntax with no location info



Tuples.  All these functions are *pre-typechecker* because they lack
types on the tuple.


# LHsSigType and LHsSigWcType


### Note: Kind signatures in typeToLHsType

There are types that typeToLHsType can produce which require explicit kind
signatures in order to kind-check. Here is an example from Trac #14579:

  newtype Wat (x :: Proxy (a :: Type)) = MkWat (Maybe a) deriving Eq
  newtype Glurp a = MkGlurp (Wat ('Proxy :: Proxy a)) deriving Eq

The derived Eq instance for Glurp (without any kind signatures) would be:

  instance Eq a => Eq (Glurp a) where
    (==) = coerce @(Wat 'Proxy -> Wat 'Proxy -> Bool)
                  @(Glurp a    -> Glurp a    -> Bool)
                  (==)

(Where the visible type applications use types produced by typeToLHsType.)

The type 'Proxy has an underspecified kind, so we must ensure that
typeToLHsType ascribes it with its kind: ('Proxy :: Proxy a).

We must be careful not to produce too many kind signatures, or else
typeToLHsType can produce noisy types like
('Proxy :: Proxy (a :: (Type :: Type))). In pursuit of this goal, we adopt the
following criterion for choosing when to annotate types with kinds:

* If there is a tycon application with any invisible arguments, annotate
  the tycon application with its kind.

Why is this the right criterion? The problem we encountered earlier was the
result of an invisible argument (the `a` in ('Proxy :: Proxy a)) being
underspecified, so producing a kind signature for 'Proxy will catch this.
If there are no invisible arguments, then there is nothing to do, so we can
avoid polluting the result type with redundant noise.

What about a more complicated tycon, such as this?

  T :: forall {j} (a :: j). a -> Type

Unlike in the previous 'Proxy example, annotating an application of `T` to an
argument (e.g., annotating T ty to obtain (T ty :: Type)) will not fix
its invisible argument `j`. But because we apply this strategy recursively,
`j` will be fixed because the kind of `ty` will be fixed! That is to say,
something to the effect of (T (ty :: j) :: Type) will be produced.

This strategy certainly isn't foolproof, as tycons that contain type families
in their kind might break down. But we'd likely need visible kind application
to make those work.


# --------- HsWrappers: type args, dict args, casts ---------


# Bindings; with a location at the top


# Collecting binders


Get all the binders in some HsBindGroups, IN THE ORDER OF APPEARANCE. eg.

...
where
  (x, y) = ...
  f i j  = ...
  [a, b] = ...

it should return [x, y, f, a, b] (remember, order important).

### Note: Collect binders only after renaming

These functions should only be used on HsSyn *after* the renamer,
to return a [Name] or [Id].  Before renaming the record punning
and wild-card mechanism makes it hard to know what is bound.
So these functions should not be applied to (HsSyn RdrName)

### Note: Unlifted id check in isUnliftedHsBind

The function isUnliftedHsBind is used to complain if we make a top-level
binding for a variable of unlifted type.

The top-level bindings for f,g are not unlifted (because of the Num a =>),
but the local, recursive, monomorphic bindings are:

Here the binding for 'fm' is illegal.  So generally we check the abe_mono types.

### Note: The abs_sig field of AbsBinds

### Note: Dictionary binders in ConPatOut

Do *not* gather (a) dictionary and (b) dictionary bindings as binders
of a ConPatOut pattern.  For most calls it doesn't matter, because
it's pre-typechecker and there are no ConPatOuts.  But it does matter
more in the desugarer; for example, DsUtils.mkSelectorBinds uses
collectPatBinders.  In a lazy pattern, for example f ~(C x y) = ...,
we want to generate bindings for x,y but not for dictionaries bound by
C.  (The type checker ensures they would not be used.)

Desugaring of arrow case expressions needs these bindings (see DsArrows
and arrowcase1), but SPJ (Jan 2007) says it's safer for it to use its
own pat-binder-collector:

Here's the problem.  Consider

data T a where
   C :: Num a => a -> Int -> T a

f ~(C (n+1) m) = (n,m)

Here, the pattern (C (n+1)) binds a hidden dictionary (d::Num a),
and *also* uses that dictionary to match the (n+1) pattern.  Yet, the
variables bound by the lazy pattern are n,m, *not* the dictionary d.
So in mkSelectorBinds in DsUtils, we want just m,n as the variables bound.




### Note: SrcSpan for binders

When extracting the (Located RdrNme) for a binder, at least for the
main name (the TyCon of a type declaration etc), we want to give it
the @SrcSpan@ of the whole /declaration/, not just the name itself
(which is how it appears in the syntax tree).  This SrcSpan (for the
entire declaration) is used as the SrcSpan for the Name that is
finally produced, and hence for error messages.  (See Trac #8607.)

### Note: Binders in family instances

In a type or data family instance declaration, the type
constructor is an *occurrence* not a binding site
    type instance T Int = Int -> Int   -- No binders
    data instance S Bool = S1 | S2     -- Binders are S1,S2

# Collecting binders the user did not write


The job of this family of functions is to run through binding sites and find the set of all Names
that were defined "implicitly", without being explicitly written by the user.

The main purpose is to find names introduced by record wildcards so that we can avoid
warning the user when they don't use those names (#4404)
