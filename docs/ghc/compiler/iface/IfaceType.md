[[src]](https://github.com/ghc/ghc/tree/master/compiler/iface/IfaceType.hs)

(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1993-1998


This module defines interface types and binders


# Local (nested) binders


# 

### Note: Free tyvars in IfaceType

Nowadays (since Nov 16, 2016) we pretty-print a Type by converting to
an IfaceType and pretty printing that.  This eliminates a lot of
pretty-print duplication, and it matches what we do with
pretty-printing TyThings.

It works fine for closed types, but when printing debug traces (e.g.
when using -ddump-tc-trace) we print a lot of /open/ types.  These
types are full of TcTyVars, and it's absolutely crucial to print them
in their full glory, with their unique, TcTyVarDetails etc.

So we simply embed a TyVar in IfaceType with the IfaceFreeTyVar constructor.
Note that:

* We never expect to serialise an IfaceFreeTyVar into an interface file, nor
  to deserialise one.  IfaceFreeTyVar is used only in the "convert to IfaceType
  and then pretty-print" pipeline.

We do the same for covars, naturally.

### Note: Equality predicates in IfaceType

### Note: The equality types story

 Predicate                         Pretty-printed as
                          Homogeneous case        Heterogeneous case
 ----------------        -----------------        -------------------
 (~)    eqTyCon                 ~                  N/A
 (~~)   heqTyCon                ~                  ~~
 (~#)   eqPrimTyCon             ~#                 ~~
 (~R#)  eqReprPrimTyCon         Coercible          Coercible

By "homogeneeous case" we mean cases where a hetero-kinded equality
(all but the first above) is actually applied to two identical kinds.
Unfortunately, determining this from an IfaceType isn't possible since
we can't see through type synonyms. Consequently, we need to record
whether this particular application is homogeneous in IfaceTyConSort
for the purposes of pretty-printing.

All this suppresses information. To get the ground truth, use -dppr-debug
(see 'print_eqs' in 'ppr_equality').

### Note: The equality types story

### Note: Holes in IfaceCoercion

When typechecking fails the typechecker will produce a HoleCo to stand
in place of the unproven assertion. While we generally don't want to
let these unproven assertions leak into interface files, we still need
to be able to pretty-print them as we use IfaceType's pretty-printer
to render Types. For this reason IfaceCoercion has a IfaceHoleCo
constructor; however, we fails when asked to serialize to a
IfaceHoleCo to ensure that they don't end up in an interface file.

# 

### Note: Substitution on IfaceType

Substitutions on IfaceType are done only during pretty-printing to
construct the result type of a GADT, and does not deal with binders
(eg IfaceForAll), so it doesn't need fancy capture stuff.  

# Functions over IFaceTcArgs


### Note: Suppressing invisible arguments

We use the IfaceTcArgs to specify which of the arguments to a type
constructor should be displayed when pretty-printing, under
the control of -fprint-explicit-kinds.
See also Type.filterOutInvisibleTypes.
For example, given
    T :: forall k. (k->*) -> k -> *    -- Ordinary kind polymorphism
    'Just :: forall k. k -> 'Maybe k   -- Promoted
we want
  T * Tree Int    prints as    T Tree Int
  'Just *         prints as    Just *

# Pretty-printing


### Note: Defaulting RuntimeRep variables


RuntimeRep variables are considered by many (most?) users to be little more than
syntactic noise. When the notion was introduced there was a signficant and
understandable push-back from those with pedagogy in mind, which argued that
RuntimeRep variables would throw a wrench into nearly any teach approach since
they appear in even the lowly ($) function's type,

    ($) :: forall (w :: RuntimeRep) a (b :: TYPE w). (a -> b) -> a -> b

which is significantly less readable than its non RuntimeRep-polymorphic type of

    ($) :: (a -> b) -> a -> b

Moreover, unboxed types don't appear all that often in run-of-the-mill Haskell
programs, so it makes little sense to make all users pay this syntactic
overhead.

For this reason it was decided that we would hide RuntimeRep variables for now
(see #11549). We do this by defaulting all type variables of kind RuntimeRep to
PtrLiftedRep. This is done in a pass right before pretty-printing
(defaultRuntimeRepVars, controlled by -fprint-explicit-runtime-reps)
