[[src]](https://github.com/ghc/ghc/tree/master/compiler/main/TidyPgm.hs)

(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

\section{Tidying up Core}



Constructing the TypeEnv, Instances, Rules, VectInfo from which the
ModIface is constructed, and which goes on to subsequent modules in
--make mode.

Most of the interface file is obtained simply by serialising the
TypeEnv.  One important consequence is that if the *interface file*
has pragma info if and only if the final TypeEnv does. This is not so
important for *this* module, but it's essential for ghc --make:
subsequent compilations must not see (e.g.) the arity if the interface
file does not contain arity If they do, they'll exploit the arity;
then the arity might change, but the iface file doesn't change =>
recompilation does not happen => disaster.

For data types, the final TypeEnv will have a TyThing for the TyCon,
plus one for each DataCon; the interface file will contain just one
data type declaration, but it is de-serialised back into a collection
of TyThings.

# Plan A: simpleTidyPgm


# Plan A: mkBootModDetails: omit pragmas, make interfaces small

* Ignore the bindings

* Drop all WiredIn things from the TypeEnv
        (we never want them in interface files)

* Retain all TyCons and Classes in the TypeEnv, to avoid
        having to find which ones are mentioned in the
        types of exported Ids

* Trim off the constructors of non-exported TyCons, both
        from the TyCon and from the TypeEnv

* Drop non-exported Ids from the TypeEnv

* Tidy the types of the DFunIds of Instances,
  make them into GlobalIds, (they already have External Names)
  and add them to the TypeEnv

* Tidy the types of the (exported) Ids in the TypeEnv,
  make them into GlobalIds (they already have External Names)

* Drop rules altogether

* Tidy the bindings, to ensure that the Caf and Arity
  information is correct for each top-level binder; the
  code generator needs it. And to ensure that local names have
  distinct OccNames in case of object-file splitting

* If this an hsig file, drop the instances altogether too (they'll
  get pulled in by the implicit module import.


# Plan B: tidy bindings, make TypeEnv full of IdInfo


# B: include pragmas, make interfaces

* Figure out which Ids are externally visible

* Tidy the bindings, externalising appropriate Ids

* Drop all Ids from the TypeEnv, and add all the External Ids from
  the bindings.  (This adds their IdInfo to the TypeEnv; and adds
  floated-out Ids that weren't even in the TypeEnv before.)

# 1: Figure out external Ids

### Note: choosing external names

See also the section "Interface stability" in the
RecompilationAvoidance commentary:
  http://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/RecompilationAvoidance

First we figure out which Ids are "external" Ids.  An
"external" Id is one that is visible from outside the compilation
unit.  These are
  a) the user exported ones
  b) the ones bound to static forms
  c) ones mentioned in the unfoldings, workers,
     rules of externally-visible ones ,
     or vectorised versions of externally-visible ones

While figuring out which Ids are external, we pick a "tidy" OccName
for each one.  That is, we make its OccName distinct from the other
external OccNames in this module, so that in interface files and
object code we can refer to it unambiguously by its OccName.  The
OccName for each binder is prefixed by the name of the exported Id
that references it; e.g. if "f" references "x" in its unfolding, then
"x" is renamed to "f_x".  This helps distinguish the different "x"s
from each other, and means that if "f" is later removed, things that
depend on the other "x"s will not need to be recompiled.  Of course,
if there are multiple "f_x"s, then we have to disambiguate somehow; we
use "f_x0", "f_x1" etc.

As far as possible we should assign names in a deterministic fashion.
Each time this module is compiled with the same options, we should end
up with the same set of external names with the same types.  That is,
the ABI hash in the interface should not change.  This turns out to be
quite tricky, since the order of the bindings going into the tidy
phase is already non-deterministic, as it is based on the ordering of
Uniques, which are assigned unpredictably.

To name things in a stable way, we do a depth-first-search of the
bindings, starting from the exports sorted by name.  This way, as long
as the bindings themselves are deterministic (they sometimes aren't!),
the order in which they are presented to the tidying phase does not
affect the names we assign.

# 2: Tidy the program

Next we traverse the bindings top to bottom.  For each *top-level*
binder

 1. Make it into a GlobalId; its IdDetails becomes VanillaGlobal,
    reflecting the fact that from now on we regard it as a global,
    not local, Id

 2. Give it a system-wide Unique.
    [Even non-exported things need system-wide Uniques because the
    byte-code generator builds a single Name->BCO symbol table.]

    We use the NameCache kept in the HscEnv as the
    source of such system-wide uniques.

    For external Ids, use the original-name cache in the NameCache
    to ensure that the unique assigned is the same as the Id had
    in any previous compilation run.

 3. Rename top-level Ids according to the names we chose in step 1.
    If it's an external Id, make it have a External Name, otherwise
    make it have an Internal Name.  This is used by the code generator
    to decide whether to make the label externally visible

 4. Give it its UTTERLY FINAL IdInfo; in ptic,
        * its unfolding, if it should have one

        * its arity, computed from the number of visible lambdas

        * its CAF info, computed from what is free in its RHS


Finally, substitute these new top-level binders consistently
throughout, including in unfoldings.  We also tidy binders in
RHSs, so that they print nicely in interfaces.


### Note: Don't attempt to trim data types

For some time GHC tried to avoid exporting the data constructors
of a data type if it wasn't strictly necessary to do so; see Trac #835.
But "strictly necessary" accumulated a longer and longer list
of exceptions, and finally I gave up the battle:

    commit 9a20e540754fc2af74c2e7392f2786a81d8d5f11
    Author: Simon Peyton Jones <simonpj@microsoft.com>
    Date:   Thu Dec 6 16:03:16 2012 +0000

    Stop attempting to "trim" data types in interface files

    Without -O, we previously tried to make interface files smaller
    by not including the data constructors of data types.  But
    there are a lot of exceptions, notably when Template Haskell is
    involved or, more recently, DataKinds.

    However Trac #7445 shows that even without TemplateHaskell, using
    the Data class and invoking Language.Haskell.TH.Quote.dataToExpQ
    is enough to require us to expose the data constructors.

    So I've given up on this "optimisation" -- it's probably not
    important anyway.  Now I'm simply not attempting to trim off
    the data constructors.  The gain in simplicity is worth the
    modest cost in interface file growth, which is limited to the
    bits reqd to describe those data constructors.

# Implicit bindings


### Note: Injecting implicit bindings

# \subsection{Step 1: finding externals}


### Note: Choosing external names

new name

show unfolding

u1

u2

# Deterministic free variables


We want a deterministic free-variable list.  exprFreeVars gives us
a VarSet, which is in a non-deterministic order when converted to a
list.  Hence, here we define a free-variable finder that returns
the free variables in the order that they are encountered.

### Note: Choosing external names

# findExternalRules


### Note: Finding external rules

The complete rules are gotten by combining
   a) local rules for imported Ids
   b) rules embedded in the top-level Ids

### Note: Which rules to expose

### Note: Which rules to expose

The function 'expose_rule' filters out rules that mention, on the LHS,
Ids that aren't externally visible; these rules can't fire in a client
module.

The externally-visible binders are computed (by chooseExternalIds)
assuming that all orphan rules are externalised (see init_ext_ids in
function 'search'). So in fact it's a bit conservative and we may
export more than we need.  (It's a sort of mutual recursion.)

### Note: Trimming auto-rules

Second, with auto-specialisation we may specialise local or imported
dfuns or INLINE functions, and then later inline them.  That may leave
behind something like
   RULE "foo" forall d. f @ Int d = f_spec
where f is either local or imported, and there is no remaining
reference to f_spec except from the RULE.

Now that RULE *might* be useful to an importing module, but that is
purely speculative, and meanwhile the code is taking up space and
codegen time.  I found that binary sizes jumped by 6-10% when I
started to specialise INLINE functions (again, Note [Inline
specialisations] in Specialise).

So it seems better to drop the binding for f_spec, and the rule
itself, if the auto-generated rule is the *only* reason that it is
being kept alive.

(The RULE still might have been useful in the past; that is, it was
the right thing to have generated it in the first place.  See Note
[Inline specialisations] in Specialise.  But now it has served its
purpose, and can be discarded.)

So findExternalRules does this:
  * Remove all bindings that are kept alive *only* by isAutoRule rules
      (this is done in trim_binds)
  * Remove all auto rules that mention bindings that have been removed
      (this is done by filtering by keep_rule)

NB: if a binding is kept alive for some *other* reason (e.g. f_spec is
called in the final code), we keep the rule too.

This stuff is the only reason for the ru_auto field in a Rule.


# tidyTopName


This is where we set names to local/global based on whether they really are
externally visible (see comment at the top of this module).  If the name
was previously local, we have to give it a unique occurrence name if
we intend to externalise it.


# \subsection{Step 2: top-level tidying}


# Figuring out CafInfo for an expression


hasCafRefs decides whether a top-level closure can point into the dynamic heap.
We mark such things as `MayHaveCafRefs' because this information is
used to decide whether a particular closure needs to be referenced
in an SRT or not.

There are two reasons for setting MayHaveCafRefs:
        a) The RHS is a CAF: a top-level updatable thunk.
        b) The RHS refers to something that MayHaveCafRefs

Possible improvement: In an effort to keep the number of CAFs (and
hence the size of the SRTs) down, we could also look at the expression and
decide whether it requires a small bounded amount of heap, so we can ignore
it as a CAF.  In these cases however, we would need to use an additional
CAF list to keep track of non-collectable CAFs.

### Note: Disgusting computation of CafRefs

We compute hasCafRefs here, because IdInfo is supposed to be finalised
after TidyPgm.  But CorePrep does some transformations that affect CAF-hood.
So we have to *predict* the result here, which is revolting.

In particular CorePrep expands Integer literals.  So in the prediction code
here we resort to applying the same expansion (cvt_integer). Ugh!


# Old, dead, type-trimming code


We used to try to "trim off" the constructors of data types that are
not exported, to reduce the size of interface files, at least without
-O.  But that is not always possible: see the old Note [When we can't
trim types] below for exceptions.