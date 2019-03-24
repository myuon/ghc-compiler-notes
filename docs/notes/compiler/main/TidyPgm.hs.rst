`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/main/TidyPgm.hs>`_

====================
compiler/main/TidyPgm.hs.rst
====================

Note [Choosing external Ids]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
See also the section "Interface stability" in the
RecompilationAvoidance commentary:
  http://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/RecompilationAvoidance

First we figure out which Ids are "external" Ids.  An
"external" Id is one that is visible from outside the compilation
unit.  These are
  a) the user exported ones
  b) the ones bound to static forms
  c) ones mentioned in the unfoldings, workers, or
     rules of externally-visible ones

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



Note [Tidy the top-level bindings]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Next we traverse the bindings top to bottom.  For each *top-level*
binder

 1. Make it into a GlobalId; its IdDetails becomes VanillaGlobal,
    reflecting the fact that from now on we regard it as a global,
    not local, Id

 2. Give it a system-wide Unique.
    [Even non-exported things need system-wide Uniques because the
    byte-code generator builds a single Name->BCO symbol table.]

.. code-block:: haskell

    We use the NameCache kept in the HscEnv as the
    source of such system-wide uniques.

.. code-block:: haskell

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


Note [Don't attempt to trim data types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For some time GHC tried to avoid exporting the data constructors
of a data type if it wasn't strictly necessary to do so; see #835.
But "strictly necessary" accumulated a longer and longer list
of exceptions, and finally I gave up the battle:

.. code-block:: haskell

    commit 9a20e540754fc2af74c2e7392f2786a81d8d5f11
    Author: Simon Peyton Jones <simonpj@microsoft.com>
    Date:   Thu Dec 6 16:03:16 2012 +0000

.. code-block:: haskell

    Stop attempting to "trim" data types in interface files

.. code-block:: haskell

    Without -O, we previously tried to make interface files smaller
    by not including the data constructors of data types.  But
    there are a lot of exceptions, notably when Template Haskell is
    involved or, more recently, DataKinds.

.. code-block:: haskell

    However #7445 shows that even without TemplateHaskell, using
    the Data class and invoking Language.Haskell.TH.Quote.dataToExpQ
    is enough to require us to expose the data constructors.

.. code-block:: haskell

    So I've given up on this "optimisation" -- it's probably not
    important anyway.  Now I'm simply not attempting to trim off
    the data constructors.  The gain in simplicity is worth the
    modest cost in interface file growth, which is limited to the
    bits reqd to describe those data constructors.



Note [Injecting implicit bindings]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We inject the implicit bindings right at the end, in CoreTidy.
Some of these bindings, notably record selectors, are not
constructed in an optimised form.  E.g. record selector for
        data T = MkT { x :: {-# UNPACK #-} !Int }
Then the unfolding looks like
        x = \t. case t of MkT x1 -> let x = I# x1 in x
This generates bad code unless it's first simplified a bit.  That is
why CoreUnfold.mkImplicitUnfolding uses simpleOptExpr to do a bit of
optimisation first.  (Only matters when the selector is used curried;
eg map x ys.)  See #2070.

[Oct 09: in fact, record selectors are no longer implicit Ids at all,
because we really do want to optimise them properly. They are treated
much like any other Id.  But doing "light" optimisation on an implicit
Id still makes sense.]

At one time I tried injecting the implicit bindings *early*, at the
beginning of SimplCore.  But that gave rise to real difficulty,
because GlobalIds are supposed to have *fixed* IdInfo, but the
simplifier and other core-to-core passes mess with IdInfo all the
time.  The straw that broke the camels back was when a class selector
got the wrong arity -- ie the simplifier gave it arity 2, whereas
importing modules were expecting it to have arity 1 (#2844).
It's much safer just to inject them right at the end, after tidying.

Oh: two other reasons for injecting them late:

  - If implicit Ids are already in the bindings when we start TidyPgm,
    we'd have to be careful not to treat them as external Ids (in
    the sense of chooseExternalIds); else the Ids mentioned in *their*
    RHSs will be treated as external and you get an interface file
    saying      a18 = <blah>
    but nothing referring to a18 (because the implicit Id is the
    one that does, and implicit Ids don't appear in interface files).

  - More seriously, the tidied type-envt will include the implicit
    Id replete with a18 in its unfolding; but we won't take account
    of a18 when computing a fingerprint for the class; result chaos.

There is one sort of implicit binding that is injected still later,
namely those for data constructor workers. Reason (I think): it's
really just a code generation trick.... binding itself makes no sense.
See Note [Data constructor workers] in CorePrep.


Note [Finding external rules]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The complete rules are gotten by combining
   a) local rules for imported Ids
   b) rules embedded in the top-level Ids

There are two complications:
  * Note [Which rules to expose]
  * Note [Trimming auto-rules]



Note [Which rules to expose]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The function 'expose_rule' filters out rules that mention, on the LHS,
Ids that aren't externally visible; these rules can't fire in a client
module.

The externally-visible binders are computed (by chooseExternalIds)
assuming that all orphan rules are externalised (see init_ext_ids in
function 'search'). So in fact it's a bit conservative and we may
export more than we need.  (It's a sort of mutual recursion.)



Note [Trimming auto-rules]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
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


Note [Disgusting computation of CafRefs]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We compute hasCafRefs here, because IdInfo is supposed to be finalised
after TidyPgm.  But CorePrep does some transformations that affect CAF-hood.
So we have to *predict* the result here, which is revolting.

In particular CorePrep expands Integer and Natural literals. So in the
prediction code here we resort to applying the same expansion (cvt_literal).
Ugh!


Note [When we can't trim types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The basic idea of type trimming is to export algebraic data types
abstractly (without their data constructors) when compiling without
-O, unless of course they are explicitly exported by the user.

We always export synonyms, because they can be mentioned in the type
of an exported Id.  We could do a full dependency analysis starting
from the explicit exports, but that's quite painful, and not done for
now.

But there are some times we can't do that, indicated by the 'no_trim_types' flag.

First, Template Haskell.  Consider (#2386) this
        module M(T, makeOne) where
          data T = Yay String
          makeOne = [| Yay "Yep" |]
Notice that T is exported abstractly, but makeOne effectively exports it too!
A module that splices in $(makeOne) will then look for a declaration of Yay,
so it'd better be there.  Hence, brutally but simply, we switch off type
constructor trimming if TH is enabled in this module.

Second, data kinds.  Consider (#5912)
     {-# LANGUAGE DataKinds #-}
     module M() where
     data UnaryTypeC a = UnaryDataC a
     type Bug = 'UnaryDataC
We always export synonyms, so Bug is exposed, and that means that
UnaryTypeC must be too, even though it's not explicitly exported.  In
effect, DataKinds means that we'd need to do a full dependency analysis
to see what data constructors are mentioned.  But we don't do that yet.

In these two cases we just switch off type trimming altogether.

mustExposeTyCon :: Bool         -- Type-trimming flag
                -> NameSet      -- Exports
                -> TyCon        -- The tycon
                -> Bool         -- Can its rep be hidden?
-- We are compiling without -O, and thus trying to write as little as
-- possible into the interface file.  But we must expose the details of
-- any data types whose constructors or fields are exported
mustExposeTyCon no_trim_types exports tc
  | no_trim_types               -- See Note [When we can't trim types]
  = True

.. code-block:: haskell

  | not (isAlgTyCon tc)         -- Always expose synonyms (otherwise we'd have to
                                -- figure out whether it was mentioned in the type
                                -- of any other exported thing)
  = True

.. code-block:: haskell

  | isEnumerationTyCon tc       -- For an enumeration, exposing the constructors
  = True                        -- won't lead to the need for further exposure

.. code-block:: haskell

  | isFamilyTyCon tc            -- Open type family
  = True

.. code-block:: haskell

  -- Below here we just have data/newtype decls or family instances

.. code-block:: haskell

  | null data_cons              -- Ditto if there are no data constructors
  = True                        -- (NB: empty data types do not count as enumerations
                                -- see Note [Enumeration types] in TyCon

.. code-block:: haskell

  | any exported_con data_cons  -- Expose rep if any datacon or field is exported
  = True

.. code-block:: haskell

  | isNewTyCon tc && isFFITy (snd (newTyConRhs tc))
  = True   -- Expose the rep for newtypes if the rep is an FFI type.
           -- For a very annoying reason.  'Foreign import' is meant to
           -- be able to look through newtypes transparently, but it
           -- can only do that if it can "see" the newtype representation

.. code-block:: haskell

  | otherwise
  = False
  where
    data_cons = tyConDataCons tc
    exported_con con = any (`elemNameSet` exports)
                           (dataConName con : dataConFieldLabels con)

