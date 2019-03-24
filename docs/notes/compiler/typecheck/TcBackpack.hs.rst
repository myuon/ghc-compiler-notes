`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcBackpack.hs>`_

====================
compiler/typecheck/TcBackpack.hs.rst
====================

Note [Error reporting bad reexport]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
NB: You want to be a bit careful about what location you report on reexports.
If the name was declared in the hsig file, 'nameSrcSpan name' is indeed the
correct source location.  However, if it was *reexported*, obviously the name
is not going to have the right location.  In this case, we need to grovel in
tcg_rn_exports to figure out where the reexport came from.


Note [Blank hsigs for all requirements]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
One invariant that a client of GHC must uphold is that there
must be an hsig file for every requirement (according to
@-this-unit-id@); this ensures that for every interface
file (hi), there is a source file (hsig), which helps grease
the wheels of recompilation avoidance which assumes that
source files always exist.
inheritedSigPvpWarning :: WarningTxt
inheritedSigPvpWarning =
    WarningTxt (noLoc NoSourceText) [noLoc (StringLiteral NoSourceText (fsLit msg))]
  where
    msg = "Inherited requirements from non-signature libraries (libraries " ++
          "with modules) should not be used, as this mode of use is not " ++
          "compatible with PVP-style version bounds.  Instead, copy the " ++
          "declaration to the local hsig file or move the signature to a " ++
          "library of its own and add that library as a dependency."


Note [Handling never-exported TyThings under Backpack]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  DEFINITION: A "never-exported TyThing" is a TyThing whose 'Name' will
  never be mentioned in the export list of a module (mi_avails).
  Unlike implicit TyThings (Note [Implicit TyThings]), non-exported
  TyThings DO have a standalone IfaceDecl declaration in their
  interface file.

Originally, Backpack was designed under the assumption that anything
you could declare in a module could also be exported; thus, merging
the export lists of two signatures is just merging the declarations
of two signatures writ small.  Of course, in GHC Haskell, there are a
few important things which are not explicitly exported but still can
be used:  in particular, dictionary functions for instances, Typeable
TyCon bindings, and coercion axioms for type families also count.

When handling these non-exported things, there two primary things
we need to watch out for:

 * Signature matching/merging is done by comparing each
   of the exported entities of a signature and a module.  These exported
   entities may refer to non-exported TyThings which must be tested for
   consistency.  For example, an instance (ClsInst) will refer to a
   non-exported DFunId.  In this case, 'checkBootDeclM' directly compares the
   embedded 'DFunId' in 'is_dfun'.

.. code-block:: haskell

   For this to work at all, we must ensure that pointers in 'is_dfun' refer
   to DISTINCT 'DFunId's, even though the 'Name's (may) be the same.
   Unfortunately, this is the OPPOSITE of how we treat most other references
   to 'Name's, so this case needs to be handled specially.

.. code-block:: haskell

   The details are in the documentation for 'typecheckIfacesForMerging'.
   and the Note [Resolving never-exported Names in TcIface].

 * When we rename modules and signatures, we use the export lists to
   decide how the declarations should be renamed.  However, this
   means we don't get any guidance for how to rename non-exported
   entities.  Fortunately, we only need to rename these entities
   *consistently*, so that 'typecheckIfacesForMerging' can wire them
   up as needed.

.. code-block:: haskell

   The details are in Note [rnIfaceNeverExported] in 'RnModIface'.

The root cause for all of these complications is the fact that these
logically "implicit" entities are defined indirectly in an interface
file.  #13151 gives a proposal to make these *truly* implicit.

