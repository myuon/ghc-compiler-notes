`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/iface/IfaceSyn.hs>`_

compiler/iface/IfaceSyn.hs
==========================


Note [Versioning of instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/iface/IfaceSyn.hs#L378>`__

See [https://gitlab.haskell.org/ghc/ghc/wikis/commentary/compiler/recompilation-avoidance#instances]



Note [Empty case alternatives]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/iface/IfaceSyn.hs#L525>`__

In IfaceSyn an IfaceCase does not record the types of the alternatives,
unlike CorSyn Case.  But we need this type if the alternatives are empty.
Hence IfaceECase.  See Note [Empty case alternatives] in CoreSyn.



Note [Expose recursive functions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/iface/IfaceSyn.hs#L531>`__

For supercompilation we want to put *all* unfoldings in the interface
file, even for functions that are recursive (or big).  So we need to
know when an unfolding belongs to a loop-breaker so that we can refrain
from inlining it (except during supercompilation).



Note [IdInfo on nested let-bindings]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/iface/IfaceSyn.hs#L538>`__

Occasionally we want to preserve IdInfo on nested let bindings. The one
that came up was a NOINLINE pragma on a let-binding inside an INLINE
function.  The user (Duncan Coutts) really wanted the NOINLINE control
to cross the separate compilation boundary.

In general we retain all info that is left by CoreTidy.tidyLetBndr, since
that is what is seen by importing module with --make



Note [Printing IfaceDecl binders]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/iface/IfaceSyn.hs#L637>`__

The binders in an IfaceDecl are just OccNames, so we don't know what module they
come from.  But when we pretty-print a TyThing by converting to an IfaceDecl
(see PprTyThing), the TyThing may come from some other module so we really need
the module qualifier.  We solve this by passing in a pretty-printer for the
binders.

When printing an interface file (--show-iface), we want to print
everything unqualified, so we can just print the OccName directly.



Note [Result type of a data family GADT]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/iface/IfaceSyn.hs#L1161>`__

Consider
   data family T a
   data instance T (p,q) where
      T1 :: T (Int, Maybe c)
      T2 :: T (Bool, q)

The IfaceDecl actually looks like

::

   data TPr p q where
      T1 :: forall p q. forall c. (p~Int,q~Maybe c) => TPr p q
      T2 :: forall p q. (p~Bool) => TPr p q

..

To reconstruct the result types for T1 and T2 that we
want to pretty print, we substitute the eq-spec
[p->Int, q->Maybe c] in the arg pattern (p,q) to give
   T (Int, Maybe c)
Remember that in IfaceSyn, the TyCon and DataCon share the same
universal type variables.

----------------------------- Printing IfaceExpr ------------------------------------



Note [Tracking data constructors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/iface/IfaceSyn.hs#L1639>`__

In a case expression
   case e of { C a -> ...; ... }
You might think that we don't need to include the datacon C
in the free names, because its type will probably show up in
the free names of 'e'.  But in rare circumstances this may
not happen.   Here's the one that bit me:

::

   module DynFlags where
     import {-# SOURCE #-} Packages( PackageState )
     data DynFlags = DF ... PackageState ...

..

   module Packages where
     import DynFlags
     data PackageState = PS ...
     lookupModule (df :: DynFlags)
        = case df of
              DF ...p... -> case p of
                               PS ... -> ...

Now, lookupModule depends on DynFlags, but the transitive dependency
on the *locally-defined* type PackageState is not visible. We need
to take account of the use of the data constructor PS in the pattern match.



Note [Lazy deserialization of IfaceId]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/iface/IfaceSyn.hs#L1849>`__

The use of lazyPut and lazyGet in the IfaceId Binary instance is
purely for performance reasons, to avoid deserializing details about
identifiers that will never be used. It's not involved in tying the
knot in the type checker. It saved ~1% of the total build time of GHC.

When we read an interface file, we extend the PTE, a mapping of Names
to TyThings, with the declarations we have read. The extension of the
PTE is strict in the Names, but not in the TyThings themselves.
LoadIface.loadDecl calculates the list of (Name, TyThing) bindings to
add to the PTE. For an IfaceId, there's just one binding to add; and
the ty, details, and idinfo fields of an IfaceId are used only in the
TyThing. So by reading those fields lazily we may be able to save the
work of ever having to deserialize them (into IfaceType, etc.).

For IfaceData and IfaceClass, loadDecl creates extra implicit bindings
(the constructors and field selectors of the data declaration, or the
methods of the class), whose Names depend on more than just the Name
of the type constructor or class itself. So deserializing them lazily
would be more involved. Similar comments apply to the other
constructors of IfaceDecl with the additional point that they probably
represent a small proportion of all declarations.

