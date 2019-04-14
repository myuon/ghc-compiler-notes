`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/basicTypes/RdrName.hs>`_

compiler/basicTypes/RdrName.hs
==============================


Note [Local bindings with Exact Names]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/basicTypes/RdrName.hs#L415>`__

With Template Haskell we can make local bindings that have Exact Names.
Computing shadowing etc may use elemLocalRdrEnv (at least it certainly
does so in RnTpes.bindHsQTyVars), so for an Exact Name we must consult
the in-scope-name-set.



Note [GlobalRdrElt provenance]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/basicTypes/RdrName.hs#L500>`__

The gre_lcl and gre_imp fields of a GlobalRdrElt describe its "provenance",
i.e. how the Name came to be in scope.  It can be in scope two ways:
  - gre_lcl = True: it is bound in this module
  - gre_imp: a list of all the imports that brought it into scope

It's an INVARIANT that you have one or the other; that is, either
gre_lcl is True, or gre_imp is non-empty.

It is just possible to have *both* if there is a module loop: a Name
is defined locally in A, and also brought into scope by importing a
module that SOURCE-imported A.  Exapmle (#7672):

::

 A.hs-boot   module A where
               data T

..

::

 B.hs        module B(Decl.T) where
               import {-# SOURCE #-} qualified A as Decl

..

 A.hs        module A where
               import qualified B
               data T = Z | S B.T

In A.hs, 'T' is locally bound, *and* imported as B.T.



Note [Parents]
~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/basicTypes/RdrName.hs#L526>`__

  Parent           Children
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  data T           Data constructors
                   Record-field ids

::

  data family T    Data constructors and record-field ids
                   of all visible data instances of T

..

  class C          Class operations
                   Associated type constructors

~~~~~~~~~~~~~~~~~~~~~~~~~
 Constructor      Meaning
 ~~~~~~~~~~~~~~~~~~~~~~~~
  NoParent        Can not be bundled with a type constructor.
  ParentIs n      Can be bundled with the type constructor corresponding to
                  n.
  FldParent       See Note [Parents for record fields]



Note [Parents for record fields]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/basicTypes/RdrName.hs#L550>`__

For record fields, in addition to the Name of the type constructor
(stored in par_is), we use FldParent to store the field label.  This
extra information is used for identifying overloaded record fields
during renaming.

In a definition arising from a normal module (without
-XDuplicateRecordFields), par_lbl will be Nothing, meaning that the
field's label is the same as the OccName of the selector's Name.  The
GlobalRdrEnv will contain an entry like this:

::

    "x" |->  GRE x (FldParent T Nothing) LocalDef

..

When -XDuplicateRecordFields is enabled for the module that contains
T, the selector's Name will be mangled (see comments in FieldLabel).
Thus we store the actual field label in par_lbl, and the GlobalRdrEnv
entry looks like this:

::

    "x" |->  GRE $sel:x:MkT (FldParent T (Just "x")) LocalDef

..

Note that the OccName used when adding a GRE to the environment
(greOccName) now depends on the parent field: for FldParent it is the
field label, if present, rather than the selector name.

~~

Record pattern synonym selectors are treated differently. Their parent
information is `NoParent` in the module in which they are defined. This is because
a pattern synonym `P` has no parent constructor either.

However, if `f` is bundled with a type constructor `T` then whenever `f` is
imported the parent will use the `Parent` constructor so the parent of `f` is
now `T`.



Note [Combining parents]
~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/basicTypes/RdrName.hs#L586>`__

With an associated type we might have
   module M where
     class C a where
       data T a
       op :: T a -> a
     instance C Int where
       data T Int = TInt
     instance C Bool where
       data T Bool = TBool

Then:   C is the parent of T
        T is the parent of TInt and TBool
So: in an export list
    C(..) is short for C( op, T )
    T(..) is short for T( TInt, TBool )

Module M exports everything, so its exports will be
   AvailTC C [C,T,op]
   AvailTC T [T,TInt,TBool]
On import we convert to GlobalRdrElt and then combine
those.  For T that will mean we have
  one GRE with Parent C
  one GRE with NoParent
That's why plusParent picks the "best" case.



Note [GRE filtering]
~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/basicTypes/RdrName.hs#L857>`__

(pickGREs rdr gres) takes a list of GREs which have the same OccName
as 'rdr', say "x".  It does two things:

(a) filters the GREs to a subset that are in scope
    * Qualified,   as 'M.x'  if want_qual    is Qual M _
    * Unqualified, as 'x'    if want_unqual  is Unqual _

(b) for that subset, filter the provenance field (gre_lcl and gre_imp)
    to ones that brought it into scope qualified or unqualified resp.

Example:
      module A ( f ) where
      import qualified Foo( f )
      import Baz( f )
      f = undefined

Let's suppose that Foo.f and Baz.f are the same entity really, but the local
'f' is different, so there will be two GREs matching "f":
   gre1:  gre_lcl = True,  gre_imp = []
   gre2:  gre_lcl = False, gre_imp = [ imported from Foo, imported from Bar ]

The use of "f" in the export list is ambiguous because it's in scope
from the local def and the import Baz(f); but *not* the import qualified Foo.
pickGREs returns two GRE
   gre1:   gre_lcl = True,  gre_imp = []
   gre2:   gre_lcl = False, gre_imp = [ imported from Bar ]

Now the "ambiguous occurrence" message can correctly report how the
ambiguity arises.



Note [GlobalRdrEnv shadowing]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/basicTypes/RdrName.hs#L996>`__

Before adding new names to the GlobalRdrEnv we nuke some existing entries;
this is "shadowing".  The actual work is done by RdrEnv.shadowName.
Suppose
   env' = shadowName env M.f

Then:
   * Looking up (Unqual f) in env' should succeed, returning M.f,
     even if env contains existing unqualified bindings for f.
     They are shadowed

   * Looking up (Qual M.f) in env' should succeed, returning M.f

   * Looking up (Qual X.f) in env', where X /= M, should be the same as
     looking up (Qual X.f) in env.
     That is, shadowName does /not/ delete earlier qualified bindings

There are two reasons for shadowing:

* The GHCi REPL

  - Ids bought into scope on the command line (eg let x = True) have
    External Names, like Ghci4.x.  We want a new binding for 'x' (say)
    to override the existing binding for 'x'.  Example:

           ghci> :load M    -- Brings `x` and `M.x` into scope
           ghci> x
           ghci> "Hello"
           ghci> M.x
           ghci> "hello"
           ghci> let x = True  -- Shadows `x`
           ghci> x             -- The locally bound `x`
                               -- NOT an ambiguous reference
           ghci> True
           ghci> M.x           -- M.x is still in scope!
           ghci> "Hello"
    So when we add `x = True` we must not delete the `M.x` from the
    `GlobalRdrEnv`; rather we just want to make it "qualified only";
    hence the `mk_fake-imp_spec` in `shadowName`.  See also Note
    [Interactively-bound Ids in GHCi] in HscTypes

  - Data types also have External Names, like Ghci4.T; but we still want
    'T' to mean the newly-declared 'T', not an old one.

* Nested Template Haskell declaration brackets
  See Note [Top-level Names in Template Haskell decl quotes] in RnNames

  Consider a TH decl quote:
      module M where
        f x = h [d| f = ...f...M.f... |]
  We must shadow the outer unqualified binding of 'f', else we'll get
  a complaint when extending the GlobalRdrEnv, saying that there are
  two bindings for 'f'.  There are several tricky points:

    - This shadowing applies even if the binding for 'f' is in a
      where-clause, and hence is in the *local* RdrEnv not the *global*
      RdrEnv.  This is done in lcl_env_TH in extendGlobalRdrEnvRn.

    - The External Name M.f from the enclosing module must certainly
      still be available.  So we don't nuke it entirely; we just make
      it seem like qualified import.

    - We only shadow *External* names (which come from the main module),
      or from earlier GHCi commands. Do not shadow *Internal* names
      because in the bracket
          [d| class C a where f :: a
              f = 4 |]
      rnSrcDecls will first call extendGlobalRdrEnvRn with C[f] from the
      class decl, and *separately* extend the envt with the value binding.
      At that stage, the class op 'f' will have an Internal name.



Note [Choosing the best import declaration]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/basicTypes/RdrName.hs#L1216>`__

When reporting unused import declarations we use the following rules.
   (see [wiki:commentary/compiler/unused-imports])

Say that an import-item is either
  * an entire import-all decl (eg import Foo), or
  * a particular item in an import list (eg import Foo( ..., x, ...)).
The general idea is that for each /occurrence/ of an imported name, we will
attribute that use to one import-item. Once we have processed all the
occurrences, any import items with no uses attributed to them are unused,
and are warned about. More precisely:

1. For every RdrName in the program text, find its GlobalRdrElt.

2. Then, from the [ImportSpec] (gre_imp) of that GRE, choose one
   the "chosen import-item", and mark it "used". This is done
   by 'bestImport'

3. After processing all the RdrNames, bleat about any
   import-items that are unused.
   This is done in RnNames.warnUnusedImportDecls.

The function 'bestImport' returns the dominant import among the
ImportSpecs it is given, implementing Step 2.  We say import-item A
dominates import-item B if we choose A over B. In general, we try to
choose the import that is most likely to render other imports
unnecessary.  Here is the dominance relationship we choose:

::

    a) import Foo dominates import qualified Foo.

..

::

    b) import Foo dominates import Foo(x).

..

::

    c) Otherwise choose the textually first one.

..

Rationale for (a).  Consider
   import qualified M  -- Import #1
   import M( x )       -- Import #2
   foo = M.x + x

The unqualified 'x' can only come from import #2.  The qualified 'M.x'
could come from either, but bestImport picks import #2, because it is
more likely to be useful in other imports, as indeed it is in this
case (see #5211 for a concrete example).

But the rules are not perfect; consider
   import qualified M  -- Import #1
   import M( x )       -- Import #2
   foo = M.x + M.y

The M.x will use import #2, but M.y can only use import #1.

