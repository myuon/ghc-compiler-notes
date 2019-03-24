`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/main/HscTypes.hs>`_

====================
compiler/main/HscTypes.hs.rst
====================

Note [hsc_type_env_var hack]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
hsc_type_env_var is used to initialize tcg_type_env_var, and
eventually it is the mutable variable that is queried from
if_rec_types to get a TypeEnv.  So, clearly, it's something
related to knot-tying (see Note [Tying the knot]).
hsc_type_env_var is used in two places: initTcRn (where
it initializes tcg_type_env_var) and initIfaceCheck
(where it initializes if_rec_types).

But why do we need a way to feed a mutable variable in?  Why
can't we just initialize tcg_type_env_var when we start
typechecking?  The problem is we need to knot-tie the
EPS, and we may start adding things to the EPS before type
checking starts.

Here is a concrete example. Suppose we are running
"ghc -c A.hs", and we have this file system state:

.. code-block:: haskell

 A.hs-boot   A.hi-boot **up to date**
 B.hs        B.hi      **up to date**
 A.hs        A.hi      **stale**

The first thing we do is run checkOldIface on A.hi.
checkOldIface will call loadInterface on B.hi so it can
get its hands on the fingerprints, to find out if A.hi
needs recompilation.  But loadInterface also populates
the EPS!  And so if compilation turns out to be necessary,
as it is in this case, the thunks we put into the EPS for
B.hi need to have the correct if_rec_types mutable variable
to query.

If the mutable variable is only allocated WHEN we start
typechecking, then that's too late: we can't get the
information to the thunks.  So we need to pre-commit
to a type variable in 'hscIncrementalCompile' BEFORE we
check the old interface.

This is all a massive hack because arguably checkOldIface
should not populate the EPS. But that's a refactor for
another day.


Note [The interactive package]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Type, class, and value declarations at the command prompt are treated
as if they were defined in modules
   interactive:Ghci1
   interactive:Ghci2
   ...etc...
with each bunch of declarations using a new module, all sharing a
common package 'interactive' (see Module.interactiveUnitId, and
PrelNames.mkInteractiveModule).

This scheme deals well with shadowing.  For example:

.. code-block:: haskell

   ghci> data T = A
   ghci> data T = B
   ghci> :i A
   data Ghci1.T = A  -- Defined at <interactive>:2:10

Here we must display info about constructor A, but its type T has been
shadowed by the second declaration.  But it has a respectable
qualified name (Ghci1.T), and its source location says where it was
defined.

So the main invariant continues to hold, that in any session an
original name M.T only refers to one unique thing.  (In a previous
iteration both the T's above were called :Interactive.T, albeit with
different uniques, which gave rise to all sorts of trouble.)

The details are a bit tricky though:

 * The field ic_mod_index counts which Ghci module we've got up to.
   It is incremented when extending ic_tythings

 * ic_tythings contains only things from the 'interactive' package.

 * Module from the 'interactive' package (Ghci1, Ghci2 etc) never go
   in the Home Package Table (HPT).  When you say :load, that's when we
   extend the HPT.

 * The 'thisPackage' field of DynFlags is *not* set to 'interactive'.
   It stays as 'main' (or whatever -this-unit-id says), and is the
   package to which :load'ed modules are added to.

 * So how do we arrange that declarations at the command prompt get to
   be in the 'interactive' package?  Simply by setting the tcg_mod
   field of the TcGblEnv to "interactive:Ghci1".  This is done by the
   call to initTc in initTcInteractive, which in turn get the module
   from it 'icInteractiveModule' field of the interactive context.

.. code-block:: haskell

   The 'thisPackage' field stays as 'main' (or whatever -this-unit-id says.

 * The main trickiness is that the type environment (tcg_type_env) and
   fixity envt (tcg_fix_env), now contain entities from all the
   interactive-package modules (Ghci1, Ghci2, ...) together, rather
   than just a single module as is usually the case.  So you can't use
   "nameIsLocalOrFrom" to decide whether to look in the TcGblEnv vs
   the HPT/PTE.  This is a change, but not a problem provided you
   know.

* However, the tcg_binds, tcg_sigs, tcg_insts, tcg_fam_insts, etc fields
  of the TcGblEnv, which collect "things defined in this module", all
  refer to stuff define in a single GHCi command, *not* all the commands
  so far.

.. code-block:: haskell

  In contrast, tcg_inst_env, tcg_fam_inst_env, have instances from
  all GhciN modules, which makes sense -- they are all "home package"
  modules.




Note [Interactively-bound Ids in GHCi]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The Ids bound by previous Stmts in GHCi are currently
        a) GlobalIds, with
        b) An External Name, like Ghci4.foo
           See Note [The interactive package] above
        c) A tidied type

.. code-block:: haskell

 (a) They must be GlobalIds (not LocalIds) otherwise when we come to
     compile an expression using these ids later, the byte code
     generator will consider the occurrences to be free rather than
     global.

.. code-block:: haskell

 (b) Having an External Name is important because of Note
     [GlobalRdrEnv shadowing] in RdrName

.. code-block:: haskell

 (c) Their types are tidied. This is important, because :info may ask
     to look at them, and :info expects the things it looks up to have
     tidy types

Where do interactively-bound Ids come from?

  - GHCi REPL Stmts   e.g.
         ghci> let foo x = x+1
    These start with an Internal Name because a Stmt is a local
    construct, so the renamer naturally builds an Internal name for
    each of its binders.  Then in tcRnStmt they are externalised via
    TcRnDriver.externaliseAndTidyId, so they get Names like Ghic4.foo.

  - Ids bound by the debugger etc have Names constructed by
    IfaceEnv.newInteractiveBinder; at the call sites it is followed by
    mkVanillaGlobal or mkVanillaGlobalWithInfo.  So again, they are
    all Global, External.

  - TyCons, Classes, and Ids bound by other top-level declarations in
    GHCi (eg foreign import, record selectors) also get External
    Names, with Ghci9 (or 8, or 7, etc) as the module name.




Note [ic_tythings]
~~~~~~~~~~~~~~~~~~
The ic_tythings field contains
  * The TyThings declared by the user at the command prompt
    (eg Ids, TyCons, Classes)

  * The user-visible Ids that arise from such things, which
    *don't* come from 'implicitTyThings', notably:
       - record selectors
       - class ops
    The implicitTyThings are readily obtained from the TyThings
    but record selectors etc are not

It does *not* contain
  * DFunIds (they can be gotten from ic_instances)
  * CoAxioms (ditto)

See also Note [Interactively-bound Ids in GHCi]



Note [Override identical instances in GHCi]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If you declare a new instance in GHCi that is identical to a previous one,
we simply override the previous one; we don't regard it as overlapping.
e.g.    Prelude> data T = A | B
        Prelude> instance Eq T where ...
        Prelude> instance Eq T where ...   -- This one overrides

It's exactly the same for type-family instances.  See #7102


Note [Printing original names]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Deciding how to print names is pretty tricky.  We are given a name
P:M.T, where P is the package name, M is the defining module, and T is
the occurrence name, and we have to decide in which form to display
the name given a GlobalRdrEnv describing the current scope.

Ideally we want to display the name in the form in which it is in
scope.  However, the name might not be in scope at all, and that's
where it gets tricky.  Here are the cases:

 1. T uniquely maps to  P:M.T      --->  "T"      NameUnqual
 2. There is an X for which X.T
       uniquely maps to  P:M.T     --->  "X.T"    NameQual X
 3. There is no binding for "M.T"  --->  "M.T"    NameNotInScope1
 4. Otherwise                      --->  "P:M.T"  NameNotInScope2

(3) and (4) apply when the entity P:M.T is not in the GlobalRdrEnv at
all. In these cases we still want to refer to the name as "M.T", *but*
"M.T" might mean something else in the current scope (e.g. if there's
an "import X as M"), so to avoid confusion we avoid using "M.T" if
there's already a binding for it.  Instead we write P:M.T.

There's one further subtlety: in case (3), what if there are two
things around, P1:M.T and P2:M.T?  Then we don't want to print both of
them as M.T!  However only one of the modules P1:M and P2:M can be
exposed (say P2), so we use M.T for that, and P1:M.T for the other one.
This is handled by the qual_mod component of PrintUnqualified, inside
the (ppr mod) of case (3), in Name.pprModulePrefix



Note [Printing unit ids]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In the old days, original names were tied to PackageIds, which directly
corresponded to the entities that users wrote in Cabal files, and were perfectly
suitable for printing when we need to disambiguate packages.  However, with
UnitId, the situation can be different: if the key is instantiated with
some holes, we should try to give the user some more useful information.


Note [Implicit TyThings]
~~~~~~~~~~~~~~~~~~~~~~~~
  DEFINITION: An "implicit" TyThing is one that does not have its own
  IfaceDecl in an interface file.  Instead, its binding in the type
  environment is created as part of typechecking the IfaceDecl for
  some other thing.

Examples:
  * All DataCons are implicit, because they are generated from the
    IfaceDecl for the data/newtype.  Ditto class methods.

  * Record selectors are *not* implicit, because they get their own
    free-standing IfaceDecl.

  * Associated data/type families are implicit because they are
    included in the IfaceDecl of the parent class.  (NB: the
    IfaceClass decl happens to use IfaceDecl recursively for the
    associated types, but that's irrelevant here.)

  * Dictionary function Ids are not implicit.

  * Axioms for newtypes are implicit (same as above), but axioms
    for data/type family instances are *not* implicit (like DFunIds).


Note [Implementation of COMPLETE signatures]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A COMPLETE signature represents a set of conlikes (i.e., constructors or
pattern synonyms) such that if they are all pattern-matched against in a
function, it gives rise to a total function. An example is:

.. code-block:: haskell

  newtype Boolean = Boolean Int
  pattern F, T :: Boolean
  pattern F = Boolean 0
  pattern T = Boolean 1
  {-# COMPLETE F, T #-}

.. code-block:: haskell

  -- This is a total function
  booleanToInt :: Boolean -> Int
  booleanToInt F = 0
  booleanToInt T = 1

COMPLETE sets are represented internally in GHC with the CompleteMatch data
type. For example, {-# COMPLETE F, T #-} would be represented as:

.. code-block:: haskell

  CompleteMatch { complateMatchConLikes = [F, T]
                , completeMatchTyCon    = Boolean }

Note that GHC was able to infer the completeMatchTyCon (Boolean), but for the
cases in which it's ambiguous, you can also explicitly specify it in the source
language by writing this:

.. code-block:: haskell

  {-# COMPLETE F, T :: Boolean #-}

For efficiency purposes, GHC collects all of the CompleteMatches that it knows
about into a CompleteMatchMap, which is a map that is keyed by the
completeMatchTyCon. In other words, you could have a multiple COMPLETE sets
for the same TyCon:

.. code-block:: haskell

  {-# COMPLETE F, T1 :: Boolean #-}
  {-# COMPLETE F, T2 :: Boolean #-}

And looking up the values in the CompleteMatchMap associated with Boolean
would give you [CompleteMatch [F, T1] Boolean, CompleteMatch [F, T2] Boolean].
dsGetCompleteMatches in DsMeta accomplishes this lookup.

Also see Note [Typechecking Complete Matches] in TcBinds for a more detailed
explanation for how GHC ensures that all the conlikes in a COMPLETE set are
consistent.

