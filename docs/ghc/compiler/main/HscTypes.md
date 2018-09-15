[[src]](https://github.com/ghc/ghc/tree/master/compiler/main/HscTypes.hs)

(c) The University of Glasgow, 2006

# Types for the per-module compiler

# \subsection{HscEnv}


# \subsection{Package and Module Tables}


# \subsection{Metaprogramming}


# \subsection{Dealing with Annotations}


# \subsection{The Finder cache}


# \subsection{Symbol tables and Module details}


# The interactive context


### Note: The interactive package

Type, class, and value declarations at the command prompt are treated
as if they were defined in modules
   interactive:Ghci1
   interactive:Ghci2
   ...etc...
with each bunch of declarations using a new module, all sharing a
common package 'interactive' (see Module.interactiveUnitId, and
PrelNames.mkInteractiveModule).

This scheme deals well with shadowing.  For example:

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

  In contrast, tcg_inst_env, tcg_fam_inst_env, have instances from
  all GhciN modules, which makes sense -- they are all "home package"
  modules.

### Note: Interactively-bound Ids in GHCi

### Note: The interactive package

 (a) They must be GlobalIds (not LocalIds) otherwise when we come to
     compile an expression using these ids later, the byte code
     generator will consider the occurrences to be free rather than
     global.

 (b) Having an External Name is important because of Note
     [GlobalRdrEnv shadowing] in RdrName

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

### Note: ic_tythings

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

### Note: Interactively-bound Ids in GHCi

### Note: Override identical instances in GHCi

If you declare a new instance in GHCi that is identical to a previous one,
we simply override the previous one; we don't regard it as overlapping.
e.g.    Prelude> data T = A | B
        Prelude> instance Eq T where ...
        Prelude> instance Eq T where ...   -- This one overrides

It's exactly the same for type-family instances.  See Trac #7102


# Building a PrintUnqualified


### Note: Printing original names

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

### Note: Printing unit ids

In the old days, original names were tied to PackageIds, which directly
corresponded to the entities that users wrote in Cabal files, and were perfectly
suitable for printing when we need to disambiguate packages.  However, with
UnitId, the situation can be different: if the key is instantiated with
some holes, we should try to give the user some more useful information.


# Implicit TyThings


### Note: Implicit TyThings

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


# TypeEnv


# \subsection{MonadThings and friends}


# \subsection{Auxiliary types}


These types are defined here because they are mentioned in ModDetails,
but they are mostly elaborated elsewhere


# \subsection{WhatsImported}


# The External Package State



Names in a NameCache are always stored as a Global, and have the SrcLoc
of their binding locations.

Actually that's not quite right.  When we first encounter the original
name, we might not be at its binding site (e.g. we are reading an
interface file); so we give it 'noSrcLoc' then.  Later, when we find
its binding site, we fix it up.


# The module graph and ModSummary type
        A ModSummary is a node in the compilation manager's
        dependency graph, and it's also passed to hscMain


# \subsection{Recmpilation}


# \subsection{Hpc Support}


# \subsection{Vectorisation Support}


The following information is generated and consumed by the vectorisation
subsystem.  It communicates the vectorisation status of declarations from one
module to another.

Why do we need both f and f_v in the ModGuts/ModDetails/EPS version VectInfo
below?  We need to know `f' when converting to IfaceVectInfo.  However, during
vectorisation, we need to know `f_v', whose `Var' we cannot lookup based
on just the OccName easily in a Core pass.


# \subsection{Safe Haskell Support}


This stuff here is related to supporting the Safe Haskell extension,
primarily about storing under what trust type a module has been compiled.


# \subsection{Parser result}


# \subsection{Linkable stuff}


This stuff is in here, rather than (say) in Linker.hs, because the Linker.hs
stuff is the *dynamic* linker, and isn't present in a stage-1 compiler


### Note: Implementation of COMPLETE signatures

A COMPLETE signature represents a set of conlikes (i.e., constructors or
pattern synonyms) such that if they are all pattern-matched against in a
function, it gives rise to a total function. An example is: