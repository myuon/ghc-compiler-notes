[[src]](https://github.com/ghc/ghc/tree/master/compiler/basicTypes/RdrName.hs)

(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998


# \subsection{The main data type}


# \subsection{Simple functions}


# \subsection{Instances}


# LocalRdrEnv


### Note: Local bindings with Exact Names

With Template Haskell we can make local bindings that have Exact Names.
Computing shadowing etc may use elemLocalRdrEnv (at least it certainly
does so in RnTpes.bindHsQTyVars), so for an Exact Name we must consult
the in-scope-name-set.

# GlobalRdrEnv


### Note: GlobalRdrElt provenance

The gre_lcl and gre_imp fields of a GlobalRdrElt describe its "provenance",
i.e. how the Name came to be in scope.  It can be in scope two ways:
  - gre_lcl = True: it is bound in this module
  - gre_imp: a list of all the imports that brought it into scope

It's an INVARIANT that you have one or the other; that is, either
gre_lcl is True, or gre_imp is non-empty.

It is just possible to have *both* if there is a module loop: a Name
is defined locally in A, and also brought into scope by importing a
module that SOURCE-imported A.  Exapmle (Trac #7672):

 A.hs-boot   module A where
               data T

### Note: GRE filtering

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

Now the the "ambiguous occurrence" message can correctly report how the
ambiguity arises.


### Note: GlobalRdrEnv shadowing

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

  - Data types also have Extenal Names, like Ghci4.T; but we still want
    'T' to mean the newly-declared 'T', not an old one.

### Note: Top-level Names in Template Haskell decl quotes

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


# ImportSpec
