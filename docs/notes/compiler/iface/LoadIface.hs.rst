`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/iface/LoadIface.hs>`_

Note [Loading instances for wired-in things]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We need to make sure that we have at least *read* the interface files
for any module with an instance decl or RULE that we might want.

* If the instance decl is an orphan, we have a whole separate mechanism
  (loadOrphanModules)

* If the instance decl is not an orphan, then the act of looking at the
  TyCon or Class will force in the defining module for the
  TyCon/Class, and hence the instance decl

* BUT, if the TyCon is a wired-in TyCon, we don't really need its interface;
  but we must make sure we read its interface in case it has instances or
  rules.  That is what LoadIface.loadWiredInHomeIface does.  It's called
  from TcIface.{tcImportDecl, checkWiredInTyCon, ifCheckWiredInThing}

* HOWEVER, only do this for TyCons.  There are no wired-in Classes.  There
  are some wired-in Ids, but we don't want to load their interfaces. For
  example, Control.Exception.Base.recSelError is wired in, but that module
  is compiled late in the base library, and we don't want to force it to
  load before it's been compiled!

All of this is done by the type checker. The renamer plays no role.
(It used to, but no longer.)


Note [Loading your own hi-boot file]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Generally speaking, when compiling module M, we should not
load M.hi boot into the EPS.  After all, we are very shortly
going to have full information about M.  Moreover, see
Note [Do not update EPS with your own hi-boot] in MkIface.

But there is a HORRIBLE HACK here.

* At the end of tcRnImports, we call checkFamInstConsistency to
  check consistency of imported type-family instances
  See Note [The type family instance consistency story] in FamInst

* Alas, those instances may refer to data types defined in M,
  if there is a M.hs-boot.

* And that means we end up loading M.hi-boot, because those
  data types are not yet in the type environment.

But in this wierd case, /all/ we need is the types. We don't need
instances, rules etc.  And if we put the instances in the EPS
we get "duplicate instance" warnings when we compile the "real"
instance in M itself.  Hence the strange business of just updateing
the eps_PTE.

This really happens in practice.  The module HsExpr.hs gets
"duplicate instance" errors if this hack is not present.

This is a mess.


Note [HPT space leak] (#15111)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In IfL, we defer some work until it is demanded using forkM, such
as building TyThings from IfaceDecls. These thunks are stored in
the ExternalPackageState, and they might never be poked.  If we're
not careful, these thunks will capture the state of the loaded
program when we read an interface file, and retain all that data
for ever.

Therefore, when loading a package interface file , we use a "clean"
version of the HscEnv with all the data about the currently loaded
program stripped out. Most of the fields can be panics because
we'll never read them, but hsc_HPT needs to be empty because this
interface will cause other interfaces to be loaded recursively, and
when looking up those interfaces we use the HPT in loadInterface.
We know that none of the interfaces below here can refer to
home-package modules however, so it's safe for the HPT to be empty.


Note [Home module load error]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If the sought-for interface is in the current package (as determined
by -package-name flag) then it jolly well should already be in the HPT
because we process home-package modules in dependency order.  (Except
in one-shot mode; see notes with hsc_HPT decl in HscTypes).

It is possible (though hard) to get this error through user behaviour.
  * Suppose package P (modules P1, P2) depends on package Q (modules Q1,
    Q2, with Q2 importing Q1)
  * We compile both packages.
  * Now we edit package Q so that it somehow depends on P
  * Now recompile Q with --make (without recompiling P).
  * Then Q1 imports, say, P1, which in turn depends on Q2. So Q2
    is a home-package module which is not yet in the HPT!  Disaster.

This actually happened with P=base, Q=ghc-prim, via the AMP warnings.
See #8320.


Note [Name qualification with --show-iface]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In order to disambiguate between identifiers from different modules, we qualify
all names that don't originate in the current module. In order to keep visual
noise as low as possible, we keep local names unqualified.

For some background on this choice see trac #15269.

