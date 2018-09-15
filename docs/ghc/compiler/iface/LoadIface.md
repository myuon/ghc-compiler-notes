[[src]](https://github.com/ghc/ghc/tree/master/compiler/iface/LoadIface.hs)

(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998


Loading interface files


# 

The main idea is this.  We are chugging along type-checking source code, and
find a reference to GHC.Base.map.  We call tcLookupGlobal, which doesn't find
it in the EPS type envt.  So it
        1 loads GHC.Base.hi
        2 gets the decl for GHC.Base.map
        3 typechecks it via tcIfaceDecl
        4 and adds it to the type env in the EPS

Note that DURING STEP 4, we may find that map's type mentions a type
constructor that also

Notice that for imported things we read the current version from the EPS
mutable variable.  This is important in situations like
        ...$(e1)...$(e2)...
where the code that e1 expands to might import some defns that
also turn out to be needed by the code that e2 expands to.


# Checks for wired-in things


### Note: Loading instances for wired-in things

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


# 

# 

# 

# 

# \subsection{Reading an interface file}


### Note: Home module load error

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
See Trac #8320.


# Wired-in interface for GHC.Prim


# Wired-in interface for GHC.Prim


# \subsection{Statistics}


# Printing interfaces



When printing export lists, we print like this:
        Avail   f               f
        AvailTC C [C, x, y]     C(x,y)
        AvailTC C [x, y]        C!(x,y)         -- Exporting x, y but not C


# \subsection{Errors}
