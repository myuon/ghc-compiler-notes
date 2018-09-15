[[src]](https://github.com/ghc/ghc/tree/master/compiler/prelude/PrelNames.hs)

(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

# Definitions of prelude modules and names


Nota Bene: all Names defined in here should come from the base package

 - ModuleNames for prelude modules,
        e.g.    pREL_BASE_Name :: ModuleName

 - Modules for prelude modules
        e.g.    pREL_Base :: Module

 - Uniques for Ids, DataCons, TyCons and Classes that the compiler
   "knows about" in some way
        e.g.    intTyConKey :: Unique
                minusClassOpKey :: Unique

 - Names for Ids, DataCons, TyCons and Classes that the compiler
   "knows about" in some way
        e.g.    intTyConName :: Name
                minusName    :: Name
   One of these Names contains
        (a) the module and occurrence name of the thing
        (b) its Unique
   The way the compiler "knows about" one of these things is
   where the type checker or desugarer needs to look it up. For
   example, when desugaring list comprehensions the desugarer
   needs to conjure up 'foldr'.  It does this by looking up
   foldrName in the environment.

 - RdrNames for Ids, DataCons etc that the compiler may emit into
   generated code (e.g. for deriving).  It's not necessary to know
   the uniques for these guys, only their names

### Note: Known-key names

It is *very* important that the compiler gives wired-in things and
things with "known-key" names the correct Uniques wherever they
occur. We have to be careful about this in exactly two places:

  1. When we parse some source code, renaming the AST better yield an
     AST whose Names have the correct uniques

  2. When we read an interface file, the read-in gubbins better have
     the right uniques

This is accomplished through a combination of mechanisms:

  1. When parsing source code, the RdrName-decorated AST has some
     RdrNames which are Exact. These are wired-in RdrNames where the
     we could directly tell from the parsed syntax what Name to
     use. For example, when we parse a [] in a type we can just insert
     an Exact RdrName Name with the listTyConKey.

     Currently, I believe this is just an optimisation: it would be
     equally valid to just output Orig RdrNames that correctly record
     the module etc we expect the final Name to come from. However,
     were we to eliminate isBuiltInOcc_maybe it would become essential
     (see point 3).

  2. The knownKeyNames (which consist of the basicKnownKeyNames from
     the module, and those names reachable via the wired-in stuff from
     TysWiredIn) are used to initialise the "OrigNameCache" in
     IfaceEnv.  This initialization ensures that when the type checker
     or renamer (both of which use IfaceEnv) look up an original name
     (i.e. a pair of a Module and an OccName) for a known-key name
     they get the correct Unique.

     This is the most important mechanism for ensuring that known-key
     stuff gets the right Unique, and is why it is so important to
     place your known-key names in the appropriate lists.

### Note: Infinite families of known-key names

### Note: Infinite families of known-key names

### Note: Known-key names

We instead handle tuples and sums separately from the "vanilla" known-key
things,

  a) The parser recognises them specially and generates an Exact Name (hence not
     looked up in the orig-name cache)

### Note: How tuples work

Most of the infinite families cannot occur in source code, so mechanisms (a) and (b)
suffice to ensure that they always have the right Unique. In particular,
implicit param TyCon names, constraint tuples and Any TyCons cannot be mentioned
by the user. For those things that *can* appear in source programs,

  c) IfaceEnv.lookupOrigNameCache uses isBuiltInOcc_maybe to map built-in syntax
     directly onto the corresponding name, rather than trying to find it in the
     original-name cache.

### Note: Built-in syntax and the OrigNameCache

# allNameStrings


# \subsection{Local Names}


This *local* name is used by the interactive stuff


# \subsection{Known key Names}


This section tells what the compiler knows about the association of
names with uniques.  These ones are the *non* wired-in ones.  The
wired in ones are defined in TysWiredIn etc.


# \subsection{Module names}



--MetaHaskell Extension Add a new module here


# RdrNames


# \subsection{Known-key names}


Many of these Names are not really "built in", but some parts of the
compiler (notably the deriving mechanism) need to mention their names,
and it's convenient to write them all down in one place.


# \subsection{Local helpers}


All these are original names; hence mkOrig


# \subsubsection[Uniques-prelude-Classes]{@Uniques@ for wired-in @Classes@}


# \subsubsection[Uniques-prelude-TyCons]{@Uniques@ for wired-in @TyCons@}


# \subsubsection[Uniques-prelude-DataCons]{@Uniques@ for wired-in @DataCons@}


# \subsubsection[Uniques-prelude-Ids]{@Uniques@ for wired-in @Ids@ (except @DataCons@)}



Certain class operations from Prelude classes.  They get their own
uniques so we can look them up easily when we want to conjure them up
during type checking.


# \subsection[Class-std-groups]{Standard groups of Prelude classes}


NOTE: @Eq@ and @Text@ do need to appear in @standardClasses@
even though every numeric class has these two as a superclass,
because the list of ambiguous dictionaries hasn't been simplified.



@derivableClassKeys@ is also used in checking \tr{deriving} constructs
(@TcDeriv@).


# Semi-builtin names


The following names should be considered by GHCi to be in scope always.

