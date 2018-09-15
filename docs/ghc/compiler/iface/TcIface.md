[[src]](https://github.com/ghc/ghc/tree/master/compiler/iface/TcIface.hs)

(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998


Type checking of type signatures in interface files



This module takes

        IfaceDecl -> TyThing
        IfaceType -> Type
        etc

An IfaceDecl is populated with RdrNames, and these are not renamed to
Names before typechecking, because there should be no scope errors etc.

        -- For (b) consider: f = \$(...h....)
        -- where h is imported, and calls f via an hi-boot file.
        -- This is bad!  But it is not seen as a staging error, because h
        -- is indeed imported.  We don't want the type-checker to black-hole
        -- when simplifying and compiling the splice!
        --
        -- Simple solution: discard any unfolding that mentions a variable
        -- bound in this module (and hence not yet processed).
        -- The discarding happens when forkM finds a type error.

# Type-checking a complete interface


Suppose we discover we don't need to recompile.  Then we must type
check the old interface file.  This is a bit different to the
incremental type checking we do as we suck in interface files.  Instead
we do things similarly as when we are typechecking source decls: we
bring into scope the type envt for the interface all at once, using a
knot.  Remember, the decls aren't necessarily in dependency order --
and even if they were, the type decls might be mutually recursive.

### Note: Knot-tying typecheckIface

Suppose we are typechecking an interface A.hi, and we come across
a Name for another entity defined in A.hi.  How do we get the
'TyCon', in this case?  There are three cases:

    1) tcHiBootIface in TcIface: We're typechecking an hi-boot file in
    preparation of checking if the hs file we're building
    is compatible.  In this case, we want all of the internal
    TyCons to MATCH the ones that we just constructed during
    typechecking: the knot is thus tied through if_rec_types.

    2) retypecheckLoop in GhcMake: We are retypechecking a
    mutually recursive cluster of hi files, in order to ensure
    that all of the references refer to each other correctly.
    In this case, the knot is tied through the HPT passed in,
    which contains all of the interfaces we are in the process
    of typechecking.

    3) genModDetails in HscMain: We are typechecking an
    old interface to generate the ModDetails.  In this case,
    we do the same thing as (2) and pass in an HPT with
    the HomeModInfo being generated to tie knots.

The upshot is that the CLIENT of this function is responsible
for making sure that the knot is tied correctly.  If you don't,
then you'll get a message saying that we couldn't load the
declaration you wanted.

BTW, in one-shot mode we never call typecheckIface; instead,
loadInterface handles type-checking interface.  In that case,
knots are tied through the EPS.  No problem!


# Typechecking for merging


# Type and class declarations


# Type and class declarations


When typechecking a data type decl, we *lazily* (via forkM) typecheck
the constructor argument types.  This is in the hope that we may never
poke on those argument types, and hence may never need to load the
interface files for types mentioned in the arg types.

E.g.
        data Foo.S = MkS Baz.T
Maybe we can get away without even loading the interface for Baz!

This is not just a performance thing.  Suppose we have
        data Foo.S = MkS Baz.T
        data Baz.T = MkT Foo.S
(in different interface files, of course).
Now, first we load and typecheck Foo.S, and add it to the type envt.
If we do explore MkS's argument, we'll load and typecheck Baz.T.
If we explore MkT's argument we'll find Foo.S already in the envt.

If we typechecked constructor args eagerly, when loading Foo.S we'd try to
typecheck the type Baz.T.  So we'd fault in Baz.T... and then need Foo.S...
which isn't done yet.

All very cunning. However, there is a rather subtle gotcha which bit
me when developing this stuff.  When we typecheck the decl for S, we
extend the type envt with S, MkS, and all its implicit Ids.  Suppose
(a bug, but it happened) that the list of implicit Ids depended in
turn on the constructor arg types.  Then the following sequence of
events takes place:
        * we build a thunk <t> for the constructor arg tys
        * we build a thunk for the extended type environment (depends on <t>)
        * we write the extended type envt into the global EPS mutvar

Now we look something up in the type envt
        * that pulls on <t>
        * which reads the global type envt out of the global EPS mutvar
        * but that depends in turn on <t>

It's subtle, because, it'd work fine if we typechecked the constructor args
eagerly -- they don't need the extended type envt.  They just get the extended
type envt by accident, because they look at it later.

What this means is that the implicitTyThings MUST NOT DEPEND on any of
the forkM stuff.


### Note: Synonym kind loop

Notice that we eagerly grab the *kind* from the interface file, but
build a forkM thunk for the *rhs* (and family stuff).  To see why,
consider this (Trac #2412)

# Rules


We move a IfaceRule from eps_rules to eps_rule_base when all its LHS free vars
are in the type environment.  However, remember that typechecking a Rule may
(as a side effect) augment the type envt, and so we may need to iterate the process.


# Annotations


# Complete Match Pragmas


# Vectorisation information


# Types


# 

# Core


 Don't ignore prags; we are inside one! 

 Don't ignore prags; we are inside one! 

# IdInfo


 Top level 


For unfoldings we try to do the job lazily, so that we never type check
an unfolding that isn't going to be looked at.


# Getting from Names to TyThings


# Bindings
