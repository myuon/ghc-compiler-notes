[[src]](https://github.com/ghc/ghc/tree/master/compiler/basicTypes/Var.hs)

(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

\section{@Vars@: Variables}


# Synonyms


 Many passes apply a substitution, and it's very handy to have type
   synonyms to remind us whether or not the substitution has been applied 

### Note: Evidence: EvIds and CoVars

* An EvId (evidence Id) is a term-level evidence variable
  (dictionary, implicit parameter, or equality). Could be boxed or unboxed.

* DictId, IpId, and EqVar are synonyms when we know what kind of
  evidence we are talking about.  For example, an EqVar has type (t1 ~ t2).

* A CoVar is always an un-lifted coercion, of type (t1 ~# t2) or (t1 ~R# t2)

### Note: Kind and type variables

Before kind polymorphism, TyVar were used to mean type variables. Now
they are use to mean kind *or* type variables. KindVar is used when we
know for sure that it is a kind variable. In future, we might want to
go over the whole compiler code to use:
   - TKVar   to mean kind or type variables
   - TypeVar to mean         type variables only
   - KindVar to mean kind         variables

# \subsection{The main data type declarations}



Every @Var@ has a @Unique@, to uniquify it and for fast comparison, a
@Type@, and an @IdInfo@ (non-essential info about it, e.g.,
strictness).  The essential info about different kinds of @Vars@ is
in its @VarDetails@.


### Note: ExportFlag on binders

An ExportFlag of "Exported" on a top-level binder says "keep this
binding alive; do not drop it as dead code".  This transitively
keeps alive all the other top-level bindings that this binding refers
to.  This property is persisted all the way down the pipeline, so that
the binding will be compiled all the way to object code, and its
symbols will appear in the linker symbol table.

However, note that this use of "exported" is quite different to the
export list on a Haskell module.  Setting the ExportFlag on an Id does
/not/ mean that if you import the module (in Haskell source code) you
will see this Id.  Of course, things that appear in the export list
of the source Haskell module do indeed have their ExportFlag set.
But many other things, such as dictionary functions, are kept alive
by having their ExportFlag set, even though they are not exported
in the source-code sense.

We should probably use a different term for ExportFlag, like
KeepAlive.

### Note: GlobalId/LocalId

A GlobalId is
  * always a constant (top-level)
  * imported, or data constructor, or primop, or record selector
  * has a Unique that is globally unique across the whole
    GHC invocation (a single invocation may compile multiple modules)
  * never treated as a candidate by the free-variable finder;
        it's a constant!

A LocalId is
  * bound within an expression (lambda, case, local let(rec))
  * or defined at top level in the module being compiled
  * always treated as a candidate by the free-variable finder

After CoreTidy, top-level LocalIds are turned into GlobalIds


# ArgFlag


# TyVarBndr, TyVarBinder


# Type and kind variables                              

# 

# \subsection{Predicates over variables}
