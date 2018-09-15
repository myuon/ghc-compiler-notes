[[src]](https://github.com/ghc/ghc/tree/master/compiler/rename/RnBinds.hs)

(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

# Renaming and dependency analysis of bindings

This module does renaming and dependency analysis on value bindings in
the abstract syntax.  It does {\em not} do cycle-checks on class or
type-synonym declarations; those cannot be done at this stage because
they may be affected by renaming (which isn't fully worked out yet).



-- ToDo: Put the annotations into the monad, so that they arrive in the proper
-- place and can be used when complaining.

The code tree received by the function @rnBinds@ contains definitions
in where-clauses which are all apparently mutually recursive, but which may
not really depend upon each other. For example, in the top level program
\begin{verbatim}
f x = y where a = x
              y = x
\end{verbatim}
the definitions of @a@ and @y@ do not depend on each other at all.
Unfortunately, the typechecker cannot always check such definitions.
\footnote{Mycroft, A. 1984. Polymorphic type schemes and recursive
definitions. In Proceedings of the International Symposium on Programming,
Toulouse, pp. 217-39. LNCS 167. Springer Verlag.}
However, the typechecker usually can check definitions in which only the
strongly connected components have been collected into recursive bindings.
This is precisely what the function @rnBinds@ does.

ToDo: deal with case where a single monobinds binds the same variable
twice.

The vertag tag is a unique @Int@; the tags only need to be unique
within one @MonoBinds@, so that unique-Int plumbing is done explicitly
(heavy monad machinery not needed).

# naming conventions                                                   

\subsection[name-conventions]{Name conventions}

The basic algorithm involves walking over the tree and returning a tuple
containing the new tree plus its free variables. Some functions, such
as those walking polymorphic bindings (HsBinds) and qualifier lists in
list comprehensions (@Quals@), return the variables bound in local
environments. These are then used to calculate the free variables of the
expression evaluated in these environments.

Conventions for variable names are as follows:
\begin{itemize}
\item
new code is given a prime to distinguish it from the old.

\item
a set of variables defined in @Exp@ is written @dvExp@

\item
a set of variables free in @Exp@ is written @fvExp@
\end{itemize}

# analysing polymorphic bindings (HsBindGroup, HsBind)


\subsubsection[dep-HsBinds]{Polymorphic bindings}

Non-recursive expressions are reconstructed without any changes at top
level, although their component expressions may have to be altered.
However, non-recursive expressions are currently not expected as
\Haskell{} programs, and this code should not be executed.

Monomorphic bindings contain information that is returned in a tuple
(a @FlatMonoBinds@) containing:

\begin{enumerate}
\item
a unique @Int@ that serves as the ``vertex tag'' for this binding.

\item
the name of a function or the names in a pattern. These are a set
referred to as @dvLhs@, the defined variables of the left hand side.

\item
the free variables of the body. These are referred to as @fvBody@.

\item
the definition's actual code. This is referred to as just @code@.
\end{enumerate}

The function @nonRecDvFv@ returns two sets of variables. The first is
the set of variables defined in the set of monomorphic bindings, while the
second is the set of free variables in those bindings.

The set of variables defined in a non-recursive binding is just the
union of all of them, as @union@ removes duplicates. However, the
free variables in each successive set of cumulative bindings is the
union of those in the previous set plus those of the newest binding after
the defined variables of the previous set have been removed.

@rnMethodBinds@ deals only with the declarations in class and
instance declarations.  It expects only to see @FunMonoBind@s, and
it expects the global environment to contain bindings for the binders
(which are all class operations).

# \subsubsection{ Top-level bindings}


# HsLocalBinds


# ValBinds


### Note: Pattern bindings that bind no variables

Generally, we want to warn about pattern bindings like
  Just _ = e
because they don't do anything!  But we have two exceptions:

* A wildcard pattern
       _ = rhs
  which (a) is not that different from  _v = rhs
        (b) is sometimes used to give a type sig for,
            or an occurrence of, a variable on the RHS

* A strict pattern binding; that is, one with an outermost bang
     !Just _ = e
  This can fail, so unlike the lazy variant, it is not a no-op.
  Moreover, Trac #13646 argues that even for single constructor
  types, you might want to write the constructor.  See also #9127.

### Note: Free-variable space leak

We have
    fvs' = trim fvs
and we seq fvs' before turning it as part of a record.

The reason is that trim is sometimes something like
    \xs -> intersectNameSet (mkNameSet bound_names) xs
and we don't want to retain the list bound_names. This showed up in
trac ticket #1136.


# Dependency analysis and other support functions


# Pattern synonym bindings


### Note: Renaming pattern synonym variables


We rename pattern synonym declaractions backwards to normal to reuse
the logic already implemented for renaming patterns.

We first rename the RHS of a declaration which brings into
scope the variables bound by the pattern (as they would be
in normal function definitions). We then lookup the variables
which we want to bind in this local environment.

It is crucial that we then only lookup in the *local* environment which
only contains the variables brought into scope by the pattern and nothing
else. Amazingly no-one encountered this bug for 3 GHC versions but
it was possible to define a pattern synonym which referenced global
identifiers and worked correctly.

```
x = 5

pattern P :: Int -> ()
pattern P x <- _

f (P x) = x

> f () = 5
```

See #13470 for the original report.

### Note: Pattern synonym builders don't yield dependencies

When renaming a pattern synonym that has an explicit builder,
references in the builder definition should not be used when
calculating dependencies. For example, consider the following pattern
synonym definition:

pattern P x <- C1 x where
  P x = f (C1 x)

f (P x) = C2 x

In this case, 'P' needs to be typechecked in two passes:

1. Typecheck the pattern definition of 'P', which fully determines the
   type of 'P'. This step doesn't require knowing anything about 'f',
   since the builder definition is not looked at.

2. Typecheck the builder definition, which needs the typechecked
   definition of 'f' to be in scope; done by calls oo tcPatSynBuilderBind
   in TcBinds.tcValBinds.

This behaviour is implemented in 'tcValBinds', but it crucially
depends on 'P' not being put in a recursive group with 'f' (which
would make it look like a recursive pattern synonym a la 'pattern P =
P' which is unsound and rejected).

So:
 * We do not include builder fvs in the Uses returned by rnPatSynBind
   (which is then used for dependency analysis)
 * But we /do/ include them in the psb_fvs for the PatSynBind
 * In rnValBinds we record these builder uses, to avoid bogus
   unused-variable warnings (Trac #12548)


# Class/instance method bindings


 @rnMethodBinds@ is used for the method bindings of a class and an instance
declaration.   Like @rnBinds@ but without dependency analysis.

NOTA BENE: we record each {\em binder} of a method-bind group as a free variable.
That's crucial when dealing with an instance decl:
\begin{verbatim}
        instance Foo (T a) where
           op x = ...
\end{verbatim}
This might be the {\em sole} occurrence of @op@ for an imported class @Foo@,
and unless @op@ occurs we won't treat the type signature of @op@ in the class
decl for @Foo@ as a source of instance-decl gates.  But we should!  Indeed,
in many ways the @op@ in an instance decl is just like an occurrence, not
a binder.


# \subsubsection[dep-Sigs]{Signatures (and user-pragmas for values)}


@renameSigs@ checks for:
\begin{enumerate}
\item more than one sig for one thing;
\item signatures given for things not bound here;
\end{enumerate}

At the moment we don't gather free-var info from the types in
signatures.  We'd only need this if we wanted to report unused tyvars.


### Note: Orphan COMPLETE pragmas

We define a COMPLETE pragma to be a non-orphan if it includes at least
one conlike defined in the current module. Why is this sufficient?
Well if you have a pattern match

  case expr of
    P1 -> ...
    P2 -> ...
    P3 -> ...

any COMPLETE pragma which mentions a conlike other than P1, P2 or P3
will not be of any use in verifying that the pattern match is
exhaustive. So as we have certainly read the interface files that
define P1, P2 and P3, we will have loaded all non-orphan COMPLETE
pragmas that could be relevant to this pattern match.

For now we simply disallow orphan COMPLETE pragmas, as the added
complexity of supporting them properly doesn't seem worthwhile.


# \subsection{Match}


# \subsubsection{Guarded right-hand sides (GRHSs)}


# Source-code fixity declarations


# \subsection{Error messages}
