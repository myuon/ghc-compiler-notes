[[src]](https://github.com/ghc/ghc/tree/master/compiler/coreSyn/CoreFVs.hs)

(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

Taken quite directly from the Peyton Jones/Lester paper.


# \section{Finding the free variables of an expression}


This function simply finds the free variables of an expression.
So far as type variables are concerned, it only finds tyvars that are

        * free in type arguments,
        * free in the type of a binder,

but not those that are free in the type of variable occurrence.


# \section{Free names}


# 

# 

# Attaching free variables to every sub-expression

### Note: Rule free var hack

We used not to include the Id in its own rhs free-var set.
Otherwise the occurrence analyser makes bindings recursive:
        f x y = x+y
        RULE:  f (f x y) z  ==>  f x (f y z)
However, the occurrence analyser distinguishes "non-rule loop breakers"
from "rule-only loop breakers" (see BasicTypes.OccInfo).  So it will
put this 'f' in a Rec block, but will mark the binding as a non-rule loop
breaker, which is perfectly inlinable.


# Attaching free variables to every sub-expression

The free variable pass annotates every node in the expression with its
NON-GLOBAL free variables and type variables.


# \subsection{Free variables (and types)}
