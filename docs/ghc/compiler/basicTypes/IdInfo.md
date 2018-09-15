[[src]](https://github.com/ghc/ghc/tree/master/compiler/basicTypes/IdInfo.hs)

(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1993-1998

# @IdInfos@: Non-essential information about @Ids@

(And a pretty good illustration of quite a few things wrong with
Haskell. [WDP 94/11])


# IdDetails


# \subsection{The main IdInfo type}


lb `seq`

# \subsection[arity-IdInfo]{Arity info about an @Id@}


For locally-defined Ids, the code generator maintains its own notion
of their arities; so it should not be asking...  (but other things
besides the code-generator need arity info!)


# \subsection{Inline-pragma information}


# Strictness


# RuleInfo


### Note: Specialisations and RULES in IdInfo

Generally speaking, a GlobalId has an *empty* RuleInfo.  All their
RULES are contained in the globally-built rule-base.  In principle,
one could attach the to M.f the RULES for M.f that are defined in M.
But we don't do that for instance declarations and so we just treat
them all uniformly.

The EXCEPTION is PrimOpIds, which do have rules in their IdInfo. That is
jsut for convenience really.

However, LocalIds may have non-empty RuleInfo.  We treat them
differently because:
  a) they might be nested, in which case a global table won't work
  b) the RULE might mention free variables, which we use to keep things alive

In TidyPgm, when the LocalId becomes a GlobalId, its RULES are stripped off
and put in the global list.


# \subsection[CG-IdInfo]{Code generator-related information}


# \subsection{Bulk operations on IdInfo}


# \subsection{TickBoxOp}


# Levity


### Note: Levity info


Ids store whether or not they can be levity-polymorphic at any amount
of saturation. This is helpful in optimizing the levity-polymorphism check
done in the desugarer, where we can usually learn that something is not
levity-polymorphic without actually figuring out its type. See
isExprLevPoly in CoreUtils for where this info is used. Storing
this is required to prevent perf/compiler/T5631 from blowing up.

