[[src]](https://github.com/ghc/ghc/tree/master/compiler/basicTypes/BasicTypes.hs)

(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1997-1998

# Miscellanous types

This module defines a miscellaneously collection of very simple
types that

\begin{itemize}
\item have no other obvious home
\item don't depend on any other complicated types
\item are used in more than one "part" of the compiler
\end{itemize}


# Binary choice


# \subsection[Arity]{Arity}


# Constructor tags


# \subsection[Alignment]{Alignment}


# One-shot information


# Swap flag


# \subsection[FunctionOrData]{FunctionOrData}


# \subsection[Version]{Module and identifier version numbers}


# Deprecations


# Rules


# \subsection[Fixity]{Fixity info}



Consider

\begin{verbatim}
        a `op1` b `op2` c
\end{verbatim}
@(compareFixity op1 op2)@ tells which way to arrange application, or
whether there's an error.


# \subsection[Top-level/local]{Top-level/not-top level flag}


# Boxity flag


# Recursive/Non-Recursive flag


# Code origin


# Deriving strategies


# Instance overlap flag


\# OVERLAPPABLE'@ or
--                              @'\{-\# OVERLAPPING'@ or
--                              @'\{-\# OVERLAPS'@ or
--                              @'\{-\# INCOHERENT'@,
--      'ApiAnnotation.AnnClose' @`\#-\}`@,

-- For details on above see note [Api annotations] in ApiAnnotation
data OverlapFlag = OverlapFlag
  { overlapMode   :: OverlapMode
  , isSafeOverlap :: Bool
  } deriving (Eq, Data)

setOverlapModeMaybe :: OverlapFlag -> Maybe OverlapMode -> OverlapFlag
setOverlapModeMaybe f Nothing  = f
setOverlapModeMaybe f (Just m) = f { overlapMode = m }

hasIncoherentFlag :: OverlapMode -> Bool
hasIncoherentFlag mode =
  case mode of
    Incoherent   _ -> True
    _              -> False

hasOverlappableFlag :: OverlapMode -> Bool
hasOverlappableFlag mode =
  case mode of
    Overlappable _ -> True
    Overlaps     _ -> True
    Incoherent   _ -> True
    _              -> False

hasOverlappingFlag :: OverlapMode -> Bool
hasOverlappingFlag mode =
  case mode of
    Overlapping  _ -> True
    Overlaps     _ -> True
    Incoherent   _ -> True
    _              -> False

### Note: Rules for instance lookup

### Note: Pragma source text

# INCOHERENT 

# Type precedence


### Note: Precedence in types

Many pretty-printing functions have type
    ppr_ty :: TyPrec -> Type -> SDoc

The TyPrec gives the binding strength of the context.  For example, in
   T ty1 ty2
we will pretty-print 'ty1' and 'ty2' with the call
  (ppr_ty TyConPrec ty)
to indicate that the context is that of an argument of a TyConApp.

We use this consistently for Type and HsType.

### Note: Type operator precedence

We don't keep the fixity of type operators in the operator. So the
pretty printer follows the following precedence order:

   TyConPrec         Type constructor application
   TyOpPrec/FunPrec  Operator application and function arrow

We have FunPrec and TyOpPrec to represent the precedence of function
arrow and type operators respectively, but currently we implement
FunPred == TyOpPrec, so that we don't distinguish the two. Reason:
it's hard to parse a type like
    a ~ b => c * d -> e - f

By treating TyOpPrec = FunPrec we end up with more parens
    (a ~ b) => (c * d) -> (e - f)

But the two are different constructors of TyPrec so we could make
(->) bind more or less tightly if we wanted.


# Tuples


# Sums


# \subsection[Generic]{Generic flag}


This is the "Embedding-Projection pair" datatype, it contains
two pieces of code (normally either RenamedExpr's or Id's)
If we have a such a pair (EP from to), the idea is that 'from' and 'to'
represents functions of type

        from :: T -> Tring
        to   :: Tring -> T

And we should have

        to (from x) = x

T and Tring are arbitrary, but typically T is the 'main' type while
Tring is the 'representation' type.  (This just helps us remember
whether to use 'from' or 'to'.



Embedding-projection pairs are used in several places:

First of all, each type constructor has an EP associated with it, the
code in EP converts (datatype T) from T to Tring and back again.

Secondly, when we are filling in Generic methods (in the typechecker,
tcMethodBinds), we are constructing bimaps by induction on the structure
of the type of the method signature.

# \subsection{Occurrence information}


This data type is used exclusively by the simplifier, but it appears in a
SubstResult, which is currently defined in VarEnv, which is pretty near
the base of the module hierarchy.  So it seemed simpler to put the
defn of OccInfo here, safely at the bottom


### Note: LoopBreaker OccInfo

   IAmALoopBreaker True  <=> A "weak" or rules-only loop breaker
                             Do not preInlineUnconditionally

   IAmALoopBreaker False <=> A "strong" loop breaker
                             Do not inline at all

### Note: Weak loop breakers

### Note: TailCallInfo

The occurrence analyser determines what can be made into a join point, but it
doesn't change the binder into a JoinId because then it would be inconsistent
with the occurrences. Thus it's left to the simplifier (or to simpleOptExpr) to
change the IdDetails.

### Note: Invariants on join points

This info is quite fragile and should not be relied upon unless the occurrence
analyser has *just* run. Use 'Id.isJoinId_maybe' for the permanent state of
the join-point-hood of a binder; a join id itself will not be marked
AlwaysTailCalled.

Note that there is a 'TailCallInfo' on a 'ManyOccs' value. One might expect that
being tail-called would mean that the variable could only appear once per branch
(thus getting a `OneOcc { occ_one_br = True }` occurrence info), but a join
point can also be invoked from other join points, not just from case branches:

  let j1 x = ...
      j2 y = ... j1 z {- tail call 

 Has default method 

 Has generic default method 

# \subsection{Success flag}


# \subsection{Source Text}


### Note: Pragma source text

The lexer does a case-insensitive match for pragmas, as well as
accepting both UK and US spelling variants.

So

# \subsection{Activation}


When a rule or inlining is active


### Note: InlinePragma

This data type mirrors what you can write in an INLINE or NOINLINE pragma in
the source program.

If you write nothing at all, you get defaultInlinePragma:
   inl_inline = NoUserInline
   inl_act    = AlwaysActive
   inl_rule   = FunLike

It's not possible to get that combination by *writing* something, so
if an Id has defaultInlinePragma it means the user didn't specify anything.

If inl_inline = Inline or Inlineable, then the Id should have an InlineRule unfolding.

If you want to know where InlinePragmas take effect: Look in DsBinds.makeCorePair

### Note: inl_inline and inl_act

* inl_inline says what the user wrote: did she say INLINE, NOINLINE,
  INLINABLE, or nothing at all

* inl_act says in what phases the unfolding is active or inactive
  E.g  If you write INLINE[1]    then inl_act will be set to ActiveAfter 1
       If you write NOINLINE[1]  then inl_act will be set to ActiveBefore 1
       If you write NOINLINE[~1] then inl_act will be set to ActiveAfter 1
  So note that inl_act does not say what pragma you wrote: it just
  expresses its consequences

* inl_act just says when the unfolding is active; it doesn't say what
  to inline.  If you say INLINE f, then f's inl_act will be AlwaysActive,
  but in addition f will get a "stable unfolding" with UnfoldingGuidance
  that tells the inliner to be pretty eager about it.

### Note: CONLIKE pragma

# INLINE"
                                   , inl_act = AlwaysActive
                                   , inl_rule = FunLike
                                   , inl_inline = NoUserInline
                                   , inl_sat = Nothing }

alwaysInlinePragma = defaultInlinePragma { inl_inline = Inline }
neverInlinePragma  = defaultInlinePragma { inl_act    = NeverActive }

inlinePragmaSpec :: InlinePragma -> InlineSpec
inlinePragmaSpec = inl_inline

-- A DFun has an always-active inline activation so that
-- exprIsConApp_maybe can "see" its unfolding
-- (However, its actual Unfolding is a DFunUnfolding, which is
--  never inlined other than via exprIsConApp_maybe.)
dfunInlinePragma   = defaultInlinePragma { inl_act  = AlwaysActive
                                         , inl_rule = ConLike }

isDefaultInlinePragma :: InlinePragma -> Bool
isDefaultInlinePragma (InlinePragma { inl_act = activation
                                    , inl_rule = match_info
                                    , inl_inline = inline })
  = noUserInlineSpec inline && isAlwaysActive activation && isFunLike match_info

isInlinePragma :: InlinePragma -> Bool
isInlinePragma prag = case inl_inline prag of
                        Inline -> True
                        _      -> False

isInlinablePragma :: InlinePragma -> Bool
isInlinablePragma prag = case inl_inline prag of
                           Inlinable -> True
                           _         -> False

isAnyInlinePragma :: InlinePragma -> Bool
-- INLINE or INLINABLE
isAnyInlinePragma prag = case inl_inline prag of
                        Inline    -> True
                        Inlinable -> True
                        _         -> False

inlinePragmaSat :: InlinePragma -> Maybe Arity
inlinePragmaSat = inl_sat

inlinePragmaActivation :: InlinePragma -> Activation
inlinePragmaActivation (InlinePragma { inl_act = activation }) = activation

inlinePragmaRuleMatchInfo :: InlinePragma -> RuleMatchInfo
inlinePragmaRuleMatchInfo (InlinePragma { inl_rule = info }) = info

setInlinePragmaActivation :: InlinePragma -> Activation -> InlinePragma
setInlinePragmaActivation prag activation = prag { inl_act = activation }

setInlinePragmaRuleMatchInfo :: InlinePragma -> RuleMatchInfo -> InlinePragma
setInlinePragmaRuleMatchInfo prag info = prag { inl_rule = info }

instance Outputable Activation where
   ppr AlwaysActive       = empty
   ppr NeverActive        = brackets (text "~")
   ppr (ActiveBefore _ n) = brackets (char '~' <> int n)
   ppr (ActiveAfter  _ n) = brackets (int n)

instance Outputable RuleMatchInfo where
   ppr ConLike = text "CONLIKE"
   ppr FunLike = text "FUNLIKE"

instance Outputable InlineSpec where
   ppr Inline       = text "INLINE"
   ppr NoInline     = text "NOINLINE"
   ppr Inlinable    = text "INLINABLE"
   ppr NoUserInline = text "NOUSERINLINE" -- what is better?

instance Outputable InlinePragma where
  ppr = pprInline

pprInline :: InlinePragma -> SDoc
pprInline = pprInline' True

pprInlineDebug :: InlinePragma -> SDoc
pprInlineDebug = pprInline' False

pprInline' :: Bool -> InlinePragma -> SDoc
pprInline' emptyInline (InlinePragma { inl_inline = inline, inl_act = activation
                                    , inl_rule = info, inl_sat = mb_arity })
    = pp_inl inline <> pp_act inline activation <+> pp_sat <+> pp_info
    where
      pp_inl x = if emptyInline then empty else ppr x

      pp_act Inline   AlwaysActive = empty
      pp_act NoInline NeverActive  = empty
      pp_act _        act          = ppr act

      pp_sat | Just ar <- mb_arity = parens (text "sat-args=" <> int ar)
             | otherwise           = empty
      pp_info | isFunLike info = empty
              | otherwise      = ppr info

isActive :: CompilerPhase -> Activation -> Bool
isActive InitialPhase AlwaysActive      = True
isActive InitialPhase (ActiveBefore {}) = True
isActive InitialPhase _                 = False
isActive (Phase p)    act               = isActiveIn p act

isActiveIn :: PhaseNum -> Activation -> Bool
isActiveIn _ NeverActive        = False
isActiveIn _ AlwaysActive       = True
isActiveIn p (ActiveAfter _ n)  = p <= n
isActiveIn p (ActiveBefore _ n) = p >  n

### Note: Activation competition

competesWith (ActiveBefore {})  AlwaysActive      = True
competesWith (ActiveBefore {})  (ActiveBefore {}) = True
competesWith (ActiveBefore _ a) (ActiveAfter _ b) = a < b

competesWith (ActiveAfter {})  AlwaysActive      = False
competesWith (ActiveAfter {})  (ActiveBefore {}) = False
competesWith (ActiveAfter _ a) (ActiveAfter _ b) = a >= b

### Note: Competing activations

### Note: Rules and inlining/other rules

We say that act1 "competes with" act2 iff
   act1 is active in the phase when act2 *becomes* active
NB: remember that phases count *down*: 2, 1, 0!

It's too conservative to ensure that the two are never simultaneously
active.  For example, a rule might be always active, and an inlining
might switch on in phase 2.  We could switch off the rule, but it does
no harm.


# IntWithInf


Represents an integer or positive infinity

