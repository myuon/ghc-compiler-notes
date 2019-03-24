`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/parser/RdrHsSyn.hs>`_

Note [Parsing data constructors is hard]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The problem with parsing data constructors is that they look a lot like types.
Compare:

.. code-block:: haskell

  (s1)   data T = C t1 t2
  (s2)   type T = C t1 t2

Syntactically, there's little difference between these declarations, except in
(s1) 'C' is a data constructor, but in (s2) 'C' is a type constructor.

This similarity would pose no problem if we knew ahead of time if we are
parsing a type or a constructor declaration. Looking at (s1) and (s2), a simple
(but wrong!) rule comes to mind: in 'data' declarations assume we are parsing
data constructors, and in other contexts (e.g. 'type' declarations) assume we
are parsing type constructors.

This simple rule does not work because of two problematic cases:

.. code-block:: haskell

  (p1)   data T = C t1 t2 :+ t3
  (p2)   data T = C t1 t2 => t3

In (p1) we encounter (:+) and it turns out we are parsing an infix data
declaration, so (C t1 t2) is a type and 'C' is a type constructor.
In (p2) we encounter (=>) and it turns out we are parsing an existential
context, so (C t1 t2) is a constraint and 'C' is a type constructor.

As the result, in order to determine whether (C t1 t2) declares a data
constructor, a type, or a context, we would need unlimited lookahead which
'happy' is not so happy with.

To further complicate matters, the interpretation of (!) and (~) is different
in constructors and types:

.. code-block:: haskell

  (b1)   type T = C ! D
  (b2)   data T = C ! D
  (b3)   data T = C ! D => E

In (b1) and (b3), (!) is a type operator with two arguments: 'C' and 'D'. At
the same time, in (b2) it is a strictness annotation: 'C' is a data constructor
with a single strict argument 'D'. For the programmer, these cases are usually
easy to tell apart due to whitespace conventions:

.. code-block:: haskell

  (b2)   data T = C !D         -- no space after the bang hints that
                               -- it is a strictness annotation

For the parser, on the other hand, this whitespace does not matter. We cannot
tell apart (b2) from (b3) until we encounter (=>), so it requires unlimited
lookahead.

The solution that accounts for all of these issues is to initially parse data
declarations and types as a reversed list of TyEl:

.. code-block:: haskell

  data TyEl = TyElOpr RdrName
            | TyElOpd (HsType GhcPs)
            | TyElBang | TyElTilde
            | ...

For example, both occurences of (C ! D) in the following example are parsed
into equal lists of TyEl:

.. code-block:: haskell

  data T = C ! D => C ! D   results in   [ TyElOpd (HsTyVar "D")
                                         , TyElBang
                                         , TyElOpd (HsTyVar "C") ]

Note that elements are in reverse order. Also, 'C' is parsed as a type
constructor (HsTyVar) even when it is a data constructor. We fix this in
`tyConToDataCon`.

By the time the list of TyEl is assembled, we have looked ahead enough to
decide whether to reduce using `mergeOps` (for types) or `mergeDataCon` (for
data constructors). These functions are where the actual job of parsing is
done.



Note [setRdrNameSpace for wired-in names]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In GHC.Types, which declares (:), we have
  infixr 5 :
The ambiguity about which ":" is meant is resolved by parsing it as a
data constructor, but then using dataTcOccs to try the type constructor too;
and that in turn calls setRdrNameSpace to change the name-space of ":" to
tcClsName.  There isn't a corresponding ":" type constructor, but it's painful
to make setRdrNameSpace partial, so we just make an Unqual name instead. It
really doesn't matter!


Note [TyElKindApp SrcSpan interpretation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A TyElKindApp captures type application written in haskell as

.. code-block:: haskell

    @ Foo

where Foo is some type.

The SrcSpan reflects both elements, and there are AnnAt and AnnVal API
Annotations attached to this SrcSpan for the specific locations of
each within it.


Note [Impossible case in mergeOps clause [unpk]]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This case should never occur. Let us consider all possible
variations of 'acc', 'xs', and 'k':

  acc          xs        k
==============================
  null   |    null       0      -- "must be applied to a type"
  null   |  not null     0      -- "must be applied to a type"
not null |    null       0      -- successful parse
not null |  not null     0      -- "cannot appear inside a type"
  null   |    null      >0      -- handled in clause [opr]
  null   |  not null    >0      -- "cannot appear inside a type"
not null |    null      >0      -- successful parse
not null |  not null    >0      -- "cannot appear inside a type"

The (null acc && null xs && k>0) case is handled in clause [opr]
by the following check:

.. code-block:: haskell

    if ... || null (filter isTyElOpd xs)
     then failOpFewArgs (L l op)

We know that this check has been performed because k>0, and by
the time we reach the end of the list (null xs), the only way
for (null acc) to hold is that there was not a single TyElOpd
between the operator and the end of the list. But this case is
caught by the check and reported as 'failOpFewArgs'.


Note [Non-empty 'acc' in mergeOps clause [end]]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In clause [end] we need to know that 'acc' is non-empty to call 'mergeAcc'
without a check.

Running 'mergeOps' with an empty input list is forbidden, so we do not consider
this possibility. This means we'll hit at least one other clause before we
reach clause [end].

* Clauses [unpk] and [doc] do not call 'go' recursively, so we cannot hit
  clause [end] from there.
* Clause [opd] makes 'acc' non-empty, so if we hit clause [end] after it, 'acc'
  will be non-empty.
* Clause [opr] checks that (filter isTyElOpd xs) is not null - so we are going
  to hit clause [opd] at least once before we reach clause [end], making 'acc'
  non-empty.
* There are no other clauses.

Therefore, it is safe to omit a check for non-emptiness of 'acc' in clause
[end].



Note [isFunLhs vs mergeDataCon]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When parsing a function LHS, we do not know whether to treat (!) as
a strictness annotation or an infix operator:

.. code-block:: haskell

  f ! a = ...

Without -XBangPatterns, this parses as   (!) f a = ...
   with -XBangPatterns, this parses as   f (!a) = ...

So in function declarations we opted to always parse as if -XBangPatterns
were off, and then rejig in 'isFunLhs'.

There are two downsides to this approach:

1. It is not particularly elegant, as there's a point in our pipeline where
   the representation is awfully incorrect. For instance,
      f !a b !c = ...
   will be first parsed as
      (f ! a b) ! c = ...

2. There are cases that it fails to cover, for instance infix declarations:
      !a + !b = ...
   will trigger an error.

Unfortunately, we cannot define different productions in the 'happy' grammar
depending on whether -XBangPatterns are enabled.

When parsing data constructors, we face a similar issue:
  (a) data T1 = C ! D
  (b) data T2 = C ! D => ...

In (a) the first bang is a strictness annotation, but in (b) it is a type
operator. A 'happy'-based parser does not have unlimited lookahead to check for
=>, so we must first parse (C ! D) into a common representation.

If we tried to mirror the approach used in functions, we would parse both sides
of => as types, and then rejig. However, we take a different route and use an
intermediate data structure, a reversed list of 'TyEl'.
See Note [Parsing data constructors is hard] for details.

This approach does not suffer from the issues of 'isFunLhs':

1. A sequence of 'TyEl' is a dedicated intermediate representation, not an
   incorrectly parsed type. Therefore, we do not have confusing states in our
   pipeline. (Except for representing data constructors as type variables).

2. We can handle infix data constructors with strictness annotations:
    data T a b = !a :+ !b



Note [Ambiguous syntactic categories]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

There are places in the grammar where we do not know whether we are parsing an
expression or a pattern without unlimited lookahead (which we do not have in
'happy'):

View patterns:

.. code-block:: haskell

    f (Con a b     ) = ...  -- 'Con a b' is a pattern
    f (Con a b -> x) = ...  -- 'Con a b' is an expression

do-notation:

.. code-block:: haskell

    do { Con a b <- x } -- 'Con a b' is a pattern
    do { Con a b }      -- 'Con a b' is an expression

Guards:

.. code-block:: haskell

    x | True <- p && q = ...  -- 'True' is a pattern
    x | True           = ...  -- 'True' is an expression

Top-level value/function declarations (FunBind/PatBind):

.. code-block:: haskell

    f !a         -- TH splice
    f !a = ...   -- function declaration

.. code-block:: haskell

    Until we encounter the = sign, we don't know if it's a top-level
    TemplateHaskell splice where ! is an infix operator, or if it's a function
    declaration where ! is a strictness annotation.

There are also places in the grammar where we do not know whether we are
parsing an expression or a command:

.. code-block:: haskell

    proc x -> do { (stuff) -< x }   -- 'stuff' is an expression
    proc x -> do { (stuff) }        -- 'stuff' is a command

.. code-block:: haskell

    Until we encounter arrow syntax (-<) we don't know whether to parse 'stuff'
    as an expression or a command.

In fact, do-notation is subject to both ambiguities:

.. code-block:: haskell

    proc x -> do { (stuff) -< x }        -- 'stuff' is an expression
    proc x -> do { (stuff) <- f -< x }   -- 'stuff' is a pattern
    proc x -> do { (stuff) }             -- 'stuff' is a command

There are many possible solutions to this problem. For an overview of the ones
we decided against, see Note [Resolving parsing ambiguities: non-taken alternatives]

The solution that keeps basic definitions (such as HsExpr) clean, keeps the
concerns local to the parser, and does not require duplication of hsSyn types,
or an extra pass over the entire AST, is to parse into a function from a GADT
to a parser-validator:

.. code-block:: haskell

    data ExpCmdG b where
      ExpG :: ExpCmdG HsExpr
      CmdG :: ExpCmdG HsCmd

.. code-block:: haskell

    type ExpCmd = forall b. ExpCmdG b -> PV (Located (b GhcPs))

.. code-block:: haskell

    checkExp :: ExpCmd -> PV (LHsExpr GhcPs)
    checkCmd :: ExpCmd -> PV (LHsCmd GhcPs)
    checkExp f = f ExpG  -- interpret as an expression
    checkCmd f = f CmdG  -- interpret as a command

Consider the 'alts' production used to parse case-of alternatives:

.. code-block:: haskell

  alts :: { Located ([AddAnn],[LMatch GhcPs (LHsExpr GhcPs)]) }
    : alts1     { sL1 $1 (fst $ unLoc $1,snd $ unLoc $1) }
    | ';' alts  { sLL $1 $> ((mj AnnSemi $1:(fst $ unLoc $2)),snd $ unLoc $2) }

We abstract over LHsExpr, and it becomes:

.. code-block:: haskell

  alts :: { forall b. ExpCmdG b -> PV (Located ([AddAnn],[LMatch GhcPs (Located (b GhcPs))])) }
    : alts1
        { \tag -> $1 tag >>= \ $1 ->
                  return $ sL1 $1 (fst $ unLoc $1,snd $ unLoc $1) }
    | ';' alts
        { \tag -> $2 tag >>= \ $2 ->
                  return $ sLL $1 $> ((mj AnnSemi $1:(fst $ unLoc $2)),snd $ unLoc $2) }

Note that 'ExpCmdG' is a singleton type, the value is completely
determined by the type:

.. code-block:: haskell

  when (b~HsExpr),  tag = ExpG
  when (b~HsCmd),   tag = CmdG

This is a clear indication that we can use a class to pass this value behind
the scenes:

.. code-block:: haskell

  class    ExpCmdI b      where expCmdG :: ExpCmdG b
  instance ExpCmdI HsExpr where expCmdG = ExpG
  instance ExpCmdI HsCmd  where expCmdG = CmdG

And now the 'alts' production is simplified, as we no longer need to
thread 'tag' explicitly:

.. code-block:: haskell

  alts :: { forall b. ExpCmdI b => PV (Located ([AddAnn],[LMatch GhcPs (Located (b GhcPs))])) }
    : alts1     { $1 >>= \ $1 ->
                  return $ sL1 $1 (fst $ unLoc $1,snd $ unLoc $1) }
    | ';' alts  { $2 >>= \ $2 ->
                  return $ sLL $1 $> ((mj AnnSemi $1:(fst $ unLoc $2)),snd $ unLoc $2) }

Compared to the initial definition, the added bits are:

.. code-block:: haskell

    forall b. ExpCmdI b => PV ( ... ) -- in the type signature
    $1 >>= \ $1 -> return $           -- in one reduction rule
    $2 >>= \ $2 -> return $           -- in another reduction rule

The overhead is constant relative to the size of the rest of the reduction
rule, so this approach scales well to large parser productions.



Note [Resolving parsing ambiguities: non-taken alternatives]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Alternative I, extra constructors in HsExpr
-------------------------------------------
We could add extra constructors to HsExpr to represent command-specific and
pattern-specific syntactic constructs. Under this scheme, we parse patterns
and commands as expressions and rejig later.  This is what GHC used to do, and
it polluted 'HsExpr' with irrelevant constructors:

  * for commands: 'HsArrForm', 'HsArrApp'
  * for patterns: 'EWildPat', 'EAsPat', 'EViewPat', 'ELazyPat'

(As of now, we still do that for patterns, but we plan to fix it).

There are several issues with this:

  * The implementation details of parsing are leaking into hsSyn definitions.

  * Code that uses HsExpr has to panic on these impossible-after-parsing cases.

  * HsExpr is arbitrarily selected as the extension basis. Why not extend
    HsCmd or HsPat with extra constructors instead?

  * We cannot handle corner cases. For instance, the following function
    declaration LHS is not a valid expression (see #1087):

.. code-block:: haskell

      !a + !b = ...

  * There are points in the pipeline where the representation was awfully
    incorrect. For instance,

.. code-block:: haskell

      f !a b !c = ...

.. code-block:: haskell

    is first parsed as

.. code-block:: haskell

      (f ! a b) ! c = ...


Alternative II, extra constructors in HsExpr for GhcPs
------------------------------------------------------
We could address some of the problems with Alternative I by using Trees That
Grow and extending HsExpr only in the GhcPs pass. However, GhcPs corresponds to
the output of parsing, not to its intermediate results, so we wouldn't want
them there either.

Alternative III, extra constructors in HsExpr for GhcPrePs
----------------------------------------------------------
We could introduce a new pass, GhcPrePs, to keep GhcPs pristine.
Unfortunately, creating a new pass would significantly bloat conversion code
and slow down the compiler by adding another linear-time pass over the entire
AST. For example, in order to build HsExpr GhcPrePs, we would need to build
HsLocalBinds GhcPrePs (as part of HsLet), and we never want HsLocalBinds
GhcPrePs.


Alternative IV, sum type and bottom-up data flow
------------------------------------------------
Expressions and commands are disjoint. There are no user inputs that could be
interpreted as either an expression or a command depending on outer context:

  5        -- definitely an expression
  x -< y   -- definitely a command

Even though we have both 'HsLam' and 'HsCmdLam', we can look at
the body to disambiguate:

.. code-block:: haskell

  \p -> 5        -- definitely an expression
  \p -> x -< y   -- definitely a command

This means we could use a bottom-up flow of information to determine
whether we are parsing an expression or a command, using a sum type
for intermediate results:

.. code-block:: haskell

  Either (LHsExpr GhcPs) (LHsCmd GhcPs)

There are two problems with this:

  * We cannot handle the ambiguity between expressions and
    patterns, which are not disjoint.

  * Bottom-up flow of information leads to poor error messages. Consider

.. code-block:: haskell

        if ... then 5 else (x -< y)

.. code-block:: haskell

    Do we report that '5' is not a valid command or that (x -< y) is not a
    valid expression?  It depends on whether we want the entire node to be
    'HsIf' or 'HsCmdIf', and this information flows top-down, from the
    surrounding parsing context (are we in 'proc'?)

Alternative V, backtracking with parser combinators
---------------------------------------------------
One might think we could sidestep the issue entirely by using a backtracking
parser and doing something along the lines of (try pExpr <|> pPat).

Turns out, this wouldn't work very well, as there can be patterns inside
expressions (e.g. via 'case', 'let', 'do') and expressions inside patterns
(e.g. view patterns). To handle this, we would need to backtrack while
backtracking, and unbound levels of backtracking lead to very fragile
performance.

Alternative VI, an intermediate data type
-----------------------------------------
There are common syntactic elements of expressions, commands, and patterns
(e.g. all of them must have balanced parentheses), and we can capture this
common structure in an intermediate data type, Frame:

data Frame
  = FrameVar RdrName
    -- ^ Identifier: Just, map, BS.length
  | FrameTuple [LTupArgFrame] Boxity
    -- ^ Tuple (section): (a,b) (a,b,c) (a,,) (,a,)
  | FrameTySig LFrame (LHsSigWcType GhcPs)
    -- ^ Type signature: x :: ty
  | FramePar (SrcSpan, SrcSpan) LFrame
    -- ^ Parentheses
  | FrameIf LFrame LFrame LFrame
    -- ^ If-expression: if p then x else y
  | FrameCase LFrame [LFrameMatch]
    -- ^ Case-expression: case x of { p1 -> e1; p2 -> e2 }
  | FrameDo (HsStmtContext Name) [LFrameStmt]
    -- ^ Do-expression: do { s1; a <- s2; s3 }
  ...
  | FrameExpr (HsExpr GhcPs)   -- unambiguously an expression
  | FramePat (HsPat GhcPs)     -- unambiguously a pattern
  | FrameCommand (HsCmd GhcPs) -- unambiguously a command

To determine which constructors 'Frame' needs to have, we take the union of
intersections between HsExpr, HsCmd, and HsPat.

The intersection between HsPat and HsExpr:

.. code-block:: haskell

  HsPat  =  VarPat   | TuplePat      | SigPat        | ParPat   | ...
  HsExpr =  HsVar    | ExplicitTuple | ExprWithTySig | HsPar    | ...
  -------------------------------------------------------------------
  Frame  =  FrameVar | FrameTuple    | FrameTySig    | FramePar | ...

The intersection between HsCmd and HsExpr:

.. code-block:: haskell

  HsCmd  = HsCmdIf | HsCmdCase | HsCmdDo | HsCmdPar
  HsExpr = HsIf    | HsCase    | HsDo    | HsPar
  ------------------------------------------------
  Frame = FrameIf  | FrameCase | FrameDo | FramePar

The intersection between HsCmd and HsPat:

.. code-block:: haskell

  HsPat  = ParPat   | ...
  HsCmd  = HsCmdPar | ...
  -----------------------
  Frame  = FramePar | ...

Take the union of each intersection and this yields the final 'Frame' data
type. The problem with this approach is that we end up duplicating a good
portion of hsSyn:

.. code-block:: haskell

    Frame         for  HsExpr, HsPat, HsCmd
    TupArgFrame   for  HsTupArg
    FrameMatch    for  Match
    FrameStmt     for  StmtLR
    FrameGRHS     for  GRHS
    FrameGRHSs    for  GRHSs
    ...

Alternative VII, a product type
-------------------------------
We could avoid the intermediate representation of Alternative VI by parsing
into a product of interpretations directly:

.. code-block:: haskell

    -- See Note [Parser-Validator]
    type ExpCmdPat = ( PV (LHsExpr GhcPs)
                     , PV (LHsCmd GhcPs)
                     , PV (LHsPat GhcPs) )

This means that in positions where we do not know whether to produce
expression, a pattern, or a command, we instead produce a parser-validator for
each possible option.

Then, as soon as we have parsed far enough to resolve the ambiguity, we pick
the appropriate component of the product, discarding the rest:

.. code-block:: haskell

    checkExpOf3 (e, _, _) = e  -- interpret as an expression
    checkCmdOf3 (_, c, _) = c  -- interpret as a command
    checkPatOf3 (_, _, p) = p  -- interpret as a pattern

We can easily define ambiguities between arbitrary subsets of interpretations.
For example, when we know ahead of type that only an expression or a command is
possible, but not a pattern, we can use a smaller type:

.. code-block:: haskell

    -- See Note [Parser-Validator]
    type ExpCmd = (PV (LHsExpr GhcPs), PV (LHsCmd GhcPs))

.. code-block:: haskell

    checkExpOf2 (e, _) = e  -- interpret as an expression
    checkCmdOf2 (_, c) = c  -- interpret as a command

However, there is a slight problem with this approach, namely code duplication
in parser productions. Consider the 'alts' production used to parse case-of
alternatives:

.. code-block:: haskell

  alts :: { Located ([AddAnn],[LMatch GhcPs (LHsExpr GhcPs)]) }
    : alts1     { sL1 $1 (fst $ unLoc $1,snd $ unLoc $1) }
    | ';' alts  { sLL $1 $> ((mj AnnSemi $1:(fst $ unLoc $2)),snd $ unLoc $2) }

Under the new scheme, we have to completely duplicate its type signature and
each reduction rule:

.. code-block:: haskell

  alts :: { ( PV (Located ([AddAnn],[LMatch GhcPs (LHsExpr GhcPs)])) -- as an expression
            , PV (Located ([AddAnn],[LMatch GhcPs (LHsCmd GhcPs)]))  -- as a command
            ) }
    : alts1
        { ( checkExpOf2 $1 >>= \ $1 ->
            return $ sL1 $1 (fst $ unLoc $1,snd $ unLoc $1)
          , checkCmdOf2 $1 >>= \ $1 ->
            return $ sL1 $1 (fst $ unLoc $1,snd $ unLoc $1)
          ) }
    | ';' alts
        { ( checkExpOf2 $2 >>= \ $2 ->
            return $ sLL $1 $> ((mj AnnSemi $1:(fst $ unLoc $2)),snd $ unLoc $2)
          , checkCmdOf2 $2 >>= \ $2 ->
            return $ sLL $1 $> ((mj AnnSemi $1:(fst $ unLoc $2)),snd $ unLoc $2)
          ) }

And the same goes for other productions: 'altslist', 'alts1', 'alt', 'alt_rhs',
'ralt', 'gdpats', 'gdpat', 'exp', ... and so on. That is a lot of code!

-------------------------------------------------------------------------
 Miscellaneous utilities


Note [Parser-Validator]
~~~~~~~~~~~~~~~~~~~~~~~~~~

When resolving ambiguities, we need to postpone failure to make a choice later.
For example, if we have ambiguity between some A and B, our parser could be

.. code-block:: haskell

  abParser :: P (Maybe A, Maybe B)

This way we can represent four possible outcomes of parsing:

.. code-block:: haskell

    (Just a, Nothing)       -- definitely A
    (Nothing, Just b)       -- definitely B
    (Just a, Just b)        -- either A or B
    (Nothing, Nothing)      -- neither A nor B

However, if we want to report informative parse errors, accumulate warnings,
and add API annotations, we are better off using 'P' instead of 'Maybe':

.. code-block:: haskell

  abParser :: P (P A, P B)

So we have an outer layer of P that consumes the input and builds the inner
layer, which validates the input.

For clarity, we introduce the notion of a parser-validator: a parser that does
not consume any input, but may fail or use other effects. Thus we have:

.. code-block:: haskell

  abParser :: P (PV A, PV B)


