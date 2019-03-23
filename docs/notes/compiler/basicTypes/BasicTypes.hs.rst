Note [Precedence in types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Many pretty-printing functions have type
    ppr_ty :: PprPrec -> Type -> SDoc

The PprPrec gives the binding strength of the context.  For example, in
   T ty1 ty2
we will pretty-print 'ty1' and 'ty2' with the call
  (ppr_ty appPrec ty)
to indicate that the context is that of an argument of a TyConApp.

We use this consistently for Type and HsType.



Note [Type operator precedence]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We don't keep the fixity of type operators in the operator. So the
pretty printer follows the following precedence order:

   TyConPrec         Type constructor application
   TyOpPrec/FunPrec  Operator application and function arrow

We have funPrec and opPrec to represent the precedence of function
arrow and type operators respectively, but currently we implement
funPrec == opPrec, so that we don't distinguish the two. Reason:
it's hard to parse a type like
    a ~ b => c * d -> e - f

By treating opPrec = funPrec we end up with more parens
    (a ~ b) => (c * d) -> (e - f)

But the two are different constructors of PprPrec so we could make
(->) bind more or less tightly if we wanted.


Note [LoopBreaker OccInfo]
~~~~~~~~~~~~~~~~~~~~~~~~~~
   IAmALoopBreaker True  <=> A "weak" or rules-only loop breaker
                             Do not preInlineUnconditionally

   IAmALoopBreaker False <=> A "strong" loop breaker
                             Do not inline at all

See OccurAnal Note [Weak loop breakers]


Note [TailCallInfo]
~~~~~~~~~~~~~~~~~~~
The occurrence analyser determines what can be made into a join point, but it
doesn't change the binder into a JoinId because then it would be inconsistent
with the occurrences. Thus it's left to the simplifier (or to simpleOptExpr) to
change the IdDetails.

The AlwaysTailCalled marker actually means slightly more than simply that the
function is always tail-called. See Note [Invariants on join points].

This info is quite fragile and should not be relied upon unless the occurrence
analyser has *just* run. Use 'Id.isJoinId_maybe' for the permanent state of
the join-point-hood of a binder; a join id itself will not be marked
AlwaysTailCalled.

Note that there is a 'TailCallInfo' on a 'ManyOccs' value. One might expect that
being tail-called would mean that the variable could only appear once per branch
(thus getting a `OneOcc { occ_one_br = True }` occurrence info), but a join
point can also be invoked from other join points, not just from case branches:

  let j1 x = ...
      j2 y = ... j1 z {- tail call -} ...
  in case w of
       A -> j1 v
       B -> j2 u
       C -> j2 q

Here both 'j1' and 'j2' will get marked AlwaysTailCalled, but j1 will get
ManyOccs and j2 will get `OneOcc { occ_one_br = True }`.



Note [Pragma source text]
~~~~~~~~~~~~~~~~~~~~~~~~~
The lexer does a case-insensitive match for pragmas, as well as
accepting both UK and US spelling variants.

So

  {-# SPECIALISE #-}
  {-# SPECIALIZE #-}
  {-# Specialize #-}

will all generate ITspec_prag token for the start of the pragma.

In order to be able to do source to source conversions, the original
source text for the token needs to be preserved, hence the
`SourceText` field.

So the lexer will then generate

  ITspec_prag "{ -# SPECIALISE"
  ITspec_prag "{ -# SPECIALIZE"
  ITspec_prag "{ -# Specialize"

for the cases above.
 [without the space between '{' and '-', otherwise this comment won't parse]




Note [Literal source text]
~~~~~~~~~~~~~~~~~~~~~~~~~~
The lexer/parser converts literals from their original source text
versions to an appropriate internal representation. This is a problem
for tools doing source to source conversions, so the original source
text is stored in literals where this can occur.

Motivating examples for HsLit

  HsChar          '\n'       == '\x20`
  HsCharPrim      '\x41`#    == `A`
  HsString        "\x20\x41" == " A"
  HsStringPrim    "\x20"#    == " "#
  HsInt           001        == 1
  HsIntPrim       002#       == 2#
  HsWordPrim      003##      == 3##
  HsInt64Prim     004##      == 4##
  HsWord64Prim    005##      == 5##
  HsInteger       006        == 6

For OverLitVal

  HsIntegral      003      == 0x003
  HsIsString      "\x41nd" == "And"


Note [InlinePragma]
~~~~~~~~~~~~~~~~~~~~~~
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



Note [inl_inline and inl_act]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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



Note [CONLIKE pragma]
~~~~~~~~~~~~~~~~~~~~~
The ConLike constructor of a RuleMatchInfo is aimed at the following.
Consider first
    {-# RULE "r/cons" forall a as. r (a:as) = f (a+1) #-}
    g b bs = let x = b:bs in ..x...x...(r x)...
Now, the rule applies to the (r x) term, because GHC "looks through"
the definition of 'x' to see that it is (b:bs).

Now consider
    {-# RULE "r/f" forall v. r (f v) = f (v+1) #-}
    g v = let x = f v in ..x...x...(r x)...
Normally the (r x) would *not* match the rule, because GHC would be
scared about duplicating the redex (f v), so it does not "look
through" the bindings.

However the CONLIKE modifier says to treat 'f' like a constructor in
this situation, and "look through" the unfolding for x.  So (r x)
fires, yielding (f (v+1)).

This is all controlled with a user-visible pragma:
     {-# NOINLINE CONLIKE [1] f #-}

The main effects of CONLIKE are:

    - The occurrence analyser (OccAnal) and simplifier (Simplify) treat
      CONLIKE thing like constructors, by ANF-ing them

    - New function CoreUtils.exprIsExpandable is like exprIsCheap, but
      additionally spots applications of CONLIKE functions

    - A CoreUnfolding has a field that caches exprIsExpandable

    - The rule matcher consults this field.  See
      Note [Expanding variables] in Rules.hs.


Note [Competing activations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Sometimes a RULE and an inlining may compete, or two RULES.
See Note [Rules and inlining/other rules] in Desugar.

We say that act1 "competes with" act2 iff
   act1 is active in the phase when act2 *becomes* active
NB: remember that phases count *down*: 2, 1, 0!

It's too conservative to ensure that the two are never simultaneously
active.  For example, a rule might be always active, and an inlining
might switch on in phase 2.  We could switch off the rule, but it does
no harm.
