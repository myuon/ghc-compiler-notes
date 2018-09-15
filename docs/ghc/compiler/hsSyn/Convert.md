[[src]](https://github.com/ghc/ghc/tree/master/compiler/hsSyn/Convert.hs)

(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998


This module converts Template Haskell syntax into HsSyn


# NOINLINE"
             src TH.Inline    = "{-# INLINE"
             src TH.Inlinable = "{-# INLINABLE"
       ; let ip   = InlinePragma { inl_src    = SourceText $ src inline
                                 , inl_inline = cvtInline inline
                                 , inl_rule   = cvtRuleMatch rm
                                 , inl_act    = cvtPhases phases dflt
                                 , inl_sat    = Nothing }
       ; returnJustL $ Hs.SigD $ InlineSig nm' ip }

cvtPragmaD (SpecialiseP nm ty inline phases)
  = do { nm' <- vNameL nm
       ; ty' <- cvtType ty
       ; let src TH.NoInline  = "{-# SPECIALISE NOINLINE"
             src TH.Inline    = "{-# SPECIALISE INLINE"
             src TH.Inlinable = "{-# SPECIALISE INLINE"
       ; let (inline', dflt,srcText) = case inline of
               Just inline1 -> (cvtInline inline1, dfltActivation inline1,
                                src inline1)
               Nothing      -> (NoUserInline,   AlwaysActive,
                                "{-# SPECIALISE")
       ; let ip = InlinePragma { inl_src    = SourceText srcText
                               , inl_inline = inline'
                               , inl_rule   = Hs.FunLike
                               , inl_act    = cvtPhases phases dflt
                               , inl_sat    = Nothing }
       ; returnJustL $ Hs.SigD $ SpecSig nm' [mkLHsSigType ty'] ip }

cvtPragmaD (SpecialiseInstP ty)
  = do { ty' <- cvtType ty
       ; returnJustL $ Hs.SigD $
         SpecInstSig (SourceText "{-# SPECIALISE") (mkLHsSigType ty') }

cvtPragmaD (RuleP nm bndrs lhs rhs phases)
  = do { let nm' = mkFastString nm
       ; let act = cvtPhases phases AlwaysActive
       ; bndrs' <- mapM cvtRuleBndr bndrs
       ; lhs'   <- cvtl lhs
       ; rhs'   <- cvtl rhs
       ; returnJustL $ Hs.RuleD
            $ HsRules (SourceText "{-# RULES")
                      [noLoc $ HsRule (noLoc (SourceText nm,nm')) act bndrs'
                                                  lhs' placeHolderNames
                                                  rhs' placeHolderNames]
       }

cvtPragmaD (AnnP target exp)
  = do { exp' <- cvtl exp
       ; target' <- case target of
         ModuleAnnotation  -> return ModuleAnnProvenance
         TypeAnnotation n  -> do
           n' <- tconName n
           return (TypeAnnProvenance  (noLoc n'))
         ValueAnnotation n -> do
           n' <- vcName n
           return (ValueAnnProvenance (noLoc n'))
       ; returnJustL $ Hs.AnnD $ HsAnnotation (SourceText "{-# ANN") target'
                                               exp'
       }

cvtPragmaD (LineP line file)
  = do { setL (srcLocSpan (mkSrcLoc (fsLit file) line 1))
       ; return Nothing
       }
cvtPragmaD (CompleteP cls mty)
  = do { cls' <- noLoc <$> mapM cNameL cls
       ; mty'  <- traverse tconNameL mty
       ; returnJustL $ Hs.SigD
                   $ CompleteMatchSig NoSourceText cls' mty' }

dfltActivation :: TH.Inline -> Activation
dfltActivation TH.NoInline = NeverActive
dfltActivation _           = AlwaysActive

cvtInline :: TH.Inline -> Hs.InlineSpec
cvtInline TH.NoInline  = Hs.NoInline
cvtInline TH.Inline    = Hs.Inline
cvtInline TH.Inlinable = Hs.Inlinable

cvtRuleMatch :: TH.RuleMatch -> RuleMatchInfo
cvtRuleMatch TH.ConLike = Hs.ConLike
cvtRuleMatch TH.FunLike = Hs.FunLike

cvtPhases :: TH.Phases -> Activation -> Activation
cvtPhases AllPhases       dflt = dflt
cvtPhases (FromPhase i)   _    = ActiveAfter NoSourceText i
cvtPhases (BeforePhase i) _    = ActiveBefore NoSourceText i

cvtRuleBndr :: TH.RuleBndr -> CvtM (Hs.LRuleBndr GhcPs)
cvtRuleBndr (RuleVar n)
  = do { n' <- vNameL n
       ; return $ noLoc $ Hs.RuleBndr n' }
cvtRuleBndr (TypedRuleVar n ty)
  = do { n'  <- vNameL n
       ; ty' <- cvtType ty
       ; return $ noLoc $ Hs.RuleBndrSig n' $ mkLHsSigWcType ty' }

---------------------------------------------------
--              Declarations
---------------------------------------------------

cvtLocalDecs :: MsgDoc -> [TH.Dec] -> CvtM (HsLocalBinds GhcPs)
cvtLocalDecs doc ds
  | null ds
  = return EmptyLocalBinds
  | otherwise
  = do { ds' <- cvtDecs ds
       ; let (binds, prob_sigs) = partitionWith is_bind ds'
       ; let (sigs, bads) = partitionWith is_sig prob_sigs
       ; unless (null bads) (failWith (mkBadDecMsg doc bads))
       ; return (HsValBinds (ValBindsIn (listToBag binds) sigs)) }

cvtClause :: HsMatchContext RdrName
          -> TH.Clause -> CvtM (Hs.LMatch GhcPs (LHsExpr GhcPs))
cvtClause ctxt (Clause ps body wheres)
  = do  { ps' <- cvtPats ps
        ; pps <- mapM wrap_conpat ps'
        ; g'  <- cvtGuard body
        ; ds' <- cvtLocalDecs (text "a where clause") wheres
        ; returnL $ Hs.Match ctxt pps (GRHSs g' (noLoc ds')) }


-------------------------------------------------------------------
--              Expressions
-------------------------------------------------------------------

### Note: Dropping constructors

### Note: Operator association

### Note: Dropping constructors

### Note: Converting UInfix

    cvt (ParensE e)      = do { e' <- cvtl e; return $ HsPar e' }
    cvt (SigE e t)       = do { e' <- cvtl e; t' <- cvtType t
                              ; return $ ExprWithTySig e' (mkLHsSigWcType t') }
    cvt (RecConE c flds) = do { c' <- cNameL c
                              ; flds' <- mapM (cvtFld (mkFieldOcc . noLoc)) flds
                              ; return $ mkRdrRecordCon c' (HsRecFields flds' Nothing) }
    cvt (RecUpdE e flds) = do { e' <- cvtl e
                              ; flds'
                                  <- mapM (cvtFld (mkAmbiguousFieldOcc . noLoc))
                                           flds
                              ; return $ mkRdrRecordUpd e' flds' }
    cvt (StaticE e)      = fmap (HsStatic placeHolderNames) $ cvtl e
    cvt (UnboundVarE s)  = do { s' <- vName s; return $ HsVar (noLoc s') }
    cvt (LabelE s)       = do { return $ HsOverLabel Nothing (fsLit s) }

### Note: Dropping constructors

When we drop constructors from the input (for instance, when we encounter @TupE [e]@)
we must insert parentheses around the argument. Otherwise, @UInfix@ constructors in @e@
could meet @UInfix@ constructors containing the @TupE [e]@. For example:

  UInfixE x * (TupE [UInfixE y + z])

If we drop the singleton tuple but don't insert parentheses, the @UInfixE@s would meet
and the above expression would be reassociated to

  OpApp (OpApp x * y) + z

which we don't want.


### Note: Operator assocation

### Note: Converting UInfix

When converting @UInfixE@, @UInfixP@, and @UInfixT@ values, we want to readjust
the trees to reflect the fixities of the underlying operators:

  UInfixE x * (UInfixE y + z) ---> (x * y) + z

This is done by the renamer (see @mkOppAppRn@, @mkConOppPatRn@, and
@mkHsOpTyRn@ in RnTypes), which expects that the input will be completely
right-biased for types and left-biased for everything else. So we left-bias the
trees of @UInfixP@ and @UInfixE@ and use HsAppsTy for UInfixT.

Sample input:

  UInfixE
   (UInfixE x op1 y)
   op2
   (UInfixE z op3 w)

Sample output:

  OpApp
    (OpApp
      (OpApp x op1 y)
      op2
      z)
    op3
    w

The functions @cvtOpApp@, @cvtOpAppP@, and @cvtOpAppT@ are responsible for this
biasing.


 | @cvtOpApp x op y@ converts @op@ and @y@ and produces the operator application @x `op` y@.
The produced tree of infix expressions will be left-biased, provided @x@ is.

We can see that @cvtOpApp@ is correct as follows. The inductive hypothesis
is that @cvtOpApp x op y@ is left-biased, provided @x@ is. It is clear that
this holds for both branches (of @cvtOpApp@), provided we assume it holds for
the recursive calls to @cvtOpApp@.

When we call @cvtOpApp@ from @cvtl@, the first argument will always be left-biased
since we have already run @cvtl@ on it.


### Note: Converting strings

If we get (ListE [CharL 'x', CharL 'y']) we'd like to convert to
a string literal for "xy".  Of course, we might hope to get
(LitE (StringL "xy")), but not always, and allCharLs fails quickly
if it isn't a literal string


 | @cvtOpAppP x op y@ converts @op@ and @y@ and produces the operator application @x `op` y@.
The produced tree of infix patterns will be left-biased, provided @x@ is.

See the @cvtOpApp@ documentation for how this function works.



The hsSyn representation of parsed source explicitly contains all the original
parens, as written in the source.

When a Template Haskell (TH) splice is evaluated, the original splice is first
renamed and type checked and then finally converted to core in DsMeta. This core
is then run in the TH engine, and the result comes back as a TH AST.

In the process, all parens are stripped out, as they are not needed.

This Convert module then converts the TH AST back to hsSyn AST.

In order to pretty-print this hsSyn AST, parens need to be adde back at certain
points so that the code is readable with its original meaning.

So scattered through Convert.hs are various points where parens are added.

See (among other closed issued) https://ghc.haskell.org/trac/ghc/ticket/14289


 | @cvtOpAppT x op y@ takes converted arguments and flattens any HsAppsTy
   structure in them.


### Note: Binders in Template Haskell

Consider this TH term construction:
  do { x1 <- TH.newName "x"   -- newName :: String -> Q TH.Name
     ; x2 <- TH.newName "x"   -- Builds a NameU
     ; x3 <- TH.newName "x"

     ; let x = mkName "x"     -- mkName :: String -> TH.Name
                              -- Builds a NameS

     ; return (LamE (..pattern [x1,x2]..) $
               LamE (VarPat x3) $
               ..tuple (x1,x2,x3,x)) }

It represents the term   \[x1,x2]. \x3. (x1,x2,x3,x)

a) We don't want to complain about "x" being bound twice in
   the pattern [x1,x2]
b) We don't want x3 to shadow the x1,x2
c) We *do* want 'x' (dynamically bound with mkName) to bind
   to the innermost binding of "x", namely x3.
d) When pretty printing, we want to print a unique with x1,x2
   etc, else they'll all print as "x" which isn't very helpful

### Note: Collect binders only after renaming

   - But to achieve (a) we must distinguish between the Exact
     RdrNames arising from TH and the Unqual RdrNames that would
     come from a user writing \[x,x] -> blah

So in Convert.thRdrName we translate
   TH Name                          RdrName
   --------------------------------------------------------
   NameU (arising from newName) --> Exact (Name{ System })
   NameS (arising from mkName)  --> Unqual

Notice that the NameUs generate *System* Names.  Then, when
figuring out shadowing and duplicates, we can filter out
System Names.

This use of System Names fits with other uses of System Names, eg for
temporary variables "a". Since there are lots of things called "a" we
usually want to print the name with the unique, and that is indeed
the way System Names are printed.

There's a small complication of course; see Note [Looking up Exact
RdrNames] in RnEnv.


### Note: Pattern synonym type signatures and Template Haskell


In general, the type signature of a pattern synonym

  pattern P x1 x2 .. xn = <some-pattern>

is of the form

   forall univs. reqs => forall exis. provs => t1 -> t2 -> ... -> tn -> t

with the following parts:

   1) the (possibly empty lists of) universally quantified type
      variables `univs` and required constraints `reqs` on them.
   2) the (possibly empty lists of) existentially quantified type
      variables `exis` and the provided constraints `provs` on them.
   3) the types `t1`, `t2`, .., `tn` of the pattern synonym's arguments x1,
      x2, .., xn, respectively
   4) the type `t` of <some-pattern>, mentioning only universals from `univs`.

Due to the two forall quantifiers and constraint contexts (either of
which might be empty), pattern synonym type signatures are treated
specially in `deSugar/DsMeta.hs`, `hsSyn/Convert.hs`, and
`typecheck/TcSplice.hs`:

   (a) When desugaring a pattern synonym from HsSyn to TH.Dec in
       `deSugar/DsMeta.hs`, we represent its *full* type signature in TH, i.e.:

           ForallT univs reqs (ForallT exis provs ty)
              (where ty is the AST representation of t1 -> t2 -> ... -> tn -> t)

   (b) When converting pattern synonyms from TH.Dec to HsSyn in
       `hsSyn/Convert.hs`, we convert their TH type signatures back to an
       appropriate Haskell pattern synonym type of the form

         forall univs. reqs => forall exis. provs => t1 -> t2 -> ... -> tn -> t

       where initial empty `univs` type variables or an empty `reqs`
       constraint context are represented *explicitly* as `() =>`.

   (c) When reifying a pattern synonym in `typecheck/TcSplice.hs`, we always
       return its *full* type, i.e.:

           ForallT univs reqs (ForallT exis provs ty)
              (where ty is the AST representation of t1 -> t2 -> ... -> tn -> t)

The key point is to always represent a pattern synonym's *full* type
in cases (a) and (c) to make it clear which of the two forall
quantifiers and/or constraint contexts are specified, and which are
not. See GHC's user's guide on pattern synonyms for more information
about pattern synonym type signatures.

