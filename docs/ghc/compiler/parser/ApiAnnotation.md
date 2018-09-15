[[src]](https://github.com/ghc/ghc/tree/master/compiler/parser/ApiAnnotation.hs)
### Note: Api annotations

Given a parse tree of a Haskell module, how can we reconstruct
the original Haskell source code, retaining all whitespace and
source code comments?  We need to track the locations of all
elements from the original source: this includes keywords such as
'let' / 'in' / 'do' etc as well as punctuation such as commas and
braces, and also comments.  We collectively refer to this
metadata as the "API annotations".

Rather than annotate the resulting parse tree with these locations
directly (this would be a major change to some fairly core data
structures in GHC), we instead capture locations for these elements in a
structure separate from the parse tree, and returned in the
pm_annotations field of the ParsedModule type.

The full ApiAnns type is

> type ApiAnns = ( Map.Map ApiAnnKey [SrcSpan]                  -- non-comments
>                , Map.Map SrcSpan [Located AnnotationComment]) -- comments

NON-COMMENT ELEMENTS

Intuitively, every AST element directly contains a bag of keywords
(keywords can show up more than once in a node: a semicolon i.e. newline
can show up multiple times before the next AST element), each of which
needs to be associated with its location in the original source code.

Consequently, the structure that records non-comment elements is logically
a two level map, from the SrcSpan of the AST element containing it, to
a map from keywords ('AnnKeyWord') to all locations of the keyword directly
in the AST element:

> type ApiAnnKey = (SrcSpan,AnnKeywordId)
>
> Map.Map ApiAnnKey [SrcSpan]

So

> let x = 1 in 2 *x

would result in the AST element

  L span (HsLet (binds for x = 1) (2 * x))

and the annotations

  (span,AnnLet) having the location of the 'let' keyword
  (span,AnnEqual) having the location of the '=' sign
  (span,AnnIn)  having the location of the 'in' keyword

For any given element in the AST, there is only a set number of
keywords that are applicable for it (e.g., you'll never see an
'import' keyword associated with a let-binding.)  The set of allowed
keywords is documented in a comment associated with the constructor
of a given AST element, although the ground truth is in Parser
and RdrHsSyn (which actually add the annotations; see #13012).

COMMENT ELEMENTS

Every comment is associated with a *located* AnnotationComment.
We associate comments with the lowest (most specific) AST element
enclosing them:

> Map.Map SrcSpan [Located AnnotationComment]

PARSER STATE

There are three fields in PState (the parser state) which play a role
with annotations.

>  annotations :: [(ApiAnnKey,[SrcSpan])],
>  comment_q :: [Located AnnotationComment],
>  annotations_comments :: [(SrcSpan,[Located AnnotationComment])]

The 'annotations' and 'annotations_comments' fields are simple: they simply
accumulate annotations that will end up in 'ApiAnns' at the end
(after they are passed to Map.fromList).

The 'comment_q' field captures comments as they are seen in the token stream,
so that when they are ready to be allocated via the parser they are
available (at the time we lex a comment, we don't know what the enclosing
AST node of it is, so we can't associate it with a SrcSpan in
annotations_comments).

PARSER EMISSION OF ANNOTATIONS

The parser interacts with the lexer using the function

> addAnnotation :: SrcSpan -> AnnKeywordId -> SrcSpan -> P ()

which takes the AST element SrcSpan, the annotation keyword and the
target SrcSpan.

This adds the annotation to the `annotations` field of `PState` and
transfers any comments in `comment_q` WHICH ARE ENCLOSED by
the SrcSpan of this element to the `annotations_comments`
field.  (Comments which are outside of this annotation are deferred
until later. 'allocateComments' in 'Lexer' is responsible for
making sure we only attach comments that actually fit in the 'SrcSpan'.)

The wiki page describing this feature is
https://ghc.haskell.org/trac/ghc/wiki/ApiAnnotations



\# LANGUAGE' etc
    | AnnOpenB   -- ^ '(|'
    | AnnOpenBU  -- ^ '(|', unicode variant
    | AnnOpenC   -- ^ '{'
    | AnnOpenE   -- ^ '[e|' or '[e||'
    | AnnOpenEQ  -- ^ '[|'
    | AnnOpenEQU -- ^ '[|', unicode variant
    | AnnOpenP   -- ^ '('
    | AnnOpenPE  -- ^ '$('
    | AnnOpenPTE -- ^ '$$('
    | AnnOpenS   -- ^ '['
    | AnnPackageName
    | AnnPattern
    | AnnProc
    | AnnQualified
    | AnnRarrow -- ^ '->'
    | AnnRarrowU -- ^ '->', unicode variant
    | AnnRec
    | AnnRole
    | AnnSafe
    | AnnSemi -- ^ ';'
    | AnnSimpleQuote -- ^ '''
    | AnnSignature
    | AnnStatic -- ^ 'static'
    | AnnStock
    | AnnThen
    | AnnThIdSplice -- ^ '$'
    | AnnThIdTySplice -- ^ '$$'
    | AnnThTyQuote -- ^ double '''
    | AnnTilde -- ^ '~'
    | AnnTildehsh -- ^ '~#'
    | AnnType
    | AnnUnit -- ^ '()' for types
    | AnnUsing
    | AnnVal  -- ^ e.g. INTEGER
    | AnnValStr  -- ^ String value, will need quotes when output
    | AnnVbar -- ^ '|'
    | AnnWhere
    | Annlarrowtail -- ^ '-<'
    | AnnlarrowtailU -- ^ '-<', unicode variant
    | Annrarrowtail -- ^ '->'
    | AnnrarrowtailU -- ^ '->', unicode variant
    | AnnLarrowtail -- ^ '-<<'
    | AnnLarrowtailU -- ^ '-<<', unicode variant
    | AnnRarrowtail -- ^ '>>-'
    | AnnRarrowtailU -- ^ '>>-', unicode variant
    | AnnEofPos
    deriving (Eq, Ord, Data, Show)

instance Outputable AnnKeywordId where
  ppr x = text (show x)

-- ---------------------------------------------------------------------

data AnnotationComment =
  -- Documentation annotations
    AnnDocCommentNext  String     -- ^ something beginning '-- |'
  | AnnDocCommentPrev  String     -- ^ something beginning '-- ^'
  | AnnDocCommentNamed String     -- ^ something beginning '-- $'
  | AnnDocSection      Int String -- ^ a section heading
  | AnnDocOptions      String     -- ^ doc options (prune, ignore-exports, etc)
  | AnnLineComment     String     -- ^ comment starting by "--"
  | AnnBlockComment    String     -- ^ comment in {- 