{-# LANGUAGE OverloadedStrings #-}

module GHC.Compiler.Notes.Parser.Internal where

import           SrcLoc
import           GHC.Compiler.Notes.Types
import           GHC.Compiler.Utils.Lexer
import           Data.Conduit
import           Lexer (Token(..))
import           Control.Monad.Trans.State
import qualified Data.Text as Text
import           Control.Monad.Trans.Class
import qualified Data.Char as Char
import qualified Text.Regex.Applicative as Regex
import           Control.Applicative
import           Data.Coerce
import           Data.Monoid
import qualified Data.Text.Monadic as Text
import           Control.Monad


data CollectedNotesCtx = CollectedNotesCtx
  { cnCtxCollectedNotes :: CollectedNotes
  , cnCtxCommentIndent  :: Int
  }

initialCollectedNotesCtx :: CollectedNotesCtx
initialCollectedNotesCtx = CollectedNotesCtx
  { cnCtxCollectedNotes = mempty
  , cnCtxCommentIndent = -1
  }

addNoteByCollectingInState :: Monad m => Located Note -> StateT CollectedNotesCtx m ()
addNoteByCollectingInState n = modify' \ctx -> ctx
  { cnCtxCollectedNotes = addNoteByCollecting n $ cnCtxCollectedNotes ctx
  }

type CollectNotesStateParser o
  = StateT CollectedNotesCtx (ConduitT (Located Token) o Parser)


type ParsingCtx = Located Text.Text

addBufferByCollecting :: Bool -> Located String -> ParsingCtx -> ParsingCtx
addBufferByCollecting b (L p2 s) (L p1 t) = L (combineSrcSpans p1 p2)
  $ t <> ls <> Text.pack s <> if b then "\n" else ""
  where
    n = betweenLineCount p1 p2

    ls = Text.replicate n "\n"

betweenLineCount :: SrcSpan -> SrcSpan -> Int
betweenLineCount (RealSrcSpan sp1) (RealSrcSpan sp2)
  = srcSpanEndLine sp1 - srcSpanEndLine sp2 - 1
betweenLineCount _ _ = 0

stripIndentedLineComment :: Monad m
  => Located String -> StateT CollectedNotesCtx m (Located String)
stripIndentedLineComment (L p s) = case s of
  '-':'-':ns -> do
    m <- fmap cnCtxCommentIndent get
    go m 0 ns
  _          -> error "unreachable"
  where
    go !m !n ns = case ns of
      _ | m == n -> goEnd n ns
      ' ':ns'    -> go m (n + 1) ns'
      []         -> goEnd m ns
      _          -> goEnd n ns

    goEnd n ns = do
      modify' \ctx -> ctx
        { cnCtxCommentIndent = n
        }
      pure $ L p ns

endLineCommentContent :: Monad m => StateT CollectedNotesCtx m ()
endLineCommentContent = modify' \ctx -> ctx
  { cnCtxCommentIndent = -1
  }

initialParsingCtx :: Bool -> Located String -> ParsingCtx
initialParsingCtx b (L p s) = L p $ Text.pack s <> if b then "\n" else ""

isNoteStartLineComment :: Located String -> CollectNotesStateParser o (Maybe (Located String))
isNoteStartLineComment s = do
  ns <- stripIndentedLineComment s
  let ~m = pure Nothing
  case Regex.match noteTitleMatcher $ unLoc ns of
    Nothing -> m
    Just{}  -> lift await >>= \case
      Nothing -> m
      Just t -> do
        lift $ leftover t
        case t of
          L p (ITlineComment s2)
            |  isTopLevelSrcLoc $ srcSpanStart p
            -> do
              ns2 <- stripIndentedLineComment $ L p s2
              case Regex.match noteTitleSectionLineMatcher $ unLoc ns2 of
                Nothing -> m
                Just{}  -> pure $ Just ns
          _ -> m

noteTitleMatcher :: Regex.RE Char String
noteTitleMatcher = "Note [" *> Regex.few Regex.anySym <* "]" <* spacesMatcher

noteTitleSectionLineMatcher :: Regex.RE Char String
noteTitleSectionLineMatcher = lineMatcher <* spacesMatcher
  where
    lineMatcher = getAlt $ mconcat $ coerce $ some . Regex.sym <$>
      [ '='
      , '-'
      , '`'
      , ':'
      , '.'
      , '\''
      , '"'
      , '~'
      , '^'
      , '_'
      -- , '*' -- for sub section
      , '+'
      , '#'
      ]

spacesMatcher :: Regex.RE Char String
spacesMatcher = many $ Regex.psym Char.isSpace

sinkWaitingNoteComment :: CollectNotesStateParser o ()
sinkWaitingNoteComment = lift await >>= \case
  Nothing -> pure ()
  Just (L p t) ->
    let
      ~m = do
        endLineCommentContent
        sinkWaitingNoteComment
    in case isTopLevelSrcLoc $ srcSpanStart p of
      False -> m
      True  -> case t of
        ITlineComment  s -> isNoteStartLineComment (L p s) >>= \case
          Just ns -> sinkParsingNoteComment $ initialParsingCtx True ns
          Nothing -> sinkWaitingNoteComment
        ITblockComment s -> parseBlockComment Nothing $ L p s
        _                -> m

parseBlockComment :: Maybe ParsingCtx -> Located String -> CollectNotesStateParser o ()
parseBlockComment mctx s
  | isPragmaBlockComment $ unLoc s = case mctx of
    Nothing  -> cont mctx
    Just ctx -> do
      completeParsingNote ctx
      cont Nothing
  | otherwise = runConduit
    $ sourceBlockCommentLines s .| sinkBlockCommentContents cont mctx
  where
    cont Nothing    = sinkWaitingNoteComment
    cont (Just ctx) = sinkParsingNoteComment ctx

isPragmaBlockComment :: String -> Bool
isPragmaBlockComment = \case
  '{':'-':'#':s -> go s
  _             -> False
  where
    go "#-}"    = True
    go []       = False
    go (_:ns)   = go ns

sourceBlockCommentLines :: Monad m => Located String -> ConduitT i (Located String) m ()
sourceBlockCommentLines (L p s) = case s of
  '{':'-':ns -> let l = srcSpanStart p in go l l id ns
  _          -> error "unreachable"
  where
    go sl !el chunk "-}" = yield $ L (mkSrcSpan sl el) $ chunk []
    go sl !el chunk (c:ns) =
      let
        el' = advanceSrcLocFromChar el c
        nchunk = chunk . (c:)
      in case c of
        '\n' -> do
          yield $ L (mkSrcSpan sl el) $ nchunk []
          go el' el' id ns
        _ -> go sl el' nchunk ns
    go _ _ _ [] = error "unreachable"

advanceSrcLocFromChar :: SrcLoc -> Char -> SrcLoc
advanceSrcLocFromChar (RealSrcLoc l) c = RealSrcLoc $ advanceSrcLoc l c
advanceSrcLocFromChar l              _ = l

sinkBlockCommentContents
  :: (Maybe ParsingCtx -> CollectNotesStateParser o2 ())
  -> Maybe ParsingCtx
  -> ConduitT (Located String) o (CollectNotesStateParser o2) ()
sinkBlockCommentContents cont = goStart
  where
    goStart mctx = await >>= \case
      Nothing -> lift $ cont mctx
      Just (L p s) -> do
        leftover $ L p $ dropWhile Char.isSpace s
        case mctx of
          Nothing  -> goWaitingNote
          Just ctx -> goParsingNote ctx

    goWaitingNote = await >>= \case
      Nothing -> lift $ cont Nothing
      Just s  -> isNoteStartLine s >>= \case
        Just ns -> goParsingNote $ initialParsingCtx False ns
        Nothing -> goWaitingNote

    goParsingNote ctx = await >>= \case
      Nothing -> lift $ cont $ Just ctx
      Just s  -> isNoteStartLine s >>= \case
        Just ns -> do
          lift $ completeParsingNote ctx
          goParsingNote $ initialParsingCtx False ns
        Nothing -> isSectionStartComment s >>= \case
          True -> do
            lift $ completeParsingNote ctx
            goWaitingNote
          False -> do
            let ns = s -- TODO: indentation parsing
            let nctx = addBufferByCollecting False ns ctx
            goParsingNote nctx

    isNoteStartLine s = do
      let ns = s -- TODO: indentation parsing
      let ~m = pure Nothing
      case Regex.match noteTitleMatcher $ unLoc ns of
        Nothing -> m
        Just{}  -> await >>= \case
          Nothing -> m
          Just s2 -> do
            leftover s2
            let ns2 = s2 -- TODO: indentation parsing
            case Regex.match noteTitleSectionLineMatcher $ unLoc ns2 of
              Nothing -> m
              Just{}  -> pure $ Just ns

    isSectionStartComment s = case Regex.match sectionLineMatcher $ unLoc s of
      Nothing -> pure False
      Just{}  -> await >>= \case
        Nothing -> pure False
        Just s2 -> do
          leftover s2
          case unLoc s2 of
            '*':_ -> pure True
            _     -> pure False

    sectionLineMatcher = some (Regex.sym '*') <* spacesMatcher

sinkParsingNoteComment :: ParsingCtx -> CollectNotesStateParser o ()
sinkParsingNoteComment ctx = lift await >>= \case
  Nothing -> pure ()
  Just (L p t) ->
    let
      ~m = do
        completeParsingNote ctx
        endLineCommentContent
        sinkWaitingNoteComment
    in case isTopLevelSrcLoc $ srcSpanStart p of
      False -> m
      True  -> case t of
        ITdocCommentNamed s ->
          let nctx = addBufferByCollecting True (removeNamedTag $ L p s) ctx
          in sinkParsingNoteComment nctx
        ITlineComment s -> isNoteStartLineComment (L p s) >>= \case
          Just ns -> do
            completeParsingNote ctx
            sinkParsingNoteComment $ initialParsingCtx True ns
          Nothing -> do
            ns <- stripIndentedLineComment $ L p s
            let nctx = addBufferByCollecting True ns ctx
            sinkParsingNoteComment nctx
        ITblockComment s -> parseBlockComment (Just ctx) $ L p s
        _ -> m
  where
    removeNamedTag (L p s) = removeNamedTag' (srcSpanStart p) (srcSpanEnd p) s

    removeNamedTag' sl el []    = L (mkSrcSpan sl el) []
    removeNamedTag' sl el (c:s) =
      let
        sl' = advanceSrcLocFromChar sl c
      in case c of
        '\n' -> L (mkSrcSpan sl' el) s
        _    -> removeNamedTag' sl' el s


completeParsingNote :: ParsingCtx -> CollectNotesStateParser o ()
completeParsingNote (L p buf) =
  addNoteByCollectingInState $ L p $ Note
    { noteId = noteIdFromBuf
    , noteContent = noteContentFromBuf
    }
  where
    noteIdFromBuf =
      let
        firstLine = Text.takeWhile (/= '\n') buf
      in case Regex.match noteTitleMatcher $ Text.unpack firstLine of
        Nothing    -> error "unreachable"
        Just title -> NoteId $ Text.pack title

    noteContentFromBuf =
      let
        skipTwoLine c = do
          i <- get
          if i >= 2
            then pure False
            else do
              when (c == '\n') do
                put $ i + 1
              pure True
      in Text.strip $ evalState (Text.dropWhileM skipTwoLine buf) (0 :: Int)

isTopLevelSrcLoc :: SrcLoc -> Bool
isTopLevelSrcLoc UnhelpfulLoc{} = False
isTopLevelSrcLoc (RealSrcLoc l) = srcLocCol l == 1
