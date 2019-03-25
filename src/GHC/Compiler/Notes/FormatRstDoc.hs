{-# LANGUAGE OverloadedStrings #-}

module GHC.Compiler.Notes.FormatRstDoc
  ( formatRstDoc
  ) where

import qualified Data.Text                as Text
import           Data.List (groupBy)

import           GHC.Compiler.Notes.Types

import           SrcLoc

import           Control.Monad

formatRstDoc :: (HasSourceResourceGetter m, Monad m)
  => FilePath -> CollectedNotes -> m Text.Text
formatRstDoc targetFn CollectedNotes{..} = do
  fileNameLink <- sourceResourceGetter targetFn Nothing
  let textTargetFn = Text.pack targetFn
  contentHeader <- pure $ Text.unlines
    [
      -- Link to source
      "`[source] <" <> fileNameLink <> ">`_",
      "",
      -- Filename header
      textTargetFn,
      Text.replicate (Text.length textTargetFn) "="
    ]
  foldM combineNotes contentHeader notes
  where
    combineNotes txt (L p Note{..}) = do
      let NoteId noteId' = noteId
      let noteTitle = "Note [" <> noteId' <> "]"
      noteLink <- sourceResourceGetter targetFn $ Just p
      pure $ Text.unlines
        [
          txt,
          "",
          noteTitle,
          Text.replicate (Text.length noteTitle) "~",
          "",
          "`[note link] <" <> noteLink <> ">`__",
          "",
          codeBlocks noteContent
        ]

codeBlocks :: Text.Text -> Text.Text
codeBlocks = Text.concat . map Text.unlines . map (\p -> if detectBlocks p then insertCodeBlock p else p) . paragraphs
  where
    paragraphs = groupBy (\x y -> Text.stripStart x /= "" && Text.stripStart y /= "") . Text.lines

    detectBlocks =
      all (\line ->
        -- A code block should not be an empty line
        Text.length (Text.stripStart line) /= 0 &&
        -- A code block should be indented
        " " `Text.isPrefixOf` line &&
        -- A code block should not be start with numbers (that's probably an ordered list)
        Text.head (Text.stripStart line) `notElem` ['1'..'9'] &&
        -- A code block should not be start with `* ` nor `- ` (that's probably a list)
        not (Text.isPrefixOf "* " (Text.stripStart line)) &&
        not (Text.isPrefixOf "- " (Text.stripStart line))
        )

    insertCodeBlock = ("::\n" :)
