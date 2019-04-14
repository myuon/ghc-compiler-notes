{-# LANGUAGE OverloadedStrings #-}

module GHC.Compiler.Notes.FormatRstDoc
  ( formatRstDoc
  ) where

import           Control.Monad

import           Data.List                (groupBy)
import qualified Data.Text                as Text

import           GHC.Compiler.Notes.Types

import           SrcLoc

formatRstDoc :: (HasSourceResourceGetter m, Monad m) => FilePath -> CollectedNotes -> m Text.Text
formatRstDoc targetFn CollectedNotes{..} = do
  fileNameLink <- sourceResourceGetter targetFn Nothing
  let textTargetFn = Text.pack targetFn
  contentHeader <- pure $ Text.unlines   -- Link to source
    [ "`[source] <" <> fileNameLink <> ">`_"
    , ""
      -- Filename header
    , textTargetFn
    , Text.replicate (Text.length textTargetFn) "="
    ]
  foldM combineNotes contentHeader notes
  where
    combineNotes txt (L p Note{..}) = do
      let NoteId noteId' = noteId
      let noteTitle = "Note [" <> noteId' <> "]"
      noteLink <- sourceResourceGetter targetFn $ Just p
      pure $ Text.unlines [ txt
                          , ""
                          , noteTitle
                          , Text.replicate (Text.length noteTitle) "~"
                          , ""
                          , "`[note link] <" <> noteLink <> ">`__"
                          , ""
                          , codeBlocks noteContent
                          ]

codeBlocks :: Text.Text -> Text.Text
codeBlocks = Text.concat . map Text.unlines
  . map (\p -> if detectBlocks p then wrapCodeBlock p else p) . paragraphs
  where
    paragraphs      = groupBy (\x y -> Text.stripStart x /= "" && Text.stripStart y /= "")
      . Text.lines

    detectBlocks    =
      all (\line ->
           -- A code block should not be empty
           Text.length (Text.stripStart line) /= 0 &&
           -- A code block should be indented
           " " `Text.isPrefixOf` line &&
           -- A code block at least contains code words (word starting symbols) > 30%
           (let codeWordSize = length $ filter codeWord $ tokenize line
                wordSize = length $ tokenize line
            in fromIntegral codeWordSize / fromIntegral wordSize > 0.3) &&
           -- A code block should not be an ordered list
           all (not . (`Text.isPrefixOf` Text.stripStart line)) ["(1", "(2", "(3", "(4"])

    tokenize t      = Text.words t

    codeWord t      = Text.head t `elem` ("!\"#$%&'()-=~^\\|@`[{;+:*]}<,>/?_" :: String)

    wrapCodeBlock p = ("::\n" : p) ++ ["\n.."]
