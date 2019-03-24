{-# LANGUAGE OverloadedStrings #-}

module GHC.Compiler.Notes.FormatRstDoc
  ( formatRstDoc
  , unLoc
  ) where

import           Data.Foldable
import qualified Data.Text                as Text

import           GHC.Compiler.Notes.Types

import           SrcLoc

formatRstDoc :: CollectedNotes -> Text.Text
formatRstDoc CollectedNotes{..} = go $ toList $ noteContent . unLoc
  <$> notes
  where
    go []     = ""
    go [n]    = n
    go (n:ns) = n <> "\n\n" <> go ns
