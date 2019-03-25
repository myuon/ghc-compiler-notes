module GHC.Compiler.Notes.Types where

import           Data.Sequence (Seq(..))
import           Data.Text     (Text)

import           SrcLoc

newtype NoteId = NoteId Text
  deriving (Eq, Show)

data Note = Note { noteId :: NoteId, noteContent :: Text }
  deriving (Eq, Show)

data CollectedNotes =
  CollectedNotes { notes :: Seq (Located Note), noteRefs :: Seq (Located NoteId) }

instance Semigroup CollectedNotes where
  ns1 <> ns2 =
    CollectedNotes { notes = notes ns1 <> notes ns2, noteRefs = noteRefs ns1 <> noteRefs ns2 }

instance Monoid CollectedNotes where
  mempty = CollectedNotes { notes = mempty, noteRefs = mempty }

addNoteByCollecting :: Located Note -> CollectedNotes -> CollectedNotes
addNoteByCollecting n ns = ns { notes = notes ns :|> n }

class HasSourceResourceGetter m where
  sourceResourceGetter :: FilePath -> Maybe SrcSpan -> m Text
