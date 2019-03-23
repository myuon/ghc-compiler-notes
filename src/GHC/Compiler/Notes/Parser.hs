module GHC.Compiler.Notes.Parser where

import           Capability.Reader

import           Control.Monad.IO.Class
import           Control.Monad.Trans.State

import           Data.Conduit

import           DynFlags                           (DynFlags)

import           GHC.Compiler.Notes.Parser.Internal
import           GHC.Compiler.Notes.Types
import           GHC.Compiler.Utils.HeaderOptions   (runParserMayPreprocessFromFile)
import           GHC.Compiler.Utils.Lexer

import           Lexer                              (Token(..))

import           SrcLoc

parseCollectedNotesFromHsFile :: (HasReader "envDynFlags" DynFlags m, MonadIO m)
                              => FilePath
                              -> m (Either ParseFailed CollectedNotes)
parseCollectedNotesFromHsFile = runParserFromHsFile pCollectNotes

runParserFromHsFile :: (HasReader "envDynFlags" DynFlags m, MonadIO m)
                    => Parser a
                    -> FilePath
                    -> m (Either ParseFailed a)
runParserFromHsFile p fn = do
  dflags <- ask @"envDynFlags"
  runParserMayPreprocessFromFile p dflags fn

pCollectNotes :: Parser CollectedNotes
pCollectNotes = runConduit $ pTokenConsumer .| sinkCollectNotes

sinkCollectNotes :: ConduitT (Located Token) o Parser CollectedNotes
sinkCollectNotes = cnCtxCollectedNotes
  <$> execStateT sinkWaitingNoteComment initialCollectedNotesCtx
