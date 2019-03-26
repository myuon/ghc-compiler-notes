module Main where

import           Control.Monad.IO.Class

import qualified Data.Text.IO                    as Text

import           GHC.Compiler.Notes.App
import           GHC.Compiler.Notes.FormatRstDoc
import           GHC.Compiler.Notes.Parser

import           Options.Applicative             hiding (Parser)
import qualified Options.Applicative             as Options

data ExtractNotesOption = ExtractNotesOption { optHsSrcPath :: FilePath }
  deriving (Eq, Show)

extractNotesOption :: Options.Parser ExtractNotesOption
extractNotesOption = ExtractNotesOption
  <$> strArgument (metavar "FILE")

main :: IO ()
main = do
  opt <- execParser $ info (extractNotesOption <**> helper) $
    header "Extractor of GHC Compiler Notes" <> progDesc "Extract GHC compiler notes" <> fullDesc
  ctx <- defaultAppContext
  runAppT (app opt) ctx

app :: ExtractNotesOption -> AppT IO ()
app opt = do
  r <- parseCollectedNotesFromHsFile $ optHsSrcPath opt
  case r of
    Left lf  -> liftIO $ print lf
    Right ns -> liftIO . Text.putStrLn =<< formatRstDoc (optHsSrcPath opt) ns
