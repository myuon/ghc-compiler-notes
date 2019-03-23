{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad.IO.Class
import           GHC.Compiler.Notes.App
import           GHC.Compiler.Notes.Config
import           Options.Applicative       hiding ( Parser )
import qualified Options.Applicative       as Options
import           RIO
import qualified System.FilePath.Glob      as Glob
import           System.FilePath
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Data.Maybe (fromJust)
import           GHC.Compiler.Notes.Parser
import           GHC.Compiler.Notes.FormatRstDoc
import           System.Directory


data CommentOption = CommentOption
  { optConfigPath :: FilePath
  }
  deriving (Eq, Show)

commentOption :: Options.Parser CommentOption
commentOption = CommentOption
  <$> argument str (metavar "FILE")

main :: IO ()
main = do
  opt <- execParser $ info (commentOption <**> helper)
    $  header "GHC Compiler Notes"
    <> progDesc "Output GHC compiler notes"
    <> fullDesc
  ctx <- defaultAppContext
  runAppT (app opt) ctx

app :: CommentOption -> AppT IO ()
app opt = do
  config <- (liftIO $ parseConfigFromFile $ optConfigPath opt) >>= \case
    Right conf -> pure conf
    Left err   -> throwM err
  files <- fmap join $ liftIO $ Glob.globDir (confTargets config) "output/ghc"
  -- TODO: catch GhcExceptions and continue
  forM_ files \fn -> do
    let outputFn = confOutDir config </> (joinPath $ drop 2 $ splitPath fn) <> ".rst"
    r <- parseCollectedNotesFromHsFile fn
    case r of
      Left lf  -> liftIO $ print lf
      Right ns -> do
        let d = formatRstDoc ns
        when (Text.length (Text.strip d) /= 0) $ do
          -- Create a directory and place an index.rst
          directoryExists <- liftIO $ doesDirectoryExist $ takeDirectory outputFn
          when (not directoryExists) $ do
            liftIO $ createDirectoryIfMissing True $ takeDirectory outputFn
            liftIO $ Text.writeFile (takeDirectory outputFn </> "index.rst") $ Text.unlines [
              fromJust $ Text.stripPrefix "docs/notes/" $ Text.pack (takeDirectory outputFn),
              "=================================",
              "",
              ".. toctree::",
              "    :maxdepth: 2",
              "    :caption: Contents:",
              "    :glob:",
              "",
              "    *"
              ]

          liftIO $ Text.writeFile outputFn $ Text.unlines [
            "`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/" `Text.append` (fromJust $ Text.stripSuffix ".rst" $ fromJust $ Text.stripPrefix "docs/notes/" $ Text.pack outputFn) `Text.append` ">`_",
            "",
            d
            ]
