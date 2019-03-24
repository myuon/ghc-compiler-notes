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
import           Data.List (groupBy)
import           GHC.Compiler.Notes.Parser
import           GHC.Compiler.Notes.FormatRstDoc (unLoc)
import           GHC.Compiler.Notes.Types
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

data LineMode = MayCodeBlock | Paragraph
  deriving Eq

customFormatRstDoc :: CollectedNotes -> Text.Text
customFormatRstDoc CollectedNotes{..} = go $ toList $ codeBlocks . noteContent . unLoc <$> notes
  where
    go []     = ""
    go [n]    = n
    go (n:ns) = n <> "\n\n" <> go ns

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
            Text.head (Text.stripStart line) `notElem` ("123456789" :: String) &&
            -- A code block should not be start with `* ` nor `- ` (that's probably a list)
            not (Text.isPrefixOf "* " (Text.stripStart line)) &&
            not (Text.isPrefixOf "- " (Text.stripStart line))
            )

        insertCodeBlock = (".. code-block:: haskell\n" :)

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
        let d = customFormatRstDoc ns
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
