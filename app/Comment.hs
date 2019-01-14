{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import RIO
import qualified RIO.List
import qualified RIO.Text
import Conduit
import GHC.SyntaxHighlighter
import Path
import Path.IO

main :: IO ()
main = do
  runConduitRes $ sourceDirectoryDeep False "submodules/ghc" .| awaitForever getCommentC
  return ()

getCommentC :: MonadIO m => FilePath -> ConduitT FilePath o m ()
getCommentC fp
  | ".hs" `RIO.List.isSuffixOf` fp = liftIO $ getComment fp
  | otherwise = return ()

getComment :: FilePath -> IO ()
getComment fp = do
  file <- readFileUtf8 fp
  case map snd . filter isComment <$> tokenizeHaskell (preprocess file) of
    Nothing -> putStrLn (show fp ++": invalid token")
    Just ts -> do
      dir <- parent <$> parseRelFile fp
      let outDir = $(mkRelDir "output") </> dir
      ensureDir outDir
      writeFileUtf8 ("output/"++fp) $ RIO.Text.unlines ts

preprocess :: Text -> Text
preprocess = RIO.Text.unlines . map removeCPP . RIO.Text.lines

removeCPP :: Text -> Text
removeCPP txt
  | or (RIO.Text.isPrefixOf <$> cpp <*> [RIO.Text.stripStart txt]) = ""
  | otherwise = txt
  where
    cpp = ["#include", "#define", "#if", "#ifdef", "#ifndef", "#elif", "#else", "#endif", "#undef"]

isComment :: (Token, Text) -> Bool
isComment (CommentTok, _) = True
isComment _ = False
