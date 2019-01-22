{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Main (main) where

import RIO
import qualified RIO.List
import qualified RIO.Text
import Conduit
import Path
import Path.IO

import GHC.Compiler.Notes

main :: IO ()
main = do
  let targetDir = "submodules/ghc"
  runConduitRes $ sourceDirectoryDeep False targetDir .| awaitForever getCommentC
  return ()

getCommentC :: MonadIO m => FilePath -> ConduitT FilePath o m ()
getCommentC fp
  | ".hs" `RIO.List.isSuffixOf` fp && not (skipDir `RIO.List.isPrefixOf` fp) = liftIO $ getComment fp
  | otherwise = return ()
  where
    skipDir = "submodules/ghc/testsuite"

getComment :: FilePath -> IO ()
getComment fp =
  readComments fp >>= \case
    Nothing -> putStrLn (show fp ++": invalid token")
    Just ts -> do
      dir <- parent <$> parseRelFile fp
      let outDir = $(mkRelDir "output") </> dir
      ensureDir outDir
      writeFileUtf8 ("output/"++fp) $ RIO.Text.unlines ts