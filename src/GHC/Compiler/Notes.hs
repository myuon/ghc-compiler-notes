{-# LANGUAGE OverloadedStrings #-}
module GHC.Compiler.Notes
  ( readComments
  , getComments
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GHC.SyntaxHighlighter

readComments :: FilePath -> IO (Maybe [Text])
readComments fp = do
  file <- TIO.readFile fp
  return $ getComments file

getComments :: Text -> Maybe [Text]
getComments = fmap (map snd . filter isComment) . tokenizeHaskell . preprocess

preprocess :: Text -> Text
preprocess = T.unlines . map removeCPP . T.lines

removeCPP :: Text -> Text
removeCPP txt
  | isCpp txt = ""
  | otherwise = txt

isCpp :: Text -> Bool
isCpp text = or (T.isPrefixOf <$> cppKeywords <*> [T.stripStart text])

cppKeywords :: [Text]
cppKeywords = ["#include", "#define", "#if", "#ifdef", "#ifndef", "#elif", "#else", "#endif", "#undef"]

isComment :: (Token, a) -> Bool
isComment (CommentTok, _) = True
isComment _ = False
