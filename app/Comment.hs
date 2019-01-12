module Main (main) where

import RIO
import qualified RIO.Text
import GHC.SyntaxHighlighter

main :: IO ()
main = do
  file <- readFileUtf8 "sample/UI.hs"
  case map snd . filter isComment <$> tokenizeHaskell file of
    Nothing -> error "invalid token"
    Just ts -> writeFileUtf8 "sample/UI.txt" $ RIO.Text.unlines ts

isComment :: (Token, Text) -> Bool
isComment (CommentTok, _) = True
isComment _ = False
