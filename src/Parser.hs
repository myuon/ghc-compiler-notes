module Parser where

import RIO
import qualified Text.Trifecta as Tri

parseComments :: String -> Tri.Result [String]
parseComments = Tri.parseString parser mempty where
  parser = many $
    Tri.try (Tri.symbol "--" *> Tri.manyTill Tri.anyChar (void Tri.newline <|> Tri.eof))
    <|> Tri.symbol "{-" *> (Tri.manyTill Tri.anyChar (Tri.try $ Tri.spaces *> Tri.symbol "-}"))


