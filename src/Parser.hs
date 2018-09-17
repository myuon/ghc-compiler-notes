module Parser where

import RIO
import qualified RIO.Text as T
import qualified Text.Trifecta as Tri

parseComments :: String -> Tri.Result [String]
parseComments = Tri.parseString parser mempty where
  parser = many $
    Tri.try (Tri.symbol "--" *> Tri.manyTill Tri.anyChar (void Tri.newline <|> Tri.eof))
    <|> Tri.symbol "{-" *> (Tri.manyTill Tri.anyChar (Tri.try $ Tri.spaces *> Tri.symbol "-}"))

data Doc
  = Section T.Text
  | Note T.Text
  | Pragma T.Text
  | Copyright [T.Text]
  | Paragraph T.Text
  deriving (Eq, Show)

parseDoc :: T.Text -> Tri.Result Doc
parseDoc = Tri.parseString parser mempty . T.unpack where
  parser = Tri.choice [section, paragraph]

  section =
    fmap (Section . T.pack) $
    Tri.string "*****" *> Tri.many (Tri.char '*' <|> Tri.space) *>
    Tri.manyTill Tri.anyChar (Tri.char '*' <|> Tri.newline)
  paragraph =
    fmap (Paragraph . T.pack) $ Tri.many Tri.anyChar

resultAsMaybe :: Tri.Result a -> Maybe a
resultAsMaybe = Tri.foldResult (\_ -> Nothing) Just

