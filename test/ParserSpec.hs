module ParserSpec where

import RIO
import Parser
import Test.Hspec
import qualified Text.Trifecta as Tri

fromSuccess :: Tri.Result a -> a
fromSuccess (Tri.Success a) = a
fromSuccess (Tri.Failure err) = error $ show err

spec :: Spec
spec = do
  describe "parseComments" $ do
    it "other" $ do
      fromSuccess (parseComments "main :: IO ()\nmain = undefined") `shouldBe` []
    it "line comment" $ do
      fromSuccess (parseComments "-- hoge") `shouldBe` ["hoge"]
    it "block comment" $ do
      fromSuccess (parseComments "{- hoge piyo nyan -}") `shouldBe` ["hoge piyo nyan"]
    it "multiple line comments" $ do
      fromSuccess (parseComments "-- hoge\n-- piyo") `shouldBe` ["hoge", "piyo"]
    it "multiple block comments" $ do
      fromSuccess (parseComments "{- hoge\npiyo\nnyan -}") `shouldBe` ["hoge\npiyo\nnyan"]

  describe "parseDoc" $ do
    it "ascii section" $ do
      fromSuccess (parseDoc "************************************************************************\n*                                                                      *\nLexical categories\n*                                                                      *\n************************************************************************") `shouldBe` Section "Lexical categories"
    it "paragraph" $ do
      fromSuccess (parseDoc "text text\ntext") `shouldBe` Paragraph "text text\ntext"

