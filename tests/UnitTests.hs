module UnitTests where

import Test.Tasty.Hspec

import GHC.Compiler.Notes

spec_prelude :: Spec
spec_prelude = do
  describe "Expect Failure" $ do
    it "Multiline in CPP" $ do
      readComments "tests/resources/failure/Multiline.hs" `shouldReturn` Nothing
