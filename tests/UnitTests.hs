module UnitTests where

import GHC.Compiler.Notes

import Test.Tasty.Hspec

spec_prelude :: Spec
spec_prelude = do
  describe "Expect Failure" $ do
    it "Multiline in CPP" $ do
      readComments "tests/resources/failure/Multiline.hs" `shouldReturn` Nothing
