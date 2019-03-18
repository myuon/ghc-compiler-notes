module ResourceTests where

import Test.Tasty.Hspec

import GHC.Compiler.Notes.App
import GHC.Compiler.Notes.Parser
import GHC.Compiler.Notes.Types


spec_prelude :: Spec
spec_prelude = do
  describe "Expect Success" $ do
    it "Standard Example with CPP" $ do
      let app = parseCollectedNotesFromHsFile "test/resource/TestSrc.hs"
      ctx <- defaultAppContext
      r <- runAppT app ctx
      case r of
        Left{}   -> expectationFailure "Expect success of parsing"
        Right ns -> length (notes ns) `shouldBe` 4
