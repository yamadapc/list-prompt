module System.Console.ListPromptSpec
  where

import System.Console.ListPrompt.Internal
import System.Console.Terminal.Size (Window(..))
import Test.Hspec

spec :: Spec
spec = -- do
    describe "getDimensions" $ do
        it "works when a `Window `is provided, centering the prompt" $ do
            let dim = getDimensions 10 (Just (Window 80 80))
            dim `shouldBe` undefined
