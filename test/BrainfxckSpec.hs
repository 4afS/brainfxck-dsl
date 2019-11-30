module BrainfxckSpec
  ( spec
  ) where

import Brainfxck
import qualified Data.Vector as V
import Test.Hspec

spec :: Spec
spec =
  describe "update memory" $ do
    it "increment" $
      applyMemoryPointed (+ 1) 0 (V.replicate 4 0) `shouldBe`
      V.fromList [1, 0, 0, 0]
    it "decrement" $
      applyMemoryPointed (subtract 1) 0 (V.replicate 4 0) `shouldBe`
      V.fromList [-1, 0, 0, 0]
