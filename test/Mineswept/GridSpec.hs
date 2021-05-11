module Mineswept.GridSpec (spec) where

import Data.Maybe (fromJust)
import Mineswept.Grid (Grid)
import Mineswept.Grid qualified as Grid
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "fromList" $ do
    it "takes a row-major values list" $ do
      let g :: Grid Int = Grid.fromList (3, 3) [1 .. 9]
      fromJust (Grid.get (1, 0) g) `shouldBe` 2
