module Mineswept.GameSpec (spec) where

import Data.Time (getCurrentTime)
import Test.Hspec (Spec, it, shouldBe)

spec :: Spec
spec = do
  it "does stuff" $ do
    ts <- getCurrentTime
    1 `shouldBe` 1

-- play :: UTCTime -> Game
-- play ts = fromJust $ do
--   let g1 = initialGame params ts
--   g2 <- step g1 (Flag (1, 1)) ts
--   g3 <- step g2 (Dig (0, 1)) ts
--   step g3 (Dig (2, 0)) ts
