module Mineswept.GameSpec (spec) where

import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Mineswept.Frame (Square (..))
import Mineswept.Game (Action (..), Parameters (..), initialGame)
import Mineswept.Spec.Util (elems, squareAt, step')
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  let ts = posixSecondsToUTCTime 0
  let g1 = initialGame params ts

  describe "step" $ do
    it "does not reveal flagged squares" $ do
      let g2 = step' g1 (Flag (1, 1)) ts
      let g3 = step' g2 (Dig (0, 1)) ts
      squareAt g3 (1, 1) `shouldBe` Flagged

    it "handles multiple identical steps" $ do
      let g2 = step' g1 (Dig (1, 0)) ts
      let g3 = step' g2 (Dig (1, 0)) ts
      let g4 = step' g3 (Dig (1, 0)) ts
      squareAt g4 (1, 0) `shouldBe` Revealed 1
      length (filter ((/= Unrevealed) . snd) $ elems g4) `shouldBe` 1

params :: Parameters
params =
  Parameters
    { width = 30,
      height = 16,
      version = 1,
      mineCount = 99,
      seed = 0
    }
