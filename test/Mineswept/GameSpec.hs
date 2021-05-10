module Mineswept.GameSpec (spec) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromJust)
import Data.Time (UTCTime, getCurrentTime)
import Mineswept.Frame (Frame (..), Square (..))
import Mineswept.Game (Action (..), Game (..), Parameters (..), initialGame, step)
import Mineswept.Grid qualified as Grid
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "step" $ do
    it "does not reveal flagged squares" $ do
      ts <- getCurrentTime
      let Game {frames = Frame {squares} :| _} = play ts
      fromJust (Grid.get (1, 1) squares) `shouldBe` Flagged
  where
    play :: UTCTime -> Game
    play ts = fromJust $ do
      let g1 = initialGame params ts
      g2 <- step g1 (Flag (1, 1)) ts
      step g2 (Dig (0, 1)) ts

params :: Parameters
params =
  Parameters
    { width = 30,
      height = 16,
      version = 1,
      mineCount = 99,
      seed = 0
    }
