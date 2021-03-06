module Mineswept.EncodingSpec (spec) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Mineswept.Encoding (encode)
import Mineswept.Frame (Square (..))
import Mineswept.Game (Action (..), Parameters (..), initialGame)
import Mineswept.Spec.Util (decode', squareAt, step')
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  let ts = posixSecondsToUTCTime 0
  let g1 = initialGame params ts

  describe "encode" $ do
    it "encodes initial games" $ do
      encode g1 `shouldBe` encodedInitial

    -- TODO: Make a "encoded step" monad?
    it "is the inverse of decode" $ do
      let g2 = step' g1 (Dig (1, 0)) ts
      let encoded_g2 = encode g2
      let decoded_g2 = decode' encoded_g2
      let g3 = step' decoded_g2 (Flag (2, 0)) ts
      let encoded_g3 = encode g3
      let decoded_g3 = decode' encoded_g3
      let g4 = step' decoded_g3 (Dig (2, 8)) ts
      let encoded_g4 = encode g4
      let decoded_g4 = decode' encoded_g4
      squareAt decoded_g4 (1, 0) `shouldBe` Revealed 1

params :: Parameters
params =
  Parameters
    { width = 30,
      height = 16,
      version = 1,
      mineCount = 99,
      seed = 0
    }

encodedInitial :: Text
encodedInitial =
  T.unlines
    [ "1",
      "30",
      "16",
      "99",
      "0",
      "",
      "P",
      "S",
      "0",
      "??????????????????????????????",
      "??????????????????????????????",
      "??????????????????????????????",
      "??????????????????????????????",
      "??????????????????????????????",
      "??????????????????????????????",
      "??????????????????????????????",
      "??????????????????????????????",
      "??????????????????????????????",
      "??????????????????????????????",
      "??????????????????????????????",
      "??????????????????????????????",
      "??????????????????????????????",
      "??????????????????????????????",
      "??????????????????????????????",
      "??????????????????????????????",
      ""
    ]
