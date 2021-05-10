module Mineswept.Encoding
  ( encode,
    decode,
    currentVersion,
  )
where

import Data.List (groupBy, intercalate)
import Data.Text (Text, pack)
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Mineswept.Frame (Frame (..), Square)
import Mineswept.Game (Game (..))
import Mineswept.Grid (Grid)
import Mineswept.Grid qualified as Grid
import Mineswept.Internal.PShow (pshow)
import Mineswept.Minefield (Parameters (..))

currentVersion :: Int
currentVersion = 1

supportsVersion :: Parameters -> Bool
supportsVersion Parameters {version}
  | version == currentVersion = True
  | otherwise = False

encode :: Game -> Text
encode Game {parameters = Parameters {width, height, mineCount, seed, version}, frames} =
  pack paramsEncoded
    <> "\n"
    <> pack framesEncoded
  where
    paramsEncoded = unlines $ show <$> [version, width, height, mineCount, seed]
    framesEncoded = concatMap encodeFrame frames
    encodeFrame Frame {status, lastMove, created, squares} =
      intercalate "\n" [pshow status, pshow lastMove, show $ encodeTime created, encodeSquares squares] ++ "\n"
    encodeTime :: UTCTime -> Int
    encodeTime t = truncate $ utcTimeToPOSIXSeconds t
    encodeSquares :: Grid Square -> String
    encodeSquares squares =
      intercalate "\n" $
        (fmap . concatMap) (pshow . snd) $
          groupBy (\((_, y1), _) ((_, y2), _) -> y1 == y2) $ Grid.elems squares

decode :: Text -> Game
decode = undefined
