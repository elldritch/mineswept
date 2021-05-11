module Mineswept.Spec.Util
  ( step',
    decode',
    squareAt,
    elems,
  )
where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Time (UTCTime)
import Mineswept.Encoding (decode)
import Mineswept.Frame (Frame (..), Square)
import Mineswept.Game (Action, Game (..), step)
import Mineswept.Grid qualified as Grid

step' :: Game -> Action -> UTCTime -> Game
step' game action ts = case step game action ts of
  Right g -> g
  Left err -> error $ "step': invalid action: " <> err

decode' :: Text -> Game
decode' msg = case decode "test" msg of
  Right g -> g
  Left err -> error $ "decode': invalid decode: " <> err

squareAt :: Game -> (Int, Int) -> Square
squareAt Game {frames = Frame {squares} :| _} pos = fromJust $ Grid.get pos squares

elems :: Game -> [((Int, Int), Square)]
elems Game {frames = Frame {squares} :| _} = Grid.elems squares
