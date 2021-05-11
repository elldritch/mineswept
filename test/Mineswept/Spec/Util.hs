module Mineswept.Spec.Util
  ( step',
    decode',
    squareAt,
    elems,
  )
where

import Data.Either (fromRight)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Time (UTCTime)
import Mineswept.Encoding (decode)
import Mineswept.Frame (Frame (..), Square)
import Mineswept.Game (Action, Game (..), step)
import Mineswept.Grid qualified as Grid

step' :: Game -> Action -> UTCTime -> Game
step' game action ts = fromRight (error "impossible: valid test moves") $ step game action ts

decode' :: Text -> Game
decode' msg = fromRight (error "impossible: valid test moves") $ decode "test" msg

squareAt :: Game -> (Int, Int) -> Square
squareAt Game {frames = Frame {squares} :| _} pos = fromJust $ Grid.get pos squares

elems :: Game -> [((Int, Int), Square)]
elems Game {frames = Frame {squares} :| _} = Grid.elems squares
