module Mineswept.Frame
  ( Frame (..),
    makeFrame,
    initialFrame,
    Status (..),
    Action (..),
    Square (..),
  )
where

import Data.Foldable (find)
import Data.Maybe (isJust)
import Data.Time (UTCTime)
import Mineswept.Grid (Grid)
import Mineswept.Grid qualified as Grid
import Mineswept.Internal.PShow (PShow (..), indent)

data Status
  = Playing
  | Won
  | Lost
  deriving (Show)

data Action
  = Start
  | Dig (Int, Int)
  | Flag (Int, Int)
  deriving (Eq, Show)

data Square
  = Unrevealed
  | Revealed Int
  | Flagged
  | Exploded
  deriving (Eq, Show)

instance PShow Square where
  pshow Unrevealed = "?"
  pshow (Revealed n) = if n == 0 then " " else show n
  pshow Flagged = "F"
  pshow Exploded = "X"

data Frame = Frame
  { status :: Status,
    squares :: Grid Square,
    lastMove :: Action,
    created :: UTCTime
  }

{- ORMOLU_DISABLE -}
instance PShow Frame where
  pshow Frame{..} =
       "Turn {\n"
    ++ "  status: " ++ show status ++ "\n"
    ++ "  last move: " ++ show lastMove ++ "\n"
    ++ "  timestamp: " ++ show created ++ "\n"
    ++ "  squares: {\n"
    ++ indent 4 (pshow squares) ++ "\n"
    ++ "  }\n"
    ++ "}\n"
{- ORMOLU_ENABLE -}

makeFrame :: Grid Square -> Action -> UTCTime -> Frame
makeFrame g a ts = Frame status g a ts
  where
    squares = snd <$> Grid.elems g
    exploded = find (== Exploded) squares
    unrevealed = find (== Unrevealed) squares
    status
      | isJust exploded = Lost
      | isJust unrevealed = Playing
      | otherwise = Won

initialFrame :: (Int, Int) -> UTCTime -> Frame
initialFrame (width, height) = makeFrame squares Start
  where
    squares = Grid.fromList (width, height) $ replicate (width * height) Unrevealed
