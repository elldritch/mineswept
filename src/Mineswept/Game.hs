module Mineswept.Game
  ( Status (..),
    Square (..),
    Frame (..),
    makeFrame,
    initialFrame,
    Game (..),
    Parameters (..),
    initialGame,
    Action (..),
    step,
  )
where

import Data.List (find)
import Data.List.NonEmpty (NonEmpty (..), (<|))
import Data.Maybe (isJust)
import Data.Time (UTCTime)
import Mineswept.Grid (Grid)
import Mineswept.Grid qualified as Grid
import Mineswept.Minefield (Minefield, Parameters (..), Tile (..), makeMinefield, reveal)
import Mineswept.Minefield qualified as Minefield

data Status
  = Playing
  | Won
  | Lost
  deriving (Show)

data Square
  = Unrevealed
  | Revealed Int
  | Flagged
  | Exploded
  deriving (Eq)

instance Show Square where
  show Unrevealed = "?"
  show (Revealed n) = show n
  show Flagged = "F"
  show Exploded = "X"

data Frame = Frame
  { status :: Status,
    squares :: Grid Square,
    created :: UTCTime
  }
  deriving (Show)

makeFrame :: Grid Square -> UTCTime -> Frame
makeFrame g ts = Frame status g ts
  where
    squares = snd <$> Grid.elems g
    exploded = find (== Exploded) squares
    unrevealed = find (== Unrevealed) squares
    status
      | isJust exploded = Lost
      | isJust unrevealed = Playing
      | otherwise = Won

initialFrame :: (Int, Int) -> UTCTime -> Frame
initialFrame (width, height) = makeFrame squares
  where
    squares = Grid.fromList (width, height) $ replicate (width * height) Unrevealed

data Game = Game
  { width :: Int,
    height :: Int,
    seed :: Int,
    version :: Int,
    minefield :: Minefield,
    frames :: NonEmpty Frame
  }
  deriving (Show)

initialGame :: Parameters -> UTCTime -> Game
initialGame params@Parameters {width, height, seed} ts =
  Game
    { width,
      height,
      seed,
      version = 1,
      minefield = makeMinefield params,
      frames = initialFrame (width, height) ts :| []
    }

data Action
  = Dig
  | Flag
  deriving (Eq)

step :: Game -> (Int, Int) -> Action -> UTCTime -> Maybe Game
step game@Game {frames = frames@(Frame {squares} :| _), minefield} pos action ts = do
  nextGrid <- makeNextGrid
  Just $ game {frames = makeFrame nextGrid ts <| frames}
  where
    makeNextGrid = case action of
      Dig -> dug
      Flag -> Just flagged

    uncover (p, tile) grid = case tile of
      Mine -> Grid.set p Exploded grid
      Hint h -> Grid.set p (Revealed h) grid

    dug = do
      tile <- Minefield.get pos minefield
      revealed <- case tile of
        Mine -> Just [(pos, tile)]
        Hint _ -> reveal pos minefield
      Just $ foldr uncover squares revealed

    flagged = Grid.set pos Flagged squares
