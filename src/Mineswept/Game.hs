module Mineswept.Game
  ( Parameters (..),
    Minefield,
    makeMinefield,
    Status (..),
    Square (..),
    Frame (..),
    makeFrame,
    initialFrame,
    Game (..),
    initialGame,
    Action (..),
    step,
  )
where

import Control.Monad.Random (evalRand, mkStdGen)
import Data.IntSet qualified as IntSet
import Data.List (find)
import Data.List.NonEmpty (NonEmpty (..), (<|))
import Data.Maybe (fromJust, isJust)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Time (UTCTime)
import Mineswept.Grid (Grid)
import Mineswept.Grid qualified as Grid
import System.Random.Shuffle (shuffleM)

data Parameters = Parameters
  { width :: Int,
    height :: Int,
    mineCount :: Int,
    seed :: Int,
    version :: Int
  }
  deriving (Show)

newtype Mine = Mine Bool
  deriving (Enum, Eq)

instance Show Mine where
  show (Mine True) = "M"
  show (Mine False) = " "

type Minefield = Grid Mine

makeMinefield :: Parameters -> Minefield
makeMinefield Parameters {width, height, seed, mineCount} =
  Grid.fromList (width, height) mines
  where
    rng = mkStdGen seed
    positions = [0 .. width * height - 1]
    shuffled = evalRand (shuffleM positions) rng
    minePositions = IntSet.fromList $ take mineCount shuffled
    mines = take (width * height) $ fmap (Mine . (`IntSet.member` minePositions)) [0 ..]

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
    squares = Grid.elems g
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
    mines :: Minefield,
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
      mines = makeMinefield params,
      frames = initialFrame (width, height) ts :| []
    }

data Action
  = Dig
  | Flag
  deriving (Eq)

step :: Game -> (Int, Int) -> Action -> UTCTime -> Maybe Game
step game@Game {width, height, frames = frames@(Frame {squares} :| _), mines} (x, y) action ts
  | x < 0 = Nothing
  | x >= width = Nothing
  | y < 0 = Nothing
  | y >= height = Nothing
  | otherwise = Just $ game {frames = makeFrame nextGrid ts <| frames}
  where
    nextGrid = case action of
      Dig -> dug
      Flag -> flagged

    (Mine mine) = fromJust $ Grid.get (x, y) mines
    fillZeros = fromJust $ fill mines (x, y) (== Mine False)
    dug =
      if mine
        then Grid.set (x, y) Exploded squares
        else foldr (\k g -> Grid.set k (Revealed $ fromJust $ adjacentMines mines k) g) squares fillZeros

    flagged = Grid.set (x, y) Flagged squares

adjacentMines :: Minefield -> (Int, Int) -> Maybe Int
adjacentMines mines k = case Grid.get k mines of
  Just _ -> Just $ sum $ fromEnum . snd <$> Grid.around8 mines k
  Nothing -> Nothing

fill :: Grid a -> (Int, Int) -> (a -> Bool) -> Maybe [(Int, Int)]
fill g k f = case Grid.get k g of
  Just _ -> Just $ fill' Set.empty k
  Nothing -> Nothing
  where
    fill' :: Set (Int, Int) -> (Int, Int) -> [(Int, Int)]
    fill' seen k'
      | k' `Set.member` seen = []
      | not passedFilter = []
      -- Infinite loop bug here? I need to update `seen` map on _every_ iteration, otherwise tree explodes - need queue somehow.
      | otherwise = k' : concatMap recurse around
      where
        v = fromJust $ Grid.get k' g
        passedFilter = f v
        around = fst <$> Grid.around4 g k'
        recurse = fill' $ Set.insert k' seen
