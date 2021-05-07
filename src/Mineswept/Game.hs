{-# LANGUAGE DuplicateRecordFields #-}

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
import Data.Maybe (fromJust, isJust)
import Data.Sequence.NonEmpty (NESeq ((:<||)), (<|))
import Data.Sequence.NonEmpty qualified as NESeq
import Data.Time (UTCTime)
import Mineswept.Grid (Grid (..))
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

instance Show Mine where
  show (Mine True) = "M"
  show (Mine False) = " "

type Minefield = Grid Mine

makeMinefield :: Parameters -> Minefield
makeMinefield Parameters {width, height, seed, mineCount} =
  case grid of
    Just g -> g
    Nothing -> error "makeMinefield: impossible: malformed grid"
  where
    rng = mkStdGen seed
    positions = [0 .. width * height - 1]
    shuffled = evalRand (shuffleM positions) rng
    minePositions = IntSet.fromList $ take mineCount shuffled
    grid = Grid.fromList (width, height) $ take (width * height) $ fmap (Mine . (`IntSet.member` minePositions)) [0 ..]

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
    created :: UTCTime,
    squares :: Grid Square
  }
  deriving (Show)

makeFrame :: Grid Square -> UTCTime -> Frame
makeFrame g@Grid {cells} ts = Frame status ts g
  where
    exploded = find (== Exploded) cells
    unrevealed = find (== Unrevealed) cells
    status
      | isJust exploded = Lost
      | isJust unrevealed = Playing
      | otherwise = Won

initialFrame :: (Int, Int) -> UTCTime -> Frame
initialFrame (width, height) = makeFrame squares
  where
    squares' = Grid.fromList (width, height) $ replicate (width * height) Unrevealed
    squares = case squares' of
      Just g -> g
      Nothing -> error "initialFrame: impossible: malformed grid"

data Game = Game
  { width :: Int,
    height :: Int,
    seed :: Int,
    version :: Int,
    mines :: Minefield,
    frames :: NESeq Frame
  }

initialGame :: Parameters -> UTCTime -> Game
initialGame p@Parameters {width, height, seed} ts =
  Game
    { width,
      height,
      seed,
      version = 1,
      mines = makeMinefield p,
      frames = NESeq.singleton $ initialFrame (width, height) ts
    }

data Action
  = Dig
  | Flag

step :: Game -> (Int, Int) -> Action -> UTCTime -> Game
step g@Game {frames = fs@(Frame {squares} :<|| _)} (x, y) Dig ts =
  undefined
step g@Game {frames = fs@(Frame {squares} :<|| _)} (x, y) Flag ts =
  g {frames = f <| fs}
  where
    g' = Grid.set (x, y) Flagged squares
    f = makeFrame (fromJust g') ts
step Game {frames = _} _ _ _ = error "step: impossible: missing pattern"
