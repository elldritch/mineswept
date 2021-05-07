{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Control.Monad.Random (evalRand)
import Data.Foldable (toList)
import qualified Data.IntSet as IntSet
import Data.List (find, intercalate)
import Data.Maybe (isJust)
import Data.Sequence (chunksOf)
import qualified Data.Sequence as Seq
import Data.Time (UTCTime, getCurrentTime)
import System.Random (mkStdGen)
import System.Random.Shuffle (shuffleM)

main :: IO ()
main = do
  let w = 30
  let h = 16
  let params = Parameters w h 99 0 1
  let mines = makeMinefield params
  print mines
  frame <- initialFrame (w, h)
  print frame

data Parameters = Parameters
  { width :: Int,
    height :: Int,
    mineCount :: Int,
    seed :: Int,
    version :: Int
  }
  deriving (Show)

supportsVersion :: Parameters -> Bool
supportsVersion Parameters {version} = case version of
  1 -> True
  _ -> False

data Grid a = Grid
  { width :: Int,
    height :: Int,
    cells :: [a] -- TODO: Maybe this is actually `Seq a`
  }

instance Show a => Show (Grid a) where
  show grid = intercalate "\n" shownRows
    where
      rs = rows grid
      shown = (fmap . fmap) show rs
      shownRows = fmap concat shown

fromList :: (Int, Int) -> [a] -> Maybe (Grid a)
fromList (width, height) cells =
  if width * height == length cells
    then Just (Grid width height cells)
    else Nothing

rows :: Grid a -> [[a]]
rows Grid {width, cells} = fmap toList $ toList $ chunksOf width $ Seq.fromList cells

set :: (Int, Int) -> a -> Grid a -> Maybe (Grid a)
set (x, y) v g@Grid {width, height, cells}
  | x < 0 = Nothing
  | x >= width = Nothing
  | y < 0 = Nothing
  | y >= height = Nothing
  | otherwise = Just g {cells = undefined}

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
    grid = fromList (width, height) $ take (width * height) $ fmap (Mine . (`IntSet.member` minePositions)) [0 ..]

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

makeFrame :: Grid Square -> IO Frame
makeFrame g@Grid {cells} = do
  now <- getCurrentTime
  pure $ Frame status now g
  where
    exploded = find (== Exploded) cells
    unrevealed = find (== Unrevealed) cells
    status
      | isJust exploded = Lost
      | isJust unrevealed = Playing
      | otherwise = Won

initialFrame :: (Int, Int) -> IO Frame
initialFrame (width, height) = makeFrame $ case squares' of
  Just g -> g
  Nothing -> error "initialFrame: impossible: malformed grid"
  where
    squares' = fromList (width, height) $ replicate (width * height) Unrevealed

data Game = Game
  { width :: Int,
    height :: Int,
    seed :: Int,
    version :: Int,
    mines :: Minefield,
    frames :: [Frame]
  }

data Action
  = Dig
  | Flag
  | Check

step :: Game -> (Int, Int) -> Action -> IO Game
step g (x, y) Dig = undefined
step g (x, y) Flag = undefined
step g (x, y) Check = undefined
