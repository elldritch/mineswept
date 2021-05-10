module Mineswept.Minefield
  ( Minefield,
    Tile (..),
    Parameters (..),
    makeMinefield,
    get,
    reveal,
    gridOf,
  )
where

import Control.Monad.Random (evalRand, mkStdGen)
import Data.Set qualified as Set
import Mineswept.Graph (LGraph, makeLGraph, reachable)
import Mineswept.Grid (Grid)
import Mineswept.Grid qualified as Grid
import Mineswept.Internal.PShow (PShow (..))
import System.Random.Shuffle (shuffleM)

data Tile
  = Mine
  | Hint Int
  deriving (Eq, Ord, Show)

instance PShow Tile where
  pshow Mine = "M"
  pshow (Hint n) = if n == 0 then " " else show n

data Minefield = Minefield
  { grid :: Grid Tile,
    zerosGraph :: LGraph (Int, Int)
  }

gridOf :: Minefield -> Grid Tile
gridOf (Minefield g _) = g

instance PShow Minefield where
  pshow (Minefield g _) = pshow g

data Parameters = Parameters
  { width :: Int,
    height :: Int,
    mineCount :: Int,
    seed :: Int,
    version :: Int
  }
  deriving (Show)

makeMinefield :: Parameters -> Minefield
makeMinefield Parameters {width, height, seed, mineCount} =
  Minefield
    { grid = hinted,
      zerosGraph = zerosGraph
    }
  where
    rng = mkStdGen seed
    positions = (\i -> (i `mod` width, i `div` width)) <$> [0 .. width * height - 1]
    shuffled = evalRand (shuffleM positions) rng
    (mined, empty) = splitAt mineCount shuffled
    unhinted = Grid.fromIndexedList (width, height) $ ((,Mine) <$> mined) ++ ((,Hint (-1)) <$> empty)
    hinted = Grid.mapWithKey (hintFor unhinted) unhinted
    hintFor grid pos val = case val of
      Hint _ -> Hint $ length $ filter ((== Mine) . snd) $ Grid.around8 grid pos
      Mine -> Mine
    zeros = fmap fst $ filter ((== Hint 0) . snd) $ Grid.elems hinted
    zeroGraphEdgesFor grid pos = fmap fst $ filter ((== Hint 0) . snd) $ Grid.around4 grid pos
    zerosGraph = makeLGraph $ (\pos -> (pos, zeroGraphEdgesFor hinted pos)) <$> zeros

get :: (Int, Int) -> Minefield -> Maybe Tile
get pos Minefield {grid} = Grid.get pos grid

-- Either reveal 1 mine or all the revealed blocks
reveal :: (Int, Int) -> Minefield -> Maybe [((Int, Int), Tile)]
reveal pos Minefield {grid, zerosGraph} = do
  spot <- Grid.get pos grid
  case spot of
    Hint 0 -> do
      rs <- reachable zerosGraph pos
      pure $ Set.toList $ Set.fromList $ concat $ Grid.around8 grid <$> rs
    _ -> Just [(pos, spot)]
