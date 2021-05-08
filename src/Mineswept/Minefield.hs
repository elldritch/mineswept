module Mineswept.Minefield
  ( Minefield,
    Parameters (..),
    makeMinefield,
    get,
    reachableZeros,
  )
where

import Control.Monad.Random (evalRand, mkStdGen)
import Data.Graph (Graph, Vertex)
import Data.IntSet qualified as IntSet
import Mineswept.Grid (Grid)
import Mineswept.Grid qualified as Grid
import System.Random.Shuffle (shuffleM)

data Tile
  = Mine
  | Hint Int

instance Show Tile where
  show Mine = "M"
  show (Hint n) = if n == 0 then " " else show n

data LabelledGraph a = LabelledGraph
  { graph :: Graph,
    labelFromVertex :: Vertex -> (a, [a]),
    vertexFromLabel :: a -> Maybe Vertex
  }

data Minefield = Minefield
  { grid :: Grid Tile,
    zeroGraph :: LabelledGraph (Int, Int)
  }

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
    { grid = Grid.fromList (width, height) mines,
      zeroGraph = undefined
    }
  where
    rng = mkStdGen seed
    positions = [0 .. width * height - 1]
    shuffled = evalRand (shuffleM positions) rng
    minePositions = IntSet.fromList $ take mineCount shuffled
    -- TODO: "Mine" needs to be "\x -> if x then Mine else Hint ?", need to pre-compute hint numbers.
    mines = take (width * height) $ fmap (Mine . (`IntSet.member` minePositions)) [0 ..]

get :: Minefield -> (Int, Int) -> Tile
get minefield (x, y) = undefined

reachableZeros :: Minefield -> (Int, Int) -> [(Int, Int)]
reachableZeros minefield (x, y) = undefined

-- Either reveal 1 mine or all the revealed blocks
dig :: Minefield -> (Int, Int) -> [((Int, Int), Tile)]
dig = undefined

-- Reveal all checked blocks (including 0 flooded)
check :: Minefield -> (Int, Int) -> [((Int, Int), Tile)]
check = undefined
