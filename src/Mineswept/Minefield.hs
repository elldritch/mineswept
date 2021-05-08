module Mineswept.Minefield
  ( Minefield,
    Parameters (..),
    makeMinefield,
  )
where

import Data.Graph (Graph, Vertex)
import Mineswept.Grid (Grid)

newtype Mine = Mine Bool
  deriving (Enum, Eq)

instance Show Mine where
  show (Mine True) = "M"
  show (Mine False) = " "

data LabelledGraph a = LabelledGraph
  { graph :: Graph,
    labelFromVertex :: Vertex -> (a, [a]),
    vertexFromLabel :: a -> Maybe Vertex
  }

data Minefield = Minefield
  { grid :: Grid Mine,
    zeros :: LabelledGraph (Int, Int)
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
makeMinefield Parameters {width, height, seed, mineCount} = undefined

-- Grid.fromList (width, height) mines
-- where
--   rng = mkStdGen seed
--   positions = [0 .. width * height - 1]
--   shuffled = evalRand (shuffleM positions) rng
--   minePositions = IntSet.fromList $ take mineCount shuffled
--   mines = take (width * height) $ fmap (Mine . (`IntSet.member` minePositions)) [0 ..]
