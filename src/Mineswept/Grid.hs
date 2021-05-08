module Mineswept.Grid
  ( Grid,
    fromList,
    elems,
    get,
    set,
    around4,
    around8,
  )
where

import Data.Bifunctor (second)
import Data.List (groupBy, intercalate, sortOn)
import Data.Maybe (catMaybes)
import Math.Geometry.Grid (neighbours)
import Math.Geometry.Grid.Octagonal (RectOctGrid, rectOctGrid)
import Math.Geometry.Grid.Square (RectSquareGrid, rectSquareGrid)
import Math.Geometry.GridMap (toGrid)
import Math.Geometry.GridMap qualified as GridMap
import Math.Geometry.GridMap.Lazy (LGridMap, lazyGridMap)

-- Hmm, should this be an _octagonal_ grid or a _square_ grid?
-- Flood fill for revealed 0 tiles is square, but mine counting is octagonal.
-- Maybe do _both_, as fields, and then provide two "around" accessors?
data Grid a = Grid
  { squareGrid :: LGridMap RectSquareGrid a,
    octGrid :: LGridMap RectOctGrid a
  }

fromList :: (Int, Int) -> [v] -> Grid v
fromList (width, height) vs =
  Grid
    { squareGrid = lazyGridMap (rectSquareGrid height width) vs,
      octGrid = lazyGridMap (rectOctGrid height width) vs
    }

instance Show a => Show (Grid a) where
  show (Grid gm _) = x6
    where
      x1 = sortOn (\((_, y), _) -> y) $ GridMap.toList gm
      x2 = second show <$> x1
      x3 = groupBy (\((_, b), _) ((_, d), _) -> b == d) x2
      x4 = (fmap . fmap) snd x3
      x5 = fmap concat x4
      x6 = intercalate "\n" x5

elems :: Grid a -> [a]
elems (Grid g _) = GridMap.elems g

get :: (Int, Int) -> Grid a -> Maybe a
get k (Grid g _) = GridMap.lookup k g

set :: (Int, Int) -> a -> Grid a -> Grid a
set k v (Grid gs go) = Grid (GridMap.insert k v gs) (GridMap.insert k v go)

around4 :: Grid a -> (Int, Int) -> [((Int, Int), a)]
around4 (Grid gs _) (x, y) = zip indexes values
  where
    indexes = neighbours (toGrid gs) (x, y)
    values = catMaybes $ (`GridMap.lookup` gs) <$> indexes

around8 :: Grid a -> (Int, Int) -> [((Int, Int), a)]
around8 (Grid _ go) (x, y) = zip indexes values
  where
    indexes = neighbours (toGrid go) (x, y)
    values = catMaybes $ (`GridMap.lookup` go) <$> indexes
