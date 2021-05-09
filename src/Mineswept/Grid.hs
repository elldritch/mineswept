module Mineswept.Grid
  ( Grid,
    fromList,
    fromIndexedList,
    elems,
    get,
    set,
    around4,
    around8,
    mapWithKey,
  )
where

import Data.Bifunctor (second)
import Data.List (groupBy, intercalate, sortOn)
import Data.Maybe (catMaybes, fromJust)
import Math.Geometry.Grid (neighbours)
import Math.Geometry.Grid.Octagonal (RectOctGrid, rectOctGrid)
import Math.Geometry.Grid.Square (RectSquareGrid, rectSquareGrid)
import Math.Geometry.GridMap (toGrid)
import Math.Geometry.GridMap qualified as GridMap
import Math.Geometry.GridMap.Lazy (LGridMap, lazyGridMap, lazyGridMapIndexed)

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

fromIndexedList :: (Int, Int) -> [((Int, Int), v)] -> Grid v
fromIndexedList (width, height) vs =
  Grid
    { squareGrid = lazyGridMapIndexed (rectSquareGrid height width) vs,
      octGrid = lazyGridMapIndexed (rectOctGrid height width) vs
    }

instance Show a => Show (Grid a) where
  show (Grid gm _) = x6
    where
      x1 = sortOn (\((_, y), _) -> y) $ GridMap.toList gm
      x2 = second show <$> x1
      x3 = groupBy (\((_, b), _) ((_, d), _) -> b == d) x2
      x4 = (fmap . fmap) snd x3
      x5 = concat <$> x4
      x6 = intercalate "\n" x5

elems :: Grid a -> [((Int, Int), a)]
elems (Grid g _) = (\pos -> (pos, fromJust $ GridMap.lookup pos g)) <$> GridMap.keys g

get :: (Int, Int) -> Grid a -> Maybe a
get pos (Grid g _) = GridMap.lookup pos g

set :: (Int, Int) -> a -> Grid a -> Grid a
set pos v (Grid gs go) = Grid (GridMap.insert pos v gs) (GridMap.insert pos v go)

around4 :: Grid a -> (Int, Int) -> [((Int, Int), a)]
around4 (Grid gs _) pos = zip indexes values
  where
    indexes = neighbours (toGrid gs) pos
    values = catMaybes $ (`GridMap.lookup` gs) <$> indexes

around8 :: Grid a -> (Int, Int) -> [((Int, Int), a)]
around8 (Grid _ go) pos = zip indexes values
  where
    indexes = neighbours (toGrid go) pos
    values = catMaybes $ (`GridMap.lookup` go) <$> indexes

mapWithKey :: ((Int, Int) -> a -> b) -> Grid a -> Grid b
mapWithKey f (Grid gs go) = Grid (GridMap.mapWithKey f gs) (GridMap.mapWithKey f go)
