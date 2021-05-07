module Mineswept.Grid
  ( Grid (..),
    fromList,
    rows,
    get,
    set,
  )
where

import Data.Foldable (toList)
import Data.List (intercalate)
import Data.Sequence (Seq, adjust, chunksOf, index)
import Data.Sequence qualified as Seq

data Grid a = Grid
  { width :: Int,
    height :: Int,
    cells :: Seq a
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
    then Just (Grid width height $ Seq.fromList cells)
    else Nothing

rows :: Grid a -> [[a]]
rows Grid {width, cells} = fmap toList $ toList $ chunksOf width cells

get :: (Int, Int) -> Grid a -> Maybe a
get (x, y) Grid {width, height, cells}
  | x < 0 = Nothing
  | x >= width = Nothing
  | y < 0 = Nothing
  | y >= height = Nothing
  | otherwise = Just $ cells `index` (x + y * width)

set :: (Int, Int) -> a -> Grid a -> Maybe (Grid a)
set (x, y) v g@Grid {width, height, cells}
  | x < 0 = Nothing
  | x >= width = Nothing
  | y < 0 = Nothing
  | y >= height = Nothing
  | otherwise = Just g {cells = adjust (const v) (x + y * width) cells}
