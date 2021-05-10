module Mineswept.Internal.PShow
  ( PShow (..),
    indent,
  )
where

import Data.List (intercalate)

class PShow a where
  pshow :: a -> String

indent :: Int -> String -> String
indent depth s = intercalate "\n" $ (\line -> replicate depth ' ' ++ line) <$> lines s
