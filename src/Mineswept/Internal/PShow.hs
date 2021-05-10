module Mineswept.Internal.PShow (PShow (..)) where

class PShow a where
  pshow :: a -> String
