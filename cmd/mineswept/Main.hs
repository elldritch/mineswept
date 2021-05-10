module Main (main) where

import Data.Maybe (fromJust)
import Data.Time (getCurrentTime)
import Mineswept.CLI (argparser)
import Mineswept.Game (Action (..), Parameters (..), initialGame, step)
import Mineswept.Internal.PShow (pshow)
import Options.Applicative (execParser)

params :: Parameters
params =
  Parameters
    { width = 30,
      height = 16,
      version = 1,
      mineCount = 99,
      seed = 0
    }

main :: IO ()
main = do
  args <- execParser argparser
  ts <- getCurrentTime
  putStrLn $ pshow $ play ts
  where
    play ts = fromJust $ do
      let g1 = initialGame params ts
      g2 <- step g1 (Flag (1, 1)) ts
      g3 <- step g2 (Dig (0, 1)) ts
      step g3 (Dig (2, 0)) ts

supportsVersion :: Parameters -> Bool
supportsVersion Parameters {version} = case version of
  1 -> True
  _ -> False
