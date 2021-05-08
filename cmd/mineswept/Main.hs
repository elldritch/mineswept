module Main (main) where

import Data.Time (getCurrentTime)
import Mineswept.Game (Action (..), Parameters (..), initialGame, step)

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
  now <- getCurrentTime
  let game = initialGame params now
  print game
  let next = step game (1, 1) Flag now
  print next
  let dug = step game (1, 1) Dig now
  print dug

supportsVersion :: Parameters -> Bool
supportsVersion Parameters {version} = case version of
  1 -> True
  _ -> False
