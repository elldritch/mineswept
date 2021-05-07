{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Data.Time (getCurrentTime)
import Mineswept.Game (Parameters (..), initialFrame, makeMinefield)

main :: IO ()
main = do
  let w = 30
  let h = 16
  let params = Parameters w h 99 0 1
  let mines = makeMinefield params
  print mines
  now <- getCurrentTime
  let frame = initialFrame (w, h) now
  print frame

supportsVersion :: Parameters -> Bool
supportsVersion Parameters {version} = case version of
  1 -> True
  _ -> False
