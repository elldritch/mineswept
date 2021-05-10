module Main (main) where

import Control.Monad (when)
import Data.Fixed (Fixed (MkFixed))
import Data.Text.IO.Utf8 qualified as Utf8
import Data.Time (getCurrentTime, nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Main.Utf8 (withUtf8)
import Mineswept.CLI (Command (..), NewGameParams (..), Position (..), argparser)
import Mineswept.Encoding (currentVersion, encode)
import Mineswept.Game (Parameters (..), initialGame)
import Options.Applicative (execParser)
import System.Exit (die)

main :: IO ()
main = withUtf8 $ do
  cmd <- execParser argparser
  now <- getCurrentTime
  case cmd of
    New NewGameParams {width, height, mineCount, guaranteedSolvable, filename} -> do
      when guaranteedSolvable $ die "--guaranteed-solvable is not yet implemented"

      let (MkFixed ts) = nominalDiffTimeToSeconds $ utcTimeToPOSIXSeconds now
      let params =
            Parameters
              { width,
                height,
                mineCount,
                seed = fromInteger ts,
                version = currentVersion
              }
      let game = initialGame params now
      Utf8.writeFile filename $ encode game
      putStrLn $ "New game created at " <> filename
    Dig filename Position {x, y} -> undefined
    Flag filename Position {x, y} -> undefined
    Check filename -> undefined
    Suggest filename -> undefined
    Reveal filename -> undefined
