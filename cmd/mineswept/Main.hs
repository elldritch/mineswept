module Main (main) where

import Data.Fixed (Fixed (MkFixed))
import Data.Text.IO.Utf8 qualified as Utf8
import Data.Time (getCurrentTime, nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Main.Utf8 (withUtf8)
import Mineswept.CLI (Command (..), NewGameParams (..), Position (..), argparser)
import Mineswept.Encoding (currentVersion, decode, encode)
import Mineswept.Game (Parameters (..), initialGame)
import Mineswept.Internal.PShow (pshow)
import Options.Applicative (execParser)
import System.Exit (die)

main :: IO ()
main = withUtf8 $ do
  cmd <- execParser argparser
  now <- getCurrentTime
  case cmd of
    New NewGameParams {width, height, mineCount, filename} -> do
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
    Dig filename Position {x, y} -> do
      contents <- Utf8.readFile filename
      game <- case decode filename contents of
        Right game -> return game
        Left err -> die $ "Could not parse game: " <> err
      putStrLn $ pshow game
      pure ()
    Flag filename Position {x, y} -> undefined
