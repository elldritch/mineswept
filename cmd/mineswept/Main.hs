module Main (main) where

import Data.Fixed (Fixed (MkFixed))
import Data.Text.IO.Utf8 qualified as Utf8
import Data.Time (getCurrentTime, nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Main.Utf8 (withUtf8)
import Mineswept.CLI (NewGameParams (..), Position (..), argparser)
import Mineswept.CLI qualified as Command
import Mineswept.Encoding (currentVersion, decode, encode)
import Mineswept.Game (Action (..), Parameters (..), initialGame, step)
import Mineswept.Internal.PShow (pshow)
import Options.Applicative (execParser)
import System.Exit (die)

main :: IO ()
main = withUtf8 $ do
  cmd <- execParser argparser
  case cmd of
    Command.New NewGameParams {width, height, mineCount, filename, seed} -> do
      now <- getCurrentTime
      let (MkFixed ts) = nominalDiffTimeToSeconds $ utcTimeToPOSIXSeconds now
      let params =
            Parameters
              { width,
                height,
                mineCount,
                seed = case seed of
                  Just s -> s
                  Nothing -> fromInteger ts,
                version = currentVersion
              }
      let game = initialGame params now
      Utf8.writeFile filename $ encode game
      putStrLn $ "New game created at " <> filename
    Command.Dig filename pos -> act filename pos Dig
    Command.Flag filename pos -> act filename pos Flag
  where
    act :: FilePath -> Position -> ((Int, Int) -> Action) -> IO ()
    act filename Position {x, y} f = do
      now <- getCurrentTime
      contents <- Utf8.readFile filename
      game <- case decode filename contents of
        Right game -> return game
        Left err -> die $ "Could not parse game: " <> err
      next <- case step game (f (x, y)) now of
        Right next -> return next
        Left err -> die $ "Invalid move: " <> err
      putStrLn $ pshow next
      Utf8.writeFile filename $ encode next
