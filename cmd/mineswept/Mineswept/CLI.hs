module Mineswept.CLI
  ( Command (..),
    Position (..),
    NewGameParams (..),
    argparser,
  )
where

import Control.Applicative ((<**>))
import Options.Applicative (argument, auto, help, hsubparser, long, metavar, option, short, str, strOption, value)
import Options.Applicative.Builder (command, fullDesc, header, info, progDesc)
import Options.Applicative.Extra (helper)
import Options.Applicative.Types (Parser, ParserInfo)

filePathP :: Parser FilePath
filePathP = strOption (long "file" <> short 'f' <> value "game.mines" <> help "File path of puzzle file")

data Position = Position
  { x :: Int,
    y :: Int
  }

positionP :: Parser Position
positionP =
  Position
    <$> argument auto (metavar "x")
    <*> argument auto (metavar "y")

data NewGameParams = NewGameParams
  { width :: Int,
    height :: Int,
    mineCount :: Int,
    filename :: FilePath
  }

newGameParamsP :: Parser NewGameParams
newGameParamsP =
  NewGameParams
    <$> option auto (long "width" <> short 'w' <> value 30 <> help "Width of puzzle")
    <*> option auto (long "height" <> short 'h' <> value 16 <> help "Height of puzzle")
    <*> option auto (long "mines" <> short 'm' <> value 99 <> help "Number of mines")
    <*> argument str (metavar "FILE" <> value "game.mines" <> help "Path to save puzzle file")

data Command
  = New NewGameParams
  | Dig FilePath Position
  | Flag FilePath Position

cmdP :: Parser Command
cmdP =
  hsubparser $
    command "new" (info (New <$> newGameParamsP) (progDesc "Create a new mineswept game file."))
      <> command "dig" (info (Dig <$> filePathP <*> positionP) (progDesc "Dig up the specified square."))
      <> command "flag" (info (Flag <$> filePathP <*> positionP) (progDesc "Flag the specified square."))

argparser :: ParserInfo Command
argparser =
  info
    (cmdP <**> helper)
    (fullDesc <> header "mineswept - a machine-friendly CLI for minesweeper games")
