module Mineswept.Encoding
  ( encode,
    decode,
    currentVersion,
  )
where

import Control.Applicative (some, (<|>))
import Control.Applicative.Combinators.NonEmpty (someTill)
import Control.Monad (replicateM)
import Data.List (groupBy, intercalate)
import Data.Text (Text, pack)
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Void (Void)
import Mineswept.Frame (Action (..), Frame (..), Square (..), Status (..))
import Mineswept.Game (Game (..))
import Mineswept.Grid (Grid)
import Mineswept.Grid qualified as Grid
import Mineswept.Internal.PShow (pshow)
import Mineswept.Minefield (Parameters (..), makeMinefield)
import Text.Megaparsec (Parsec, eof, errorBundlePretty, runParser)
import Text.Megaparsec.Char (char, newline, numberChar)
import Text.Megaparsec.Char.Lexer (signed)

currentVersion :: Int
currentVersion = 1

supportsVersion :: Parameters -> Bool
supportsVersion Parameters {version}
  | version == currentVersion = True
  | otherwise = False

encode :: Game -> Text
encode Game {parameters = Parameters {width, height, mineCount, seed, version}, frames} =
  pack paramsEncoded
    <> "\n"
    <> pack framesEncoded
  where
    paramsEncoded = unlines $ show <$> [version, width, height, mineCount, seed]
    framesEncoded = concatMap encodeFrame frames
    encodeFrame Frame {status, lastMove, created, squares} =
      intercalate "\n" [pshow status, pshow lastMove, show $ encodeTime created, encodeSquares squares]
        ++ "\n\n"
    encodeTime :: UTCTime -> Int
    encodeTime t = truncate $ utcTimeToPOSIXSeconds t
    encodeSquares :: Grid Square -> String
    encodeSquares squares =
      intercalate "\n" $
        (fmap . concatMap) (pshow . snd) $
          groupBy (\((_, y1), _) ((_, y2), _) -> y1 == y2) $ Grid.elems squares

type Parser = Parsec Void Text

decode :: FilePath -> Text -> Either String Game
decode filepath msg = case runParser parser filepath msg of
  Right game -> Right game
  Left err -> Left $ errorBundlePretty err
  where
    parser :: Parser Game
    parser = do
      -- Parse game parameters.
      version <- natL
      width <- natL
      height <- natL
      mineCount <- natL
      seed <- intL
      _ <- newline

      -- Parse frames.
      frames <- someTill (frame (width, height)) eof

      let params = Parameters {version, width, height, mineCount, seed}
      return
        Game
          { parameters = params,
            minefield = makeMinefield params,
            frames
          }

    nat :: Parser Int
    nat = read <$> some numberChar

    natL :: Parser Int
    natL = nat <* newline

    int :: Parser Int
    int = signed (pure ()) nat

    intL :: Parser Int
    intL = int <* newline

    charL :: Char -> Parser Char
    charL c = char c <* newline

    frame :: (Int, Int) -> Parser Frame
    frame (width, height) = do
      status <- (Playing <$ charL 'P') <|> (Won <$ charL 'W') <|> (Lost <$ charL 'L')
      lastMove <- action
      created <- posixSecondsToUTCTime . fromInteger . toInteger <$> (nat <* newline)
      squares <- Grid.fromList (width, height) <$> squareRows
      _ <- newline
      return Frame {status, lastMove, created, squares}
      where
        action :: Parser Action
        action =
          (Start <$ charL 'S')
            <|> (Dig <$> (charL 'D' *> position))
            <|> (Flag <$> (charL 'F' *> position))
          where
            position :: Parser (Int, Int)
            position = do
              x <- nat <* newline
              y <- nat <* newline
              return (x, y)

        squareRows :: Parser [Square]
        squareRows = concat <$> replicateM height row
          where
            row :: Parser [Square]
            row = replicateM width square <* newline

            square :: Parser Square
            square =
              (Unrevealed <$ char '?')
                <|> (Flagged <$ char 'F')
                <|> (Exploded <$ char 'X')
                <|> (Revealed 0 <$ char ' ')
                <|> (Revealed . read . replicate 1 <$> numberChar)
