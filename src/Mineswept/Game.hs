module Mineswept.Game
  ( Status (..),
    Square (..),
    Frame (..),
    makeFrame,
    initialFrame,
    Game (..),
    Parameters (..),
    initialGame,
    Action (..),
    step,
  )
where

import Data.List (find, intercalate)
import Data.List.NonEmpty (NonEmpty (..), (<|))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (isJust)
import Data.Time (UTCTime)
import Mineswept.Grid (Grid)
import Mineswept.Grid qualified as Grid
import Mineswept.Internal.PShow (PShow (..))
import Mineswept.Minefield (Minefield, Parameters (..), Tile (..), makeMinefield, reveal)
import Mineswept.Minefield qualified as Minefield

data Status
  = Playing
  | Won
  | Lost
  deriving (Show)

data Square
  = Unrevealed
  | Revealed Int
  | Flagged
  | Exploded
  deriving (Eq)

instance PShow Square where
  pshow Unrevealed = "?"
  pshow (Revealed n) = if n == 0 then " " else show n
  pshow Flagged = "F"
  pshow Exploded = "X"

data Frame = Frame
  { status :: Status,
    squares :: Grid Square,
    lastMove :: Action,
    created :: UTCTime
  }

indent :: Int -> String -> String
indent depth s = intercalate "\n" $ (\line -> replicate depth ' ' ++ line) <$> lines s

{- ORMOLU_DISABLE -}
instance PShow Frame where
  pshow Frame{..} =
       "Turn {\n"
    ++ "  status: " ++ show status ++ "\n"
    ++ "  last move: " ++ show lastMove ++ "\n"
    ++ "  timestamp: " ++ show created ++ "\n"
    ++ "  squares: {\n"
    ++ indent 4 (pshow squares) ++ "\n"
    ++ "  }\n"
    ++ "}\n"
{- ORMOLU_ENABLE -}

makeFrame :: Grid Square -> Action -> UTCTime -> Frame
makeFrame g a ts = Frame status g a ts
  where
    squares = snd <$> Grid.elems g
    exploded = find (== Exploded) squares
    unrevealed = find (== Unrevealed) squares
    status
      | isJust exploded = Lost
      | isJust unrevealed = Playing
      | otherwise = Won

initialFrame :: (Int, Int) -> UTCTime -> Frame
initialFrame (width, height) = makeFrame squares Start
  where
    squares = Grid.fromList (width, height) $ replicate (width * height) Unrevealed

data Game = Game
  { width :: Int,
    height :: Int,
    seed :: Int,
    version :: Int,
    minefield :: Minefield,
    frames :: NonEmpty Frame
  }

{- ORMOLU_DISABLE -}
instance PShow Game where
  pshow Game {..} =
       "Game {\n"
    ++ "  width: " ++ show width ++ "\n"
    ++ "  height: " ++ show height ++ "\n"
    ++ "  seed: " ++ show seed ++ "\n"
    ++ "  version: " ++ show version ++ "\n"
    ++ "  minefield: {\n"
    ++ indent 4 (pshow minefield) ++ "\n"
    ++ "  }\n"
    ++ "  turns: {\n"
    ++ indent 4 (concat $ pshow <$> NE.reverse frames) ++ "\n"
    ++ "  }\n"
    ++ "}"
{- ORMOLU_ENABLE -}

initialGame :: Parameters -> UTCTime -> Game
initialGame params@Parameters {width, height, seed} ts =
  Game
    { width,
      height,
      seed,
      version = 1,
      minefield = makeMinefield params,
      frames = initialFrame (width, height) ts :| []
    }

data Action
  = Start
  | Dig (Int, Int)
  | Flag (Int, Int)
  deriving (Eq, Show)

step :: Game -> Action -> UTCTime -> Maybe Game
step game@Game {frames = frames@(Frame {squares} :| _), minefield} action ts = do
  nextGrid <- makeNextGrid
  Just $ game {frames = makeFrame nextGrid action ts <| frames}
  where
    makeNextGrid = case action of
      Dig pos -> dig pos
      Flag pos -> Just $ flag pos
      Start -> error "step: impossible: Start is an invalid step action"

    uncover (p, tile) grid = case tile of
      Mine -> Grid.set p Exploded grid
      Hint h -> Grid.set p (Revealed h) grid

    dig pos = do
      tile <- Minefield.get pos minefield
      revealed <- case tile of
        Mine -> Just [(pos, tile)]
        -- FIXME: bug - never reveal flagged tiles, even when 0-adjacent
        Hint _ -> reveal pos minefield
      Just $ foldr uncover squares revealed

    flag pos = Grid.set pos Flagged squares
