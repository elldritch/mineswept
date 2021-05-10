module Mineswept.Game
  ( Game (..),
    Parameters (..),
    initialGame,
    Action (..),
    step,
  )
where

import Data.List.NonEmpty (NonEmpty (..), (<|))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromJust)
import Data.Time (UTCTime)
import Mineswept.Frame (Action (..), Frame (..), Square (..), initialFrame, makeFrame)
import Mineswept.Grid qualified as Grid
import Mineswept.Internal.PShow (PShow (..), indent)
import Mineswept.Minefield (Minefield, Parameters (..), Tile (..), makeMinefield)
import Mineswept.Minefield qualified as Minefield

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
        Hint _ -> do
          reveals <- Minefield.reveal pos minefield
          return $ filter (\(p, _) -> fromJust (Grid.get p squares) /= Flagged) reveals
      Just $ foldr uncover squares revealed

    flag pos = Grid.set pos Flagged squares
