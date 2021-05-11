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
  { parameters :: Parameters,
    minefield :: Minefield,
    frames :: NonEmpty Frame
  }

{- ORMOLU_DISABLE -}
instance PShow Game where
  pshow Game {parameters = Parameters {width, height, seed, version, mineCount}, minefield, frames} =
       "Game {\n"
    ++ "  width: " ++ show width ++ "\n"
    ++ "  height: " ++ show height ++ "\n"
    ++ "  mines: " ++ show mineCount ++ "\n"
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
initialGame parameters@Parameters {width, height} ts =
  Game
    { parameters,
      minefield = makeMinefield parameters,
      frames = initialFrame (width, height) ts :| []
    }

step :: Game -> Action -> UTCTime -> Either String Game
step game@Game {frames = frames@(Frame {squares} :| _), minefield} action ts = do
  nextGrid <- makeNextGrid
  Right $ game {frames = makeFrame nextGrid action ts <| frames}
  where
    maybeToEither l m = case m of
      Just r -> pure r
      Nothing -> Left l

    makeNextGrid = case action of
      Dig pos -> dig pos
      Flag pos -> pure $ flag pos
      Start -> error "step: impossible: Start is an invalid step action"

    uncover (p, tile) grid = case tile of
      Mine -> Grid.set p Exploded grid
      Hint h -> Grid.set p (Revealed h) grid

    dig pos = do
      tile <- maybeToEither ("invalid position: " <> show pos) $ Minefield.get pos minefield
      revealed <- case tile of
        Mine -> pure [(pos, tile)]
        Hint _ -> do
          reveals <- maybeToEither ("invalid position" <> show pos) $ Minefield.reveal pos minefield
          return $ filter (\(p, _) -> fromJust (Grid.get p squares) /= Flagged) reveals
      pure $ foldr uncover squares revealed

    flag pos = Grid.set pos Flagged squares
