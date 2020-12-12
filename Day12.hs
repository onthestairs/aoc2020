{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day12 (solution) where

import AOC (Parser, Solution (..), parseFile, parseInt)
import Control.Lens
import Linear (V2 (V2), (*^))
import Relude hiding (Op)
import Text.Megaparsec (eof, sepBy1)
import Text.Megaparsec.Char (char, newline)

data Action = N Int | S Int | E Int | W Int | L Int | R Int | F Int deriving (Show)

type Input = [Action]

parseInput :: Parser Input
parseInput = sepBy1 parseAction newline <* eof

parseAction = do
  constructor <- (char 'N' $> N <|> char 'S' $> S <|> char 'E' $> E <|> char 'W' $> W <|> char 'L' $> L <|> char 'R' $> R <|> char 'F' $> F)
  n <- parseInt
  pure $ constructor n

data Direction = North | South | East | West

data ShipState = ShipState
  { _direction :: Direction,
    _position :: V2 Int
  }

makeLenses ''ShipState

goTowards wp n p = p + n *^ wp

goForward North n p = goTowards (V2 0 1) n p
goForward South n p = goTowards (V2 0 (-1)) n p
goForward East n p = goTowards (V2 1 0) n p
goForward West n p = goTowards (V2 (-1) 0) n p

rotateLeft 90 North = West
rotateLeft 180 North = South
rotateLeft 270 North = East
rotateLeft 90 South = East
rotateLeft 180 South = North
rotateLeft 270 South = West
rotateLeft 90 East = North
rotateLeft 180 East = West
rotateLeft 270 East = South
rotateLeft 90 West = South
rotateLeft 180 West = East
rotateLeft 270 West = North

rotateRight 90 North = East
rotateRight 180 North = South
rotateRight 270 North = West
rotateRight 90 South = West
rotateRight 180 South = North
rotateRight 270 South = East
rotateRight 90 East = South
rotateRight 180 East = West
rotateRight 270 East = North
rotateRight 90 West = North
rotateRight 180 West = East
rotateRight 270 West = South

runAction (N n) = modifying position (goForward North n)
runAction (S n) = modifying position (goForward South n)
runAction (E n) = modifying position (goForward East n)
runAction (W n) = modifying position (goForward West n)
runAction (L n) = modifying direction (rotateLeft n)
runAction (R n) = modifying direction (rotateRight n)
runAction (F n) = do
  dir <- use direction
  modifying position (goForward dir n)

runActions as = forM_ as runAction

manhattan (V2 x y) = abs x + abs y

solve1 as = manhattan $ view position $ snd $ usingState startingState (runActions as)
  where
    startingState =
      ShipState
        { _direction = East,
          _position = V2 0 0
        }

data ShipState2 = ShipState2
  { _waypoint :: V2 Int,
    _position2 :: V2 Int
  }
  deriving (Show)

makeLenses ''ShipState2

rotateWaypointLeft 90 (V2 wpx wpy) = V2 (- wpy) (wpx)
rotateWaypointLeft 180 (V2 wpx wpy) = V2 (- wpx) (- wpy)
rotateWaypointLeft 270 (V2 wpx wpy) = V2 (wpy) (- wpx)

rotateWaypointRight 90 (V2 wpx wpy) = V2 (wpy) (- wpx)
rotateWaypointRight 180 (V2 wpx wpy) = V2 (- wpx) (- wpy)
rotateWaypointRight 270 (V2 wpx wpy) = V2 (- wpy) (wpx)

runAction2 :: MonadState ShipState2 m => Action -> m ()
runAction2 (N n) = modifying waypoint (goForward North n)
runAction2 (S n) = modifying waypoint (goForward South n)
runAction2 (E n) = modifying waypoint (goForward East n)
runAction2 (W n) = modifying waypoint (goForward West n)
runAction2 (L n) = modifying waypoint (rotateWaypointLeft n)
runAction2 (R n) = modifying waypoint (rotateWaypointRight n)
runAction2 (F n) = do
  wp <- use waypoint
  modifying position2 (goTowards wp n)

runActions2 as = forM_ as runAction2

solve2 as = manhattan $ view position2 $ snd $ usingState startingState (runActions2 as)
  where
    startingState =
      ShipState2
        { _waypoint = V2 10 1,
          _position2 = V2 0 0
        }

solution =
  Solution
    { _parse = parseFile "12.txt" parseInput,
      _solve1 = solve1,
      _solve2 = solve2
    }
