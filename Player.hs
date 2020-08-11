module Player
  ( Player (..)
  , Step (..)
  , Direction (..)
  , defaultStep
  , initialPlayer
  , movePlayer
  )
where

import           Graphics.Gloss (Point)

data Player = Player
  { position :: Point
  , size     :: Float
  }

data Step = Step Float

initialPlayer :: Player
initialPlayer = Player (0,0) 20

defaultStep :: Step
defaultStep = Step 10

data Direction
  = Direction'Left
  | Direction'Down
  | Direction'Up
  | Direction'Right
  | Direction'Nope

movePlayer :: Direction -> Player -> Step -> Player
movePlayer direction (Player (x, y) size) (Step step) =
    case direction of
      Direction'Left  -> Player (x - step, y) size
      Direction'Down  -> Player (x, y - step) size
      Direction'Up    -> Player (x, y + step) size
      Direction'Right -> Player (x + step, y) size
      Direction'Nope  -> Player (x, y) size

