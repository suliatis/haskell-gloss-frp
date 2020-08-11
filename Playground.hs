module Playground
  ( Playground(..)
  , defualtPlayground
  , isOutside
  )
where

import           Player (Player (..))

data Playground = Playground
  { width  :: Int
  , height :: Int
  }

defualtPlayground :: Playground
defualtPlayground = Playground 640 480

isOutside :: Playground -> Player -> Bool
isOutside (Playground width height) (Player (x, y) size) =
  x > fromIntegral width / 2 - size / 2
  || x < -(fromIntegral width / 2 - size / 2)
  || y > fromIntegral height / 2 - size / 2
  || y < -(fromIntegral height / 2 - size / 2)
