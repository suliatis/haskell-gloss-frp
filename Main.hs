module Main (main) where

import           Control.Concurrent        (threadDelay)
import           Control.Monad             (join, sequence, unless)
import           Control.Monad.Fix         (fix)
import           Data.List                 (find)
import           Data.Maybe                (fromMaybe)
import           FRP.Elerea.Simple         (external, start, transfer)
import           Graphics.Gloss            (color, pictures, rectangleSolid, translate)
import           Graphics.Gloss.Data.Color (black, white)
import           Graphics.Gloss.Rendering  (Picture, State, displayPicture, initState)
import           Graphics.UI.GLFW          (Key (..), Window, pollEvents, swapBuffers)
import           Player                    (Direction (..), Player (..), defaultStep, initialPlayer, movePlayer)
import           Playground                (Playground (..), defualtPlayground, isOutside)
import           System.Exit               (exitSuccess)
import           Window                    (keyIsPressed, withWindow)

playground :: Playground
playground = defualtPlayground

defaultPlayerStep :: Float
defaultPlayerStep = 10

sleepInMicros :: Int
sleepInMicros = 2000

main :: IO ()
main = do
  glossState <- initState
  (directionGen, directionSink) <- external Direction'Nope
  withWindow (width playground) (height playground) "Haskell FRP" $ \window -> do
    network <- start $ do
      direction <- directionGen
      player <- transfer initialPlayer movePlayerInPlayground direction
      return $ renderFrame window glossState <$> player
    fix $ \loop -> do
      pollEvents
      threadDelay sleepInMicros
      join network
      getDirection window directionSink
      exit <- keyIsPressed window Key'Escape
      unless exit loop
    exitSuccess

movePlayerInPlayground :: Direction -> Player -> Player
movePlayerInPlayground direction player =
  let player' = movePlayer direction player defaultStep in
      if isOutside playground player' then player else player'

getDirection :: Window -> (Direction -> IO ()) -> IO ()
getDirection window sink = do
  directions <- sequence
    [ getDirection' Key'Left Direction'Left
    , getDirection' Key'H Direction'Left
    , getDirection' Key'A Direction'Left
    , getDirection' Key'Down Direction'Down
    , getDirection' Key'J Direction'Down
    , getDirection' Key'S Direction'Down
    , getDirection' Key'Up Direction'Up
    , getDirection' Key'K Direction'Up
    , getDirection' Key'W Direction'Up
    , getDirection' Key'Right Direction'Right
    , getDirection' Key'L Direction'Right
    , getDirection' Key'D Direction'Right
    ]
  let maybeDirection = find actualDirection directions
  sink $ fromMaybe Direction'Nope maybeDirection
  where
    getDirection' key dir = do
      b <- keyIsPressed window key
      if b then return dir else return Direction'Nope
    actualDirection Direction'Nope = False
    actualDirection _              = True

renderFrame :: Window -> State -> Player -> IO ()
renderFrame window glossState player = do
  displayPicture (width playground, height playground) white glossState 1.0 $ pictures
    [ drawPlayer player ]
  swapBuffers window

drawPlayer :: Player -> Picture
drawPlayer (Player (x, y) size) =
  translate x y $ color black $ rectangleSolid size size

