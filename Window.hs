module Window
  ( withWindow
  , keyIsPressed
  )
where

import           Control.Monad    (when)
import           Graphics.UI.GLFW (Key (Key'Escape), KeyState (KeyState'Pressed, KeyState'Repeating), Window,
                                   createWindow, destroyWindow, getKey, makeContextCurrent, pollEvents,
                                   setErrorCallback, swapBuffers, terminate)
import qualified Graphics.UI.GLFW as GLFW (init)

withWindow :: Int -> Int -> String -> (Window -> IO ()) -> IO ()
withWindow width height title render = do
  setErrorCallback $ Just simpleErrorCallback
  success <- GLFW.init
  when success $ do
    maybeWindiow <- createWindow width height title Nothing Nothing
    case maybeWindiow of
      Just window -> do
        makeContextCurrent maybeWindiow
        render window
        setErrorCallback $ Just simpleErrorCallback
        destroyWindow window
      Nothing -> return ()
    terminate
  where simpleErrorCallback e msg = putStrLn $ unwords [show e, show msg]

keyIsPressed :: Window -> Key -> IO Bool
keyIsPressed window key = isPress `fmap` getKey window key

isPress :: KeyState -> Bool
isPress KeyState'Pressed   = True
isPress KeyState'Repeating = True
isPress _                  = False
