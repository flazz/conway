module Input where

import Data.IORef
import Graphics.UI.GLUT

handleInput :: IORef stateRef -> Key -> KeyState -> Modifiers -> Position -> IO ()
handleInput stateRef key state modifiers position = do
  return ()
