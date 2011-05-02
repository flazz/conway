module Input where

import Data.IORef
import Graphics.UI.GLUT
import Data.Char
import System.Exit

import Display

handleInput stateRef (Char 'q') Down modifiers position = exitSuccess
handleInput stateRef (Char c) Down modifiers position | isSpace c = modifyIORef stateRef toggleState
handleInput stateRef _ _ modifiers position = return ()

toggleState (Run g) = Modify g
toggleState (Modify g) = Run g

handleMovement stateRef (Position px py) = do
  state <- readIORef stateRef
  case state of
    Modify g -> do
      vp <- get viewport
      model <- get . matrix $ Just (Modelview 0) :: IO (GLmatrix GLdouble)
      proj <- get . matrix $ Just Projection :: IO (GLmatrix GLdouble)
      let px' = fromIntegral px
      let py' = fromIntegral py
      Vertex3 x y z <- unProject (Vertex3 px' py' 0) model proj vp
      putStrLn . show $ (round x, round y)
    otherwise -> return ()
