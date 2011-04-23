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
