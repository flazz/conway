module Main where

import Graphics.Rendering.OpenGL
import qualified Graphics.UI.GLUT as GLUT
import Data.IORef
import Data.Array.IArray

import Conway
import Colors
import Things
import Input
import Display

period delay action = do
  action
  GLUT.addTimerCallback delay $ period delay action


update (Run g) = Run $ updateGrid g
update s = s

main :: IO ()
main = do
  GLUT.initialWindowSize $= Size 800 800
  GLUT.initialDisplayMode $= [ GLUT.DoubleBuffered , GLUT.RGBAMode ]
  (_progname, _args) <- GLUT.getArgsAndInitialize
  GLUT.createWindow "conway"
  let activeCells = glider (8,8)
  let g = grid (0xf, 0xf) // activeCells
  stateRef <- newIORef (Run g)
  GLUT.displayCallback $= display stateRef
  GLUT.keyboardMouseCallback $= Just (handleInput stateRef)

  setup

  period 30 $ do
    modifyIORef stateRef update
    GLUT.postRedisplay Nothing

  GLUT.mainLoop

setup :: IO ()
setup = do
  blend $= Enabled
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
  clearColor $= base03
