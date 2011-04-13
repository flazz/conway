module Main where

import Graphics.Rendering.OpenGL
import qualified Graphics.UI.GLUT as GLUT
import Data.IORef
import Data.Array.IArray
import Conway

black = Color4 0 0 0 1 :: Color4 GLclampf
white = Color4 1 1 1 1 :: Color4 GLclampf
blue = Color4 0 0 1 1 :: Color4 GLclampf
red = Color4 1 0 0 1 :: Color4 GLclampf

dead = Color4 1 1 1 0.15 :: Color4 GLclampf

main :: IO ()
main = do
  GLUT.initialDisplayMode $= [ GLUT.DoubleBuffered , GLUT.RGBAMode ]
  (_progname, _args) <- GLUT.getArgsAndInitialize
  let g = (grid (3,3)) // [((1,2),True)
                          ,((2,2),True)
                          ,((3,2),True)
                          ]
  gridRef <- newIORef g
  GLUT.createWindow "conway"
  GLUT.displayCallback $= display gridRef

  blend $= Enabled
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)

  period 200 $ do
    modifyIORef gridRef update
    GLUT.postRedisplay Nothing

  GLUT.mainLoop

period delay action = do
  action
  GLUT.addTimerCallback delay $ period delay action

drawCell :: (GridIx, Bool) -> IO ()
drawCell ((x,y), alive) = do

  if alive
    then color blue
    else color dead

  let x' = (fromIntegral x - 2) * 0.6
  let y' = (fromIntegral y - 2) * 0.6
  let v1 = Vertex2 (x' - 0.25 ) (y' - 0.25 ) :: Vertex2 GLfloat
  let v2 = Vertex2 (x' + 0.25 ) (y' + 0.25 ) :: Vertex2 GLfloat
  rect v1 v2

display :: IORef Grid -> IO ()
display gridRef = do
  clear [ColorBuffer]
  g <- readIORef gridRef
  let vs = assocs g
  preservingMatrix $ do
    mapM_ drawCell vs
  GLUT.swapBuffers
