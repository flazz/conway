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

hLine y = do
  vertex (Vertex2 1.0 y :: Vertex2 GLfloat)
  vertex (Vertex2 (- 1.0) y :: Vertex2 GLfloat)

vLine x = do
  vertex (Vertex2 x 1.0 :: Vertex2 GLfloat)
  vertex (Vertex2 x (- 1.0) :: Vertex2 GLfloat)

guides n = map tick [0..n]
  where half = n / 2
        tick x = (x - half) / half

main :: IO ()
main = do
  GLUT.initialDisplayMode $= [ GLUT.DoubleBuffered
                             , GLUT.RGBAMode
                             ]

  (_progname, _args) <- GLUT.getArgsAndInitialize
  gridRef <- newIORef . grid $ (3,3)
  GLUT.createWindow "demo"
  GLUT.displayCallback $= display gridRef
  {-blend $= Enabled-}
  {-blendFunc $= (SrcAlpha, OneMinusSrcAlpha)-}
  GLUT.mainLoop

rectAbout x y = do
  let v1 = Vertex2 (x - 0.1) (y - 0.1 ) :: Vertex2 GLfloat
  let v2 = Vertex2 (x + 0.1) (y + 0.1 ) :: Vertex2 GLfloat
  rect v1 v2

drawCell :: ((Int,Int), Bool) -> IO ()
drawCell ((x,y), l) = do
  let x' = (fromIntegral x - 1) / 10
  let y' = (fromIntegral y - 1) / 10
  rectAbout x' y'

display :: IORef Grid -> IO ()
display gridRef = do
  g <- readIORef gridRef
  let vs = assocs g
  preservingMatrix . mapM_ drawCell $ vs
  clear [ColorBuffer]
  GLUT.swapBuffers
