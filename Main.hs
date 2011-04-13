module Main where

import Graphics.Rendering.OpenGL
import qualified Graphics.UI.GLUT as GLUT
import Data.IORef
import Data.Array.IArray
import Conway

black = Color4 0 0 0 1 :: Color4 GLclampf
white = Color4 1 1 1 1 :: Color4 GLclampf
red = Color4 1 0 0 1 :: Color4 GLclampf

dead = Color4 1 1 1 0.15 :: Color4 GLclampf
blue = Color4 0 0 1 1 :: Color4 GLclampf

main :: IO ()
main = do
  GLUT.initialDisplayMode $= [ GLUT.DoubleBuffered , GLUT.RGBAMode ]
  (_progname, _args) <- GLUT.getArgsAndInitialize

  let liveCells = [ ((3,3),True)
                  , ((4,3),True)
                  , ((5,3),True)
                  ]

  let g = (grid (20,20)) // liveCells

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
drawCell ((col, row), state) = do
  let x = fromIntegral col
  let y = fromIntegral row


  let padding = 0.05
  let v1 = Vertex2 (x + padding) (y + padding) :: Vertex2 GLfloat
  let v2 = Vertex2 (x + 1 - padding) (y + 1 - padding) :: Vertex2 GLfloat

  let c = case state of
            True -> blue
            otherwise -> dead
  color c

  rect v1 v2

display :: IORef Grid -> IO ()
display gridRef = do
  clear [ColorBuffer]
  g <- readIORef gridRef

  preservingMatrix $ do
    let (cols, rows) = size g
    let width = fromIntegral cols
    let height = fromIntegral rows

    -- zoom out
    let sX = 2 / width :: GLfloat
    let sY = 2 / height :: GLfloat
    scale sX sY 0

    -- move to the center
    let origin = Vector3 (width/(-2) - 1) (height/(-2) - 1) 0 :: Vector3 GLfloat
    translate origin

    mapM_ drawCell . assocs $ g

  GLUT.swapBuffers
