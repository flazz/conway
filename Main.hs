module Main where

import Graphics.Rendering.OpenGL
import qualified Graphics.UI.GLUT as GLUT
import Data.IORef
import Data.Array.IArray
import Conway

base03 = Color4 0 (43/255)  (54/255) 1 :: Color4 GLclampf
base02 = Color4 (7/255) (54/255) (66/255) 1 :: Color4 GLclampf
magenta = Color4 (211/255) (54/255) (130/255) 1 :: Color4 GLclampf
violet = Color4 (108/255) (113/255) (196/255) 1 :: Color4 GLclampf
blue = Color4 (38/255) (139/255) (210/255) 1 :: Color4 GLclampf
green = Color4 (133/255) (153/255) (0/255) 1 :: Color4 GLclampf

black = Color4 0 0 0 1 :: Color4 GLclampf
white = Color4 1 1 1 1 :: Color4 GLclampf

period delay action = do
  action
  GLUT.addTimerCallback delay $ period delay action

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

  period 150 $ do
    modifyIORef gridRef update
    GLUT.postRedisplay Nothing

  clearColor $= base03

  GLUT.mainLoop

drawCell :: (GridIx, Bool) -> IO ()
drawCell ((col, row), state) = do
  let x = fromIntegral col
      y = fromIntegral row
      v1 = Vertex2 x y :: Vertex2 GLfloat
      v2 = Vertex2 (x + 1) (y + 1) :: Vertex2 GLfloat
  rect v1 v2

hLine :: GLfloat -> GLfloat -> IO ()
hLine w y = do
  let v1 = Vertex2 1 y :: Vertex2 GLfloat
  let v2 = Vertex2 w y :: Vertex2 GLfloat

  renderPrimitive Lines $ do
    vertex v1
    vertex v2

vLine :: GLfloat -> GLfloat -> IO ()
vLine h x = do
  let v1 = Vertex2 x 1 :: Vertex2 GLfloat
  let v2 = Vertex2 x h :: Vertex2 GLfloat

  renderPrimitive Lines $ do
    vertex v1
    vertex v2

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

    color green
    let activeCell (_, state) = state
    mapM_ drawCell . filter activeCell . assocs $ g

    color base02
    mapM_ ( hLine $ width + 1 ) [1..height]
    mapM_ ( vLine $ height + 1 ) [1..width]

  GLUT.swapBuffers
