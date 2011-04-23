module Display where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.IORef
import Data.Array.IArray

import Colors
import Conway

data State = Run Grid | Modify Grid

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

display :: IORef State -> IO ()
display stateRef = do
  clear [ColorBuffer]
  st <- readIORef stateRef
  case st of
    Run g ->
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

    otherwise -> return ()

  swapBuffers
