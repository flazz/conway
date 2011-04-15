module Main where

import Graphics.Rendering.OpenGL
import qualified Graphics.UI.GLUT as GLUT
import Data.IORef
import Data.Array.IArray

import Conway
import Colors

period delay action = do
  action
  GLUT.addTimerCallback delay $ period delay action

data State = Run Grid | Modify Grid GridIx

update (Run g) = Run $ updateGrid g
update s = s

hBlinker (x,y) = [ ((x-1,y),True)
                 , ((x,y),True)
                 , ((x+1,y),True)
                 ]

vBlinker (x,y) = [ ((x,y-1),True)
                 , ((x,y),True)
                 , ((x,y+1),True)
                 ]

glider (x,y) = [ ((x,y+1), True)
               , ((x,y-1), True)
               , ((x+1,y), True)
               , ((x-1,y-1), True)
               , ((x+1,y-1), True)
               ]

dieHard = [ ((10,22),True)
          , ((11,21),True)
          , ((11,22),True)
          , ((15,21),True)
          , ((16,21),True)
          , ((17,21),True)
          , ((16,23),True)
          ]

fPentomino = [ ((6,6),True)
             , ((6,7),True)
             , ((7,6),True)
             , ((7,7),True)
             , ((8,8),True)
             , ((8,9),True)
             , ((9,8),True)
             , ((9,9),True)
             , ((10,12),True)
             , ((11,11),True)
             , ((11,12),True)
             , ((11,13),True)
             , ((14,13),True)
             ]

main :: IO ()
main = do
  GLUT.initialWindowSize $= Size 800 800
  GLUT.initialDisplayMode $= [ GLUT.DoubleBuffered , GLUT.RGBAMode ]
  (_progname, _args) <- GLUT.getArgsAndInitialize
  GLUT.createWindow "conway"


  let activeCells = glider (8,8)
  --let activeCells = dieHard

  let g = grid (0xf, 0xf) // activeCells

  stateRef <- newIORef (Run g)

  GLUT.displayCallback $= display stateRef
  {-GLUT.keyboardMouseCallback $= Just (handleInput gameRef)-}

  blend $= Enabled
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)

  period 30 $ do
    modifyIORef stateRef update
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

  GLUT.swapBuffers
