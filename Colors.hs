module Colors where

import Graphics.Rendering.OpenGL

black = Color4 0 0 0 1 :: Color4 GLclampf
white = Color4 1 1 1 1 :: Color4 GLclampf

base03 = Color4 0 (43/255)  (54/255) 1 :: Color4 GLclampf
base02 = Color4 (7/255) (54/255) (66/255) 1 :: Color4 GLclampf
magenta = Color4 (211/255) (54/255) (130/255) 1 :: Color4 GLclampf
violet = Color4 (108/255) (113/255) (196/255) 1 :: Color4 GLclampf
blue = Color4 (38/255) (139/255) (210/255) 1 :: Color4 GLclampf
green = Color4 (133/255) (153/255) (0/255) 1 :: Color4 GLclampf
