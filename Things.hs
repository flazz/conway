module Things where

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

