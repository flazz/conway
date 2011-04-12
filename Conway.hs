import Data.Array.IArray
import Data.Array.Unboxed
import Data.List

type GridIx = (Int, Int)
type Grid = UArray GridIx Bool

grid :: GridIx -> Grid
grid upper = listArray (lower, upper) es
  where es = repeat False
        lower = (1,1)

neighborhood :: Grid -> GridIx -> [Bool]
neighborhood g ix@(x,y) = [ g ! nx
                          | a <- xs
                          , b <- ys
                          , let nx = (a,b)
                          , nx /= ix
                          , inRange (bounds g) nx
                          ]

  where base = [-1, 0, 1]
        xs = map (+ x) base
        ys = map (+ y) base

updateCell :: Grid -> GridIx -> Bool
updateCell g ix = rules state liveNeighbors
  where state = g ! ix
        liveNeighbors = length . filter id $ neighborhood g ix

rules :: Bool -> Int -> Bool
rules True 2 = True
rules True 3 = True
rules False 3 = True
rules _ _ = False

update :: Grid -> Grid
update g = g // newAssocs
  where f = updateCell g
        ixs = indices g
        newAssocs = zip ixs (map f ixs)
