module Conway (grid, grids, update) where

import Data.Array.IArray
import Data.Array.Unboxed
import Data.List

type GridIx = (Int, Int)
type Grid = UArray GridIx Bool

-- | Construct a new grid with bounds starting at (1.1) to upper
grid :: GridIx -> Grid
grid upper = listArray (lower, upper) es
  where es = repeat False
        lower = (1,1)

-- | Return a list of frames originated from a seed
grids :: Grid -> [Grid]
grids g = g:gs
  where gs = unfoldr nextGrid g
        nextGrid g | g == g' = Nothing
                   | otherwise = Just (g,g')
          where g' = update g

-- | Return a list of states about the given index
neighborhood :: Grid -> GridIx -> [Bool]
neighborhood g ix@(x,y) = [ g ! nx | a <- xs , b <- ys
                          , let nx = (a,b)
                          , nx /= ix
                          , inRange (bounds g) nx
                          ]

  where base = [-1, 0, 1]
        xs = map (+ x) base
        ys = map (+ y) base

-- | Determines the new state based on the number of living neighbors
rules :: Bool -> Int -> Bool
rules True 2 = True
rules True 3 = True
rules False 3 = True
rules _ _ = False

update :: Grid -> Grid
update g = g // newAssocs
  where uc = updateCell g
        ixs = indices g
        newAssocs = zip ixs (map uc ixs)

updateCell :: Grid -> GridIx -> Bool
updateCell g ix = rules state liveNeighbors
  where state = g ! ix
        liveNeighbors = length . filter id $ neighborhood g ix
