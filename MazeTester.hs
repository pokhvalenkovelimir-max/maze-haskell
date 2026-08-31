module MazeTester where

import MazeTypes
import MazeGenerator
import qualified Data.Set as Set

type Path = [Cell]

-- runs bfs to find shortest path
-- cycles created by crossroads will provide alternative ways
-- shortest one counts as main
findPath :: Config -> State -> (Path, Int)
findPath cfg state =
    -- defining starting state
    let startCell = (0, 0)
        targetCell = (width cfg - 1, height cfg - 1)
        startQueue = [[startCell]]
        visited = Set.singleton startCell
        validEdges = carvedPaths state
        finalPath = bfs validEdges targetCell startQueue visited
    in (finalPath, length finalPath)

-- Edges to choose from, finish, current queue conatining all paths so far, already visited cells
-- returns shortest path
bfs :: Set.Set (Cell, Cell) -> Cell -> [Path] -> Set.Set Cell -> Path
-- if we have nothing in paths, none to process, then no way exists, that doesn't happen
-- in case of a tree, one shall always be
bfs _ _ [] _ = []
-- take fist element from known paths, find other ways, store all of them
bfs validEdges finish (currentPath@(currentCell:_):restQueue) visited
    -- once we have our way tp finish, return it
    | currentCell == finish = reverse currentPath
    -- otherwise continue bfs
    | otherwise =
        let neighbors = getNeighbours currentCell validEdges visited
            newPaths = [ n : currentPath | n <- neighbors ]
            newVisited = foldr Set.insert visited neighbors
            newQueue = restQueue ++ newPaths
        in bfs validEdges finish newQueue newVisited

-- cell we are at right now, all edges, visited edges
-- returns cells to visit
getNeighbours :: Cell -> Set.Set (Cell, Cell) -> Set.Set Cell -> [Cell]
-- for aech neighbor, check if edge between it and current cell exists and
-- and if this neighbor is not visited, add it, this also checks if neihbor
-- is withing boundaries of array
getNeighbours (x,y) validEdges visitedSet =
    let allDirections = [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]
    in [ n | n <- allDirections
           , Set.member (getEdge (x,y) n) validEdges
           , not (Set.member n visitedSet) ]

-- because we deal with a tree, each new edge which is not in n - 1 edges is a crossroad
-- but we still have to check for each cell, because crossroad may add two new edges,
-- this way amount of these is almost always less than given procent
countCrossroads :: Config -> State -> Int
countCrossroads cfg state =
    let paths = carvedPaths state
        w = width cfg
        h = height cfg
        
        -- gets amount of edges for one cell
        brokenWalls (x,y) = length [ n | n <- [(x+1,y), (x-1,y), (x,y+1), (x,y-1)]
                                 , Set.member (getEdge (x,y) n) paths ]
                                 
        -- gets amount of cells with 3 or 4 edges from them
        crossroads = [ (x,y) | x <- [0..w-1], y <- [0..h-1], brokenWalls (x,y) > 2 ]
    in 
        length crossroads