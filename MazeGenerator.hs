module MazeGenerator where

import MazeTypes
import qualified Data.Set as Set
import System.Random
import Data.Graph (edges)

-- DFS carves a perfect tree in the graph of size given by user
dfs :: Config -> Cell -> State -> State

-- configuration - we use probablility of crossroads and size of the maze later in helper functions
-- currentCell   - for it we search neighbours and shuffle those for randomness
-- state         - just easier way to remember three variables
dfs cfg currentCell state = 
    let
        -- extracting variables from data structure
        visitedNow = visited state
        seedNow    = randomSeed state

        -- add new visited cell to state
        newVisited       = Set.insert currentCell visitedNow
        stateWithVisited = state { visited = newVisited }

        -- get neigbors and update unvisited
        validNeighbors = getValidNeighbors cfg currentCell
        unvisited      = [ n | n <- validNeighbors, not (Set.member n newVisited) ]
        
        -- shuffling for randomness and update state's ceed
        (shuffledNeighbors, newSeed) = shuffle unvisited seedNow
        newState = stateWithVisited { randomSeed = newSeed}
    in 
        -- processNeighbors later calls dfs, so a recursive call
        processNeighbors cfg currentCell shuffledNeighbors newState

-- this one does writing edges logic and builds the response
-- parameters are configuration, currentCell, cells to look at, current state
-- returns new state
processNeighbors :: Config -> Cell -> [Cell] -> State -> State

-- base case, we don't have any new cells to visit, backtrack
processNeighbors cfg currentCell [] state = state

-- there is at least one neighbor to look at, process it
processNeighbors cfg currentCell (x:rest) state = 
    -- because we always choose one cell and propagate, it will
    -- eventually get to processed neighbor
    if Set.member x (visited state) then
        processNeighbors cfg currentCell rest state
    else
        -- if unvisited, we break a wall between and make a recursive call
        let
            stateNewEdge  = carveWall currentCell x state
            stateAfterDfs = dfs cfg x stateNewEdge
        in
            processNeighbors cfg currentCell rest stateAfterDfs

bfs :: Config -> State -> [Cell] -> State
bfs _ state [] = state
bfs cfg state (currentCell:rest) = 
    let
        validNeighbors = getValidNeighbors cfg currentCell
        unvisited = [ n | n <- validNeighbors, not (Set.member n (visited state)) ]
    in 
        if unvisited == [] 
        then 
            bfs cfg state rest
        else 
            let
                -- we shuffle all roads, so this is not a normal bfs
                -- anyway, when doing this we don't build road by road, but
                -- propagate each road almost evenly
                -- almost, because further from start there are more new cells
                -- therefore it acts like building one road and building branches from it
                (picked:_, newSeed) = shuffle unvisited (randomSeed state)
                
                stateNewEdge = carveWall currentCell picked (state { randomSeed = newSeed })
                stateVisited = stateNewEdge { visited = Set.insert picked (visited stateNewEdge) }
                
                -- we keep these cells, because we might propagate once from them, but in case all neighbors
                -- of one cell are propagated in another direction, it will stay isolated, so we throw out
                -- cells from queue only in case it has no new neighbors
                newQueue = picked : currentCell : rest
                
                (randomizedQueue, finalSeed) = shuffle newQueue (randomSeed stateVisited)
                finalState = stateVisited { randomSeed = finalSeed }
            in 
                bfs cfg finalState randomizedQueue

-- list to shuffle, current seed, new list and seed
shuffle :: [Cell] -> StdGen -> ([Cell], StdGen)
-- nothing to shuffle, base case
shuffle [] seed = ([], seed)
-- choose number randomly and extract from Set, call function again, then append to what we have already
shuffle list seed = 
    let (idx, nextSeed) = randomR (0, length list - 1) seed
        (before, picked : after) = splitAt idx list
        rest = before ++ after
        (shuffledRest, finalSeed) = shuffle rest nextSeed
    in 
        (picked : shuffledRest, finalSeed) 

-- inserts an edge to state's edges
carveWall :: Cell -> Cell -> State -> State
carveWall x y state = state { carvedPaths = newPaths }
    where
        edge = getEdge x y
        newPaths = Set.insert edge (carvedPaths state)

createLoops :: [Edge] -> State -> Config -> State
createLoops [] state cfg = state

createLoops ((cell1, cell2):edges) state cfg =
    let (chance, nextSeed) = randomR (1, 100 :: Int) (randomSeed state)
        stateWithSeed = state { randomSeed = nextSeed }
    in 
        -- once generated numbers is less then chance of crossroad, carve one
        -- from cell1 to cell2
        -- if not, same call without that neighbor
        if (chance <= loops cfg) then
            let stateWithLoop = carveWall cell1 cell2 stateWithSeed
            in createLoops edges stateWithLoop cfg
        else
            createLoops edges stateWithSeed cfg

-- within array's boundaries, define neighbors on top, right, bottom, left
getValidNeighbors :: Config -> Cell -> [Cell]
getValidNeighbors cfg (x, y) =
    [ (nx, ny) | (nx, ny) <- [(x, y-1), (x+1, y), (x, y+1), (x-1, y)], isValid cfg (nx, ny)]

isValid :: Config -> Cell -> Bool
isValid cfg (x, y) = x >= 0 && x < width cfg && y >= 0 && y < height cfg

-- sorting, so that we don't add two same edges 
-- (break same two walls)
getEdge :: Ord b => b -> b -> (b, b)
getEdge a b = (min a b, max a b)