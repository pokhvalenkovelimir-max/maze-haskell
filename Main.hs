module Main where

import MazeTypes
import MazeGenerator
import MazeDrawer
import MazeTester
import System.Random
import System.IO (hFlush, stdout) 
import qualified Data.Set as Set
import System.Environment

-- interactive 
main :: IO ()
main = do
    -- on input four values, width, height, percent and isLong are expected separated with spaces
    args <- getArgs
    if length args /= 4 then
        putStrLn "Input shall be <width> <height> <loops> <isLong (1/0)"
    else do
        let [wStr, hStr, lStr, longStr] = args
            w      = read wStr :: Int
            h      = read hStr :: Int
            l      = read lStr :: Int
            longInt = read longStr :: Int
            isLong  = longInt /= 0
        initialSeed <- newStdGen

        let 
            cfg = Config w h l isLong
            emptyState = MazeState { visited = Set.empty, carvedPaths = Set.empty, randomSeed = initialSeed }
            stateWithTree = if long cfg 
                            then dfs cfg (0, 0) emptyState
                            else bfs cfg emptyState [(0, 0)]

            allPossibleEdges = Set.fromList 
                [ getEdge (x, y) n 
                | x <- [0 .. width cfg - 1]
                , y <- [0 .. height cfg - 1]
                , n <- getValidNeighbors cfg (x, y) 
                ]

            uncarvedPaths = Set.toList (Set.difference allPossibleEdges (carvedPaths stateWithTree))
            finalState = createLoops uncarvedPaths stateWithTree cfg
            (pathStartFinish, steps) = findPath cfg finalState

        putStrLn "\nMaze:"
        putStrLn (drawMaze cfg finalState)
        putStrLn "\nThe Solution Path:"
        putStrLn (drawOnlyPath cfg pathStartFinish)
        putStrLn $ "Path length: " ++ show steps
        putStrLn $ "Total crossroads: " ++ show (countCrossroads cfg finalState)
