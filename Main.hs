module Main where

import MazeTypes
import MazeGenerator
import MazeDrawer
import MazeTester
import System.Random
import System.IO (hFlush, stdout) 
import qualified Data.Set as Set

-- interactive 
main :: IO ()
main = do
    -- on input three values, width, height, percent are expected separated with spaces
    input <- getLine
    let [w, h, l, lengthModificator] = map read (words input) :: [Int]
    initialSeed <- newStdGen

    let 
        cfg = Config w h l lengthModificator
        emptyState = MazeState Set.empty Set.empty initialSeed
        finalState = dfs cfg (0, 0) emptyState
        (pathStartFinish, steps) = findPath cfg finalState

    putStrLn "\nMaze:"
    putStrLn (drawMaze cfg finalState)
    putStrLn "\nThe Solution Path:"
    putStrLn (drawOnlyPath cfg pathStartFinish)
    putStrLn $ "Path length: " ++ show steps
    putStrLn $ "Total crossroads: " ++ show (countCrossroads cfg finalState)
