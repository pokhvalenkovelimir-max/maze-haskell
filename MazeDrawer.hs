module MazeDrawer where

import MazeTypes
import MazeGenerator
import qualified Data.Set as Set

-- 
drawMaze :: Config -> State -> String
drawMaze cfg state = 
    let 
        w = width cfg
        h = height cfg
        paths = carvedPaths state

        topBorder = "+" ++ concat (replicate w "---+") ++ "\n"
        
        drawRow y = 
            let 
                -- first left wall is missing, because it's an entry point
                leftWall = if y == 0 then " " else "|"
                
                -- room is a row at odd indexes
                rooms = leftWall ++ concat [
                    -- in case we are at the finish, there is no wall
                    -- also if it's an edge, wall is broken, we don't paint |
                    if (x == w - 1 && y == h - 1) || Set.member (getEdge (x,y) (x+1,y)) paths 
                    then "    "
                    -- wall, in four positions
                    else "   |"
                    | x <- [0 .. w - 1] ]

                -- floor is a row at even indexes
                -- same, but vertical walls
                floors = "+" ++ concat [ 
                    if Set.member (getEdge (x,y) (x,y+1)) paths 
                    then "   +"
                    else "---+"
                    | x <- [0 .. w - 1] ]
            in 
                rooms ++ "\n" ++ floors ++ "\n"
        -- draw these layers and left wall + topBorder, others are generated
        allRows = concat [ drawRow y | y <- [0 .. h - 1] ]
    in 
        topBorder ++ allRows

-- for test result, paints just the * at broken walls edges, that create the shortest path
-- draws * for edges on each row one by one
drawOnlyPath :: Config -> [Cell] -> String
drawOnlyPath cfg path = 
    let
        pathSet = Set.fromList path
        w = width cfg
        h = height cfg

        drawRow y = concat [ if Set.member (x,y) pathSet then " * " else "   " 
                           | x <- [0 .. w - 1] ]
    in 
        concat [ drawRow y ++ "\n" | y <- [0 .. h - 1] ]