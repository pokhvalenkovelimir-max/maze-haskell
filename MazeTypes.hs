module MazeTypes where

import qualified Data.Set as Set
import System.Random

-- stores size and probability of crossroad
data Config = Config
  { width  :: Int
  , height :: Int
  , loops :: Int
  , long :: Bool
  }

-- coordinates of one cell
type Cell = (Int, Int)
-- tuple of two cells define an edge
type Edge = (Cell, Cell)

type Visited = Set.Set Cell
type Edges = Set.Set Edge

-- helping data structure to avoid writing same three variables
-- used in dfs
data State = MazeState
    { visited      :: Visited
    , carvedPaths  :: Edges
    , randomSeed   :: StdGen
    }