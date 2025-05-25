Module to define my maze

> module MyMaze (
>   Maze,
>   makeMaze,
>   hasWall,
>   sizeOf
> )
> where

> import Geography 
> data Maze = MyMaze Size [Place] [Place] [Place] [Place] deriving Show

wBoundary, eBoundary, sBoundary, nBoundary is the same as in maze
Then I define nWalls, sWalls, eWalls, wWalls as all positions that have a wall in that direction
I need to then reflect all the walls passed into makeMaze as well as the boundary walls to be able to add them to nWalls, eWalls...
    - to do this I will define variables nDefined, sDefined, eDefined, wDefined where I will filter all the walls passed into the function that refer to that direction

> makeMaze :: Size -> [Wall] -> Maze
> makeMaze (x,y) walls =
>   let wBoundary = [(0,j) | j <- [0..y-1]]
>       eBoundary = [(x-1,j) | j <- [0..y-1]]
>       sBoundary = [(i,0) | i <- [0..x-1]]
>       nBoundary = [(i,y-1) | i <- [0..x-1]]
>       nWalls    = nDefined ++ nBoundary ++ map (reflect S) (sDefined ++ sBoundary)
>       sWalls    = sDefined ++ sBoundary ++ map (reflect N) (nDefined ++ nBoundary)
>       eWalls    = eDefined ++ eBoundary ++ map (reflect W) (wDefined ++ wBoundary)
>       wWalls    = wDefined ++ wBoundary ++ map (reflect E) (eDefined ++ eBoundary)
>       nDefined  = [fst wall | wall <- walls, N `elem` wall]
>       sDefined  = [fst wall | wall <- walls, S `elem` wall]
>       eDefined  = [fst wall | wall <- walls, E `elem` wall]
>       wDefined  = [fst wall | wall <- walls, W `elem` wall]
>   in MyMaze (x,y) nWalls sWalls eWalls wWalls

For the reflect function we need to pass the place and the direction because they are no longer together in a pair

> reflect :: Direction -> Place -> Place
> reflect d (i,j) = (move d (i,j))

The following function tests whether the maze includes a wall in a particular
direction from a particular place.
Simply check if the position is in the list of walls in that direction

> hasWall :: Maze -> Place -> Direction -> Bool
> hasWall (MyMaze _ nWalls sWalls eWalls wWalls) pos d | d == N   = pos `elem` nWalls
>                                                      | d == S   = pos `elem` sWalls
>                                                      | d == E   = pos `elem` eWalls
>                                                      | d == W   = pos `elem` wWalls

Simply return the size from MyMaze

> sizeOf :: Maze -> Size
> sizeOf (MyMaze size _ _ _ _) = size


TESTING:

Using MyMaze:
ghci> fastSolveMaze smallMaze (0,0) (3,2)
[E,N,E,S,E,N,N]
(0.00 secs, 158,264 bytes)

ghci> fastSolveMaze largeMaze (0,0) (22,21)
[N,N,N,N,N,N,N,N,N,E,E,E,N,W,W,W,N,E,E,E,N,W,W,W,N,E,E,E,E,E,N,N,N,W,S,S,W,W,W,W,N,N,N,E,S,S,E,E,N,W,N,N,W,W,N,E,E,E,E,E,E,N,W,W,W,W,W,W,N,E,E,E,E,E,E,E,S,S,S,S,E,E,N,N,N,N,E,E,E,E,S,W,W,W,S,S,S,E,N,N,E,E,E,S,W,W,S,S,W,W,W,W,W,S,E,E,E,S,W,W,W,S,S,S,E,S,S,S,E,N,N,N,E,S,S,S,S,W,W,W,S,E,E,E,S,W,W,W,S,E,E,E,E,S,S,E,E,E,E,E,E,E,S,E,E,E,N,W,W,N,N,N,E,S,S,E,E,N,W,N,E,N,N,W,S,W,W,W,W,S,W,N,N,N,W,W,W,N,N,N,E,S,S,E,N,N,N,W,W,N,N,N,N,N,E,S,S,S,S,E,E,E,E,E,E,E,S,W,W,W,W,W,S,E,E,E,E,E,E,N,N,N,W,W,W,W,N,E,E,N,W,W,N,E,E,N,W,W,W,N,N,N,E,S,S,E,N,N,E,E,E]
(0.10 secs, 35,941,712 bytes)

Using Maze:
ghci> fastSolveMaze smallMaze (0,0) (3,2)
[E,N,E,S,E,N,N]
(0.02 secs, 150,320 bytes)

ghci> fastSolveMaze largeMaze (0,0) (22,21)
[N,N,N,N,N,N,N,N,N,E,E,E,N,W,W,W,N,E,E,E,N,W,W,W,N,E,E,E,E,E,N,N,N,W,S,S,W,W,W,W,N,N,N,E,S,S,E,E,N,W,N,N,W,W,N,E,E,E,E,E,E,N,W,W,W,W,W,W,N,E,E,E,E,E,E,E,S,S,S,S,E,E,N,N,N,N,E,E,E,E,S,W,W,W,S,S,S,E,N,N,E,E,E,S,W,W,S,S,W,W,W,W,W,S,E,E,E,S,W,W,W,S,S,S,E,S,S,S,E,N,N,N,E,S,S,S,S,W,W,W,S,E,E,E,S,W,W,W,S,E,E,E,E,S,S,E,E,E,E,E,E,E,S,E,E,E,N,W,W,N,N,N,E,S,S,E,E,N,W,N,E,N,N,W,S,W,W,W,W,S,W,N,N,N,W,W,W,N,N,N,E,S,S,E,N,N,N,W,W,N,N,N,N,N,E,S,S,S,S,E,E,E,E,E,E,E,S,W,W,W,W,W,S,E,E,E,E,E,E,N,N,N,W,W,W,W,N,E,E,N,W,W,N,E,E,N,W,W,W,N,N,N,E,S,S,E,N,N,E,E,E]
(0.14 secs, 36,256,208 bytes)

The time taken and memory space needed is lower for both using the new representation.
