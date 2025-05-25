> import Geography

> import BinaryMaze

import Maze

import MyMaze


======================================================================

Draw a maze.

***************************************
*              Question 2             *
* Complete the definition of drawMaze *
***************************************

I use a list comprehension that goes over the indexes for every row in the maze by (snd (sizeOf maze))-1
I then reverse the list the row indexes are taken from because I want to build the maze from the top down (greatest index to the smallest)
Then I call my subsidiary functions with the corresponding row and I call drawNSWalls before drawEWWalls because I have made my drawEWWalls to draw the row of walls below the current row
Then I concatenate this list comprehension to get a String containing the maze
However, it is missing the very top line because of the way that I defined my drawEWWalls function so I call it one more time on row 0 (as it will also be a row of full walls) and add it to the maze string
Finally I output this String giving the intended maze

> drawMaze :: Maze -> IO()
> drawMaze maze = putStr((drawEWWalls maze 0) ++ "\n" ++ concat([(drawNSWalls maze row) ++ "\n" ++ (drawEWWalls maze row) ++ "\n" | row <- reverse [0 .. ((snd (sizeOf maze))-1)]]))


Defining the 2 subsidiary functions:
First the one that draws a row of east-west oriented walls:
For each box if there is a wall below it and then draw one if there is("--"), otherwise put 2 spaces 
to get the number of columns we do (fst (sizeOf maze)) - 1 because it is 0 indexed 

> drawEWWalls :: Maze -> Int -> String
> drawEWWalls maze row = concat(["+"] ++ [if (hasWall maze (column,row) S) then "--+" else "  +" | column <- [0 .. ((fst (sizeOf maze))-1)]])

Secondly, the one that draws a row of north-south oriented walls, and the spaces between them:
For each box in the row check if there is a wall to the right of it and then draw a wall ("|"), otherwise put 2 spaces

> drawNSWalls :: Maze -> Int -> String
> drawNSWalls maze row = concat (["|"] ++ [if (hasWall maze (column,row) E) then "  |" else "   " | column <- [0 .. ((fst (sizeOf maze))-1)]])

======================================================================

Solve the maze, giving a result of type:

> type Path = [Direction]

***************************************
*            Questions 3--4           *
*     Complete the definition of      *
*              solveMaze              *
***************************************

> solveMaze :: Maze -> Place -> Place -> Path
> solveMaze maze start target = solveMazeIter maze target [(start, [])]

QUESTION 3:
If the current position is not the target position I append a list comprehension of all possible next positions to the current list of positions to be checked
To generate this list of new positions I just check if there is a wall at every direction from the current position and if there isn't then I add that new position and the path with the new direction
I define currentPlace and currentPath in a where statement so that I don't have to keep calling (fst x) and (snd x)
If the list of viable positions is empty then that means there is no place you can move so no path is found

> solveMazeIter :: Maze -> Place -> [(Place, Path)] -> Path
> solveMazeIter maze target [] = error "No path found :("
> solveMazeIter maze target (x:xs) | currentPlace == target     = currentPath
>                                  | otherwise                  = solveMazeIter maze target (xs ++ [(move direction currentPlace, currentPath ++ [direction]) | direction <- [N,E,S,W], not (hasWall maze currentPlace direction)])
>                                       where currentPlace = fst x
>                                             currentPath  = snd x

Testing for question 3 - all of them yield the expected output and it shows how the impossibleMaze cannot be solved (had to interrupt the running - was in an infinite loop):
ghci> solveMaze smallMaze (0,0) (3,2)
[E,N,E,S,E,N,N]

ghci> solveMaze impossibleMaze (0,0) (2,2)
^CInterrupted.

ghci> solveMaze smallMaze (0,0) (1,2)
[E,N,W,N,E]

QUESTION 4:

> fastSolveMaze :: Maze -> Place -> Place -> Path
> fastSolveMaze maze start target = fastSolveMazeIter maze target [] [(start, [])]

My fastSolveMazeIter function works in the following way:
- Get a list of the possible places you can go from the current location in the same way as in question 3
- Then filter the possible places by checking if they are in the visited list 
- now given a list of nonVisitedPlaces where you can go add them to the end of the list of places to be processed and to the visited list so that they won't be checked again


> fastSolveMazeIter :: Maze -> Place -> [(Place)] -> [(Place, Path)] -> Path
> fastSolveMazeIter maze target visited [] = error "No path found :("
> fastSolveMazeIter maze target visited (x:xs) | currentPlace == target     = currentPath
>                                              | otherwise                  = fastSolveMazeIter maze target (visited ++ [fst nonVisitedPlace | nonVisitedPlace <- nonVisitedPlaces]) (xs ++ nonVisitedPlaces)
>                                                  where  currentPlace      = fst x
>                                                         currentPath       = snd x
>                                                         newPlaces         = [(move direction currentPlace, currentPath ++ [direction]) | direction <- [N,E,S,W], not (hasWall maze currentPlace direction)]
>                                                         nonVisitedPlaces  = [newPlace | newPlace <- newPlaces, notVisited (fst newPlace) visited]

Linear search to check if the new place is in the visited list 

> notVisited :: Place -> [(Place)] -> Bool
> notVisited cPlace  [] = True
> notVisited cPlace (x:xs) | (cPlace == x)      = False
>                          | otherwise          = notVisited cPlace xs

> emptyMaze :: Maze
> emptyMaze = makeMaze (10,10) []

Testing for question 4:
ghci> fastSolveMaze smallMaze (0,0) (3,2)
[E,N,E,S,E,N,N]

ghci> fastSolveMaze impossibleMaze (0,0) (2,2)
*** Exception: No path found :(
CallStack (from HasCallStack):
  error, called at Main.lhs:80:46 in main:Main

ghci> fastSolveMaze impossibleMaze (0,0) (2,1)
[E,E,N]

ghci> fastSolveMaze largeMaze (0,0) (22,21)
[N,N,N,N,N,N,N,N,N,E,E,E,N,W,W,W,N,E,E,E,N,W,W,W,N,E,E,E,E,E,N,N,N,W,S,S,W,W,W,W,N,N,N,E,S,S,E,E,N,W,N,N,W,W,N,E,E,E,E,E,E,N,W,W,W,W,W,W,N,E,E,E,E,E,E,E,S,S,S,S,E,E,N,N,N,N,E,E,E,E,S,W,W,W,S,S,S,E,N,N,E,E,E,S,W,W,S,S,W,W,W,W,W,S,E,E,E,S,W,W,W,S,S,S,E,S,S,S,E,N,N,N,E,S,S,S,S,W,W,W,S,E,E,E,S,W,W,W,S,E,E,E,E,S,S,E,E,E,E,E,E,E,S,E,E,E,N,W,W,N,N,N,E,S,S,E,E,N,W,N,E,N,N,W,S,W,W,W,W,S,W,N,N,N,W,W,W,N,N,N,E,S,S,E,N,N,N,W,W,N,N,N,N,N,E,S,S,S,S,E,E,E,E,E,E,E,S,W,W,W,W,W,S,E,E,E,E,E,E,N,N,N,W,W,W,W,N,E,E,N,W,W,N,E,E,N,W,W,W,N,N,N,E,S,S,E,N,N,E,E,E]

ghci> drawMaze emptyMaze
+--+--+--+--+--+--+--+--+--+--+
|                             |
+  +  +  +  +  +  +  +  +  +  +
|                             |
+  +  +  +  +  +  +  +  +  +  +
|                             |
+  +  +  +  +  +  +  +  +  +  +
|                             |
+  +  +  +  +  +  +  +  +  +  +
|                             |
+  +  +  +  +  +  +  +  +  +  +
|                             |
+  +  +  +  +  +  +  +  +  +  +
|                             |
+  +  +  +  +  +  +  +  +  +  +
|                             |
+  +  +  +  +  +  +  +  +  +  +
|                             |
+  +  +  +  +  +  +  +  +  +  +
|                             |
+--+--+--+--+--+--+--+--+--+--+

ghci> fastSolveMaze emptyMaze (0,0) (9,9)
[N,N,N,N,N,N,N,N,N,E,E,E,E,E,E,E,E,E]

======================================================================

Some test mazes.  In both cases, the task is to find a path from the bottom
left corner to the top right.

First a small one

> smallMaze :: Maze
> smallMaze = 
>   let walls = [((0,0), N), ((2,2), E), ((2,1),E), ((1,0),E), 
>                ((1,2), E), ((1,1), N)]
>   in makeMaze (4,3) walls

Now a large one.  Define a function to produce a run of walls:

> run (x,y) n E = [((x,y+i),E) | i <- [0..n-1]]
> run (x,y) n N = [((x+i,y),N) | i <- [0..n-1]]

And here is the maze.

> largeMaze :: Maze 
> largeMaze =
>   let walls = 
>         run (0,0) 3 E ++ run (1,1) 3 E ++ [((1,3),N)] ++ run (0,4) 5 E ++
>         run (2,0) 5 E ++ [((2,4),N)] ++ run (1,5) 3 E ++
>         run (1,8) 3 N ++ run (2,6) 3 E ++
>         run (3,1) 7 E ++ run (4,0) 4 N ++ run (4,1) 5 E ++ run (5,2) 3 N ++
>         run (4,6) 2 N ++ run (5,4) 3 E ++ run (6,3) 5 N ++ run (8,0) 4 E ++
>         run (6,1) 3 N ++ run (0,9) 3 N ++ run (1,10) 3 N ++ run (0,11) 3 N ++
>         run (1,12) 6 N ++ run (3,9) 4 E ++ run (4,11) 2 N ++
>         run (5,9) 3 E ++ run (4,8) 3 E ++ run (5,7) 5 N ++ run (6,4) 9 E ++
>         run (7,5) 3 N ++ run (8,4) 4 N ++ run (8,6) 3 N ++ run (10,5) 7 E ++
>         run (9,8) 3 E ++ run (8,9) 3 E ++ run (7,8) 3 E ++ run (8,11) 3 N ++
>         run (0,13) 5 N ++ run (4,14) 2 E ++ run (0,15) 2 E ++ 
>         run (1,14) 3 N ++ run (3,15) 2 E ++ run (0,17) 2 N ++ 
>         run (1,16) 2 E ++ run (2,15) 1 N ++ run (3,16) 3 N ++
>         run (2,17) 2 E ++ run (1,18) 6 N ++ run (4,17) 3 N ++ 
>         run (6,14) 7 E ++ run (5,13) 4 E ++ run (7,12) 2 E ++
>         run (8,13) 3 N ++ run (7,14) 3 N ++ run (10,14) 2 E ++
>         run (8,15) 5 N ++ run (7,16) 5 N ++ run (9,1) 2 E ++
>         run (10,0) 12 N ++ run (21,1) 1 E ++ run (10,2) 2 E ++
>         run (11,1) 7 N ++ run (17,1) 1 E ++ run (11,3) 3 E ++
>         run (12,2) 7 N ++ run (18,2) 2 E ++ run (19,1) 2 N ++
>         run (15,3) 3 N ++ run (14,4) 3 E ++ run (13,3) 3 E ++
>         run (12,4) 3 E ++ run (12,6) 3 N ++ run (11,7) 8 E ++ 
>         run (9,12) 3 N ++ run (12,14) 1 N ++ run (12,8) 10 E ++
>         run (0,19) 6 N ++ run (1,20) 6 N ++ run (7,18) 8 E ++
>         run (8,17) 1 N ++ run (8,18) 3 E ++ run (9,17) 4 E ++ 
>         run (10,18) 2 E ++ run (11,17) 2 E ++ run (10,20) 3 N ++
>         run (11,19) 3 N ++ run (12,18) 2 N ++ run (13,17) 2 N ++
>         run (13,13) 4 E ++ run (14,12) 7 N ++ run (13,11) 2 N ++
>         run (14,10) 2 E ++ run (13,9)2 E ++ run (14,8) 3 N ++ 
>         run (13,7) 3 N ++ run (15,5) 3 E ++ run (16,6) 3 E ++
>         run (18,5) 4 N ++ run (16,4) 2 N ++ run (13,20) 2 E ++
>         run (14,18) 4 E ++ run (20,2) 3 N ++ run (19,3) 2 E ++
>         run (18,4) 2 E ++ run (23,4) 1 E ++ run (22,4) 1 N ++
>         run (21,3) 1 N ++ run (20,4) 2 E ++ run (17,6) 4 N ++ 
>         run (20,7) 2 E ++ run (21,7) 2 N ++ run (21,6) 1 E ++ 
>         run (15,9) 1 E ++ run (17,8) 2 E ++ run (18,7) 2 E ++ 
>         run (19,8) 2 E ++ run (21,9) 1 E ++ run (16,9) 6 N ++
>         run (16,10) 7 N ++ run (15,11) 2 E ++ run (17,11) 5 N ++ 
>         run (14,14) 3 E ++ run (15,15) 6 E ++ run (17,14) 4 E ++
>         run (16,18) 4 E ++ run (15,17) 1 N ++ run (17,17) 3 N ++
>         run (15,13) 7 N ++ run (21,12) 2 E ++ run (16,16) 1 N ++
>         run (16,14) 1 N ++ run (17,15) 3 N ++ run (19,14) 4 N ++
>         run (20,15) 5 E ++ run (19,16) 2 N ++ run (21,16) 5 E ++
>         run (17,19) 2 E ++ run (18,20) 2 E ++ run (19,19) 2 E ++
>         run (18,18) 2 N ++ run (20,20) 3 N
>   in makeMaze (23,22) walls

And now an impossible maze

> impossibleMaze :: Maze
> impossibleMaze =
>   let walls = [((0,1), E), ((1,0),N), ((1,2), E), ((2,1), N)]
>   in makeMaze (3,3) walls


QUESTION 6:

For my first maze I decided to do the python logo in the middle of the maze and then I opened it up so that it wasn't an impossible maze

> stefanMaze1 :: Maze
> stefanMaze1 = 
>   let walls = 
>           run (0,2) 2 E ++ [((2,1), E)] ++ [((5,4), E)] ++ run (7,2) 2 E ++ -- East walls
>           run (3,0) 3 N ++ run (3,2) 3 N ++ run (3,4) 3 N ++                -- central North walls
>           run (1,1) 2 N ++ run (5,1) 3 N ++                                 -- lower North walls
>           run (1,3) 3 N ++ run (6,3) 2 N                                    -- upper North walls
>   in makeMaze (9,6) walls

ghci> drawMaze stefanMaze1
+--+--+--+--+--+--+--+--+--+
|                          |
+  +  +  +--+--+--+  +  +  +
|                 |        |
+  +--+--+--+  +  +--+--+  +
|  |                    |  |
+  +  +  +--+--+--+  +  +  +
|  |                    |  |
+  +--+--+  +  +--+--+--+  +
|        |                 |
+  +  +  +--+--+--+  +  +  +
|                          |
+--+--+--+--+--+--+--+--+--+


For my second maze I decided to simply do a staircase

> stefanMaze2 :: Maze
> stefanMaze2 = 
>   let walls = [ ((0,0), E), ((1,0), N), ((1,1), E), ((2,1), N), ((2,2), E), ((3,2), N) ]
>   in makeMaze (5,4) walls

TESTING:
ghci> drawMaze stefanMaze2
+--+--+--+--+--+
|              |
+  +  +  +--+  +
|        |     |
+  +  +--+  +  +
|     |        |
+  +--+  +  +  +
|  |           |
+--+--+--+--+--+

ghci> fastSolveMaze stefanMaze2 (0,0) (1,0)
[N,N,N,E,E,E,E,S,S,S,W,W,W]

I then noticed I could automate this process to generate mazes of size (n, n-1) with n being inputted by the user 
I just alternate between adding one to the x coordinate and then one to the y coordinate as well as alternating between E and N
As I didn't know how to do the alternating in a list comprehension I just took them in pairs and then concat the final sub lists of pairs 
I then observed the relation between pairs of being (x,x) and (x+1, x)

> makeStefanMaze2 :: Int -> Maze
> makeStefanMaze2 n = makeMaze (n, n-1) walls
>   where walls = concat [[((x,x), E), ((x+1,x), N)] | x <- [0..(n-3)]]

TESTING:
ghci> drawMaze (makeStefanMaze2 5)
+--+--+--+--+--+
|              |
+  +  +  +--+  +
|        |     |
+  +  +--+  +  +
|     |        |
+  +--+  +  +  +
|  |           |
+--+--+--+--+--+

ghci> drawMaze (makeStefanMaze2 10)
+--+--+--+--+--+--+--+--+--+--+
|                             |
+  +  +  +  +  +  +  +  +--+  +
|                       |     |
+  +  +  +  +  +  +  +--+  +  +
|                    |        |
+  +  +  +  +  +  +--+  +  +  +
|                 |           |
+  +  +  +  +  +--+  +  +  +  +
|              |              |
+  +  +  +  +--+  +  +  +  +  +
|           |                 |
+  +  +  +--+  +  +  +  +  +  +
|        |                    |
+  +  +--+  +  +  +  +  +  +  +
|     |                       |
+  +--+  +  +  +  +  +  +  +  +
|  |                          |
+--+--+--+--+--+--+--+--+--+--+

Hence, working as intended!
