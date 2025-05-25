Module to define maze using BSTs 

> module BinaryMaze (
>   Maze,
>   makeMaze,
>   hasWall,
>   sizeOf
> )
> where

> import Geography

I need to define the functions for the binary search trees that I will use. I think I only need a search and insert function.

> data Tree a = Fork (Tree a) a (Tree a) | Empty deriving Show

My insert function for the unbalanced BST

> insert :: Place -> Tree Place -> Tree Place
> insert newPlace Empty = Fork Empty newPlace Empty
> insert newPlace (Fork left root right) | newPlace < root  = Fork (insert newPlace left) root right
>                                        | newPlace > root  = Fork left root (insert newPlace right)

My insert function for the balanced BST
Put the middle element of the sorted list of places as the root and pass the left and right half to left and right half of tree
Looking at the structure I could make this a fold but this is more readable 

> insertBalanced :: [Place] -> Tree Place -> Tree Place
> insertBalanced [] Empty = Empty
> insertBalanced xs Empty = Fork (insertBalanced leftHalf Empty) middleElement (insertBalanced rightHalf Empty)
>   where (leftHalf, middleElement, rightHalf) = halve xs

Helper function to split a list into 2 halves and the middle element

> halve :: [a] -> ([a], a, [a])
> halve xs = (fst split, head (snd split), tail (snd split))
>   where split = splitAt (length xs `div` 2) xs

TESTING:
ghci> halve [1..9]
([1,2,3,4],5,[6,7,8,9])

ghci> halve [1]
([],1,[])

ghci> halve [1..4]
([1,2],3,[4])

Binary search function that searches for a place in a BST of places 

> binarySearch :: Place -> Tree Place -> Bool
> binarySearch p Empty = False
> binarySearch p (Fork left root right) | p == root     = True
>                                       | p < root      = binarySearch p left
>                                       | p > root      = binarySearch p right

So I will now have 4 trees containing the walls in each direction as compared to a list holding them

> data Maze = BinaryMaze Size (Tree Place) (Tree Place) (Tree Place) (Tree Place) deriving Show


Defining insertion sort so that I can sort the list of places initially

> ssort :: Ord a => [a] -> [a]
> ssort [] = []
> ssort xs = y : ssort ys
>            where y = minimum xs
>                  ys = delete y xs

> delete :: Eq a => a -> [a] -> [a]
> delete y xs = takeWhile (/= y) xs ++ tail (dropWhile (/= y) xs)

For the makeMaze function it will be the same as in myMaze but I will then insert each element from nWalls, sWalls, eWalls, wWalls into binary trees using my insert function
To do the inserting into binary trees I will use a foldr so that I don't need to define a helper function (foldr insert Empty) - this was for the unbalanced BST implementation
For the balanced BST implementation I will simply call my helper function insertBalanced 

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
>       nTree     = insertBalanced (ssort nWalls) Empty
>       sTree     = insertBalanced (ssort sWalls) Empty
>       eTree     = insertBalanced (ssort eWalls) Empty
>       wTree     = insertBalanced (ssort wWalls) Empty
>   in BinaryMaze (x,y) nTree sTree eTree wTree

The hasWall function will now be changed to search the binary tree for the corresponding wall direction
I will be using my binarySearch function defined earlier
Improvement will be from using a linear search (O(n)) to binary search (O(logn))

> hasWall :: Maze -> Place -> Direction -> Bool
> hasWall (BinaryMaze _ nTree sTree eTree wTree) pos d | d == N   = binarySearch pos nTree
>                                                      | d == S   = binarySearch pos sTree
>                                                      | d == E   = binarySearch pos eTree
>                                                      | d == W   = binarySearch pos wTree

Reflect, and sizeOf functions stays the same

> reflect :: Direction -> Place -> Place
> reflect d (i,j) = (move d (i,j))

> sizeOf :: Maze -> Size
> sizeOf (BinaryMaze size _ _ _ _) = size

TESTING:
Using an unbalanced BST (first part of exercise 7):
ghci> fastSolveMaze largeMaze (0,0) (22,21)
[N,N,N,N,N,N,N,N,N,E,E,E,N,W,W,W,N,E,E,E,N,W,W,W,N,E,E,E,E,E,N,N,N,W,S,S,W,W,W,W,N,N,N,E,S,S,E,E,N,W,N,N,W,W,N,E,E,E,E,E,E,N,W,W,W,W,W,W,N,E,E,E,E,E,E,E,S,S,S,S,E,E,N,N,N,N,E,E,E,E,S,W,W,W,S,S,S,E,N,N,E,E,E,S,W,W,S,S,W,W,W,W,W,S,E,E,E,S,W,W,W,S,S,S,E,S,S,S,E,N,N,N,E,S,S,S,S,W,W,W,S,E,E,E,S,W,W,W,S,E,E,E,E,S,S,E,E,E,E,E,E,E,S,E,E,E,N,W,W,N,N,N,E,S,S,E,E,N,W,N,E,N,N,W,S,W,W,W,W,S,W,N,N,N,W,W,W,N,N,N,E,S,S,E,N,N,N,W,W,N,N,N,N,N,E,S,S,S,S,E,E,E,E,E,E,E,S,W,W,W,W,W,S,E,E,E,E,E,E,N,N,N,W,W,W,W,N,E,E,N,W,W,N,E,E,N,W,W,W,N,N,N,E,S,S,E,N,N,E,E,E]
(0.11 secs, 46,820,392 bytes)

ghci> fastSolveMaze largeMaze (0,0) (12,5)
[N,N,N,N,N,N,N,N,N,E,E,E,N,W,W,W,N,E,E,E,N,W,W,W,N,E,E,E,E,E,N,N,N,W,S,S,W,W,W,W,N,N,N,E,S,S,E,E,N,W,N,N,W,W,N,E,E,E,E,E,E,N,W,W,W,W,W,W,N,E,E,E,E,E,E,E,S,S,S,S,E,E,N,N,N,N,E,E,E,E,S,W,W,W,S,S,S,E,N,N,E,E,E,S,W,W,S,S,W,W,W,W,W,S,E,E,E,S,W,W,W,S,S,S,E,S,S,S,E,N,N,N,E,S,S,S,S,W,W,W,S,E,E,E,S,W,W,W,S,E,E,E,E,S,S,E,E,E,E,E,E,E,S,E,E,E,N,W,W,N,N,N,E,S,S,E,E,N,W,N,E,N,N,W,S,W,W,W,W,S,E,S,S,W,W,W,W,N,N,N,W,S,S,S,W,N,N]
(0.09 secs, 27,799,784 bytes)

This is roughly the same performance as using MyMaze but also uses more memory

Using a balanced BST (second part of exercise 7):
ghci> fastSolveMaze largeMaze (0,0) (22,21)
[N,N,N,N,N,N,N,N,N,E,E,E,N,W,W,W,N,E,E,E,N,W,W,W,N,E,E,E,E,E,N,N,N,W,S,S,W,W,W,W,N,N,N,E,S,S,E,E,N,W,N,N,W,W,N,E,E,E,E,E,E,N,W,W,W,W,W,W,N,E,E,E,E,E,E,E,S,S,S,S,E,E,N,N,N,N,E,E,E,E,S,W,W,W,S,S,S,E,N,N,E,E,E,S,W,W,S,S,W,W,W,W,W,S,E,E,E,S,W,W,W,S,S,S,E,S,S,S,E,N,N,N,E,S,S,S,S,W,W,W,S,E,E,E,S,W,W,W,S,E,E,E,E,S,S,E,E,E,E,E,E,E,S,E,E,E,N,W,W,N,N,N,E,S,S,E,E,N,W,N,E,N,N,W,S,W,W,W,W,S,W,N,N,N,W,W,W,N,N,N,E,S,S,E,N,N,N,W,W,N,N,N,N,N,E,S,S,S,S,E,E,E,E,E,E,E,S,W,W,W,W,W,S,E,E,E,E,E,E,N,N,N,W,W,W,W,N,E,E,N,W,W,N,E,E,N,W,W,W,N,N,N,E,S,S,E,N,N,E,E,E]
(0.08 secs, 38,507,608 bytes)

ghci> fastSolveMaze largeMaze (0,0) (12,5)
[N,N,N,N,N,N,N,N,N,E,E,E,N,W,W,W,N,E,E,E,N,W,W,W,N,E,E,E,E,E,N,N,N,W,S,S,W,W,W,W,N,N,N,E,S,S,E,E,N,W,N,N,W,W,N,E,E,E,E,E,E,N,W,W,W,W,W,W,N,E,E,E,E,E,E,E,S,S,S,S,E,E,N,N,N,N,E,E,E,E,S,W,W,W,S,S,S,E,N,N,E,E,E,S,W,W,S,S,W,W,W,W,W,S,E,E,E,S,W,W,W,S,S,S,E,S,S,S,E,N,N,N,E,S,S,S,S,W,W,W,S,E,E,E,S,W,W,W,S,E,E,E,E,S,S,E,E,E,E,E,E,E,S,E,E,E,N,W,W,N,N,N,E,S,S,E,E,N,W,N,E,N,N,W,S,W,W,W,W,S,E,S,S,W,W,W,W,N,N,N,W,S,S,S,W,N,N]
(0.07 secs, 20,746,608 bytes)

In both cases the result is faster and more space efficient than using an unbalanced BST
Now the time taken is less than with the MyMaze implementation as well



Showing the balanced maze is balanced:

(Fork 
(Fork (Fork (Fork Empty (0,-1) Empty) (0,0) Empty) 
(0,2) 
(Fork (Fork Empty (1,-1) Empty) (1,1) Empty))
 (1,2) 
(Fork (Fork (Fork Empty (2,-1) Empty) (2,2) Empty) 
(3,-1) 
(Fork Empty (3,2) Empty)))

(Fork (Fork (Fork (Fork Empty (0,0) Empty) (0,1) Empty) (0,3) (Fork (Fork Empty (1,0) Empty) (1,2) Empty)) (1,3) (Fork (Fork (Fork Empty (2,0) Empty) (2,3) Empty) (3,0) (Fork Empty (3,3) Empty)))

(Fork (Fork (Fork (Fork Empty (-1,0) Empty) (-1,1) Empty) (-1,2) (Fork (Fork Empty (1,0) Empty) (1,2) Empty)) (2,1) (Fork (Fork (Fork Empty (2,2) Empty) (3,0) Empty) (3,1) (Fork Empty (3,2) Empty)))

(Fork (Fork (Fork (Fork Empty (0,0) Empty) (0,1) Empty) (0,2) (Fork (Fork Empty (2,0) Empty) (2,2) Empty)) (3,1) (Fork (Fork (Fork Empty (3,2) Empty) (4,0) Empty) (4,1) (Fork Empty (4,2) Empty)))
