module Tic
  (Mark, Board, newGame) where

import Data.List (transpose)
data Mark = Tic | Tac deriving(Eq, Show, Enum, Bounded)
data State = Playing | Draw | Victory Mark deriving(Eq, Show)

type Row = Int
type Col = Int
type Location = (Col,Row)

data Board = Board
  { tics :: [Int]
  , tacs :: [Int]
  , turn :: Mark
  } deriving(Show)

newtype MetaData = MetaData
  { size :: Int
  } deriving(Show)

data Game = Game
  { metaData :: MetaData
  , board :: Board
  , state :: State
  } deriving(Show)

newBoard :: Mark -> Board
newBoard = Board [] []

newGame :: Int -> Mark -> Game
newGame n m
  | even n = error "Cannot create a game of even size!"
  | otherwise = Game (MetaData n) (newBoard m) Playing

update :: Mark -> Location -> Game -> Game
update m loc (Game meta board Playing) =
  let newBoard = placeMeta meta loc board m
      newState = checkStateMeta meta newBoard
  in Game meta newBoard newState
update _ _ g = g
      
next :: Mark -> Mark
next Tac = Tic
next t = succ t

placeMeta :: MetaData -> Location -> Board -> Mark -> Board
placeMeta meta loc = place' $ indexMeta meta loc

indexMeta :: MetaData -> Location -> Int
indexMeta meta = index' $ size meta

indexInvMeta :: MetaData -> Int -> Location
indexInvMeta meta = indexInv' $ size meta

hasDrawMeta :: MetaData -> Board -> Bool
hasDrawMeta meta = hasDraw' $ size meta

checkStateMeta :: MetaData -> Board -> State
checkStateMeta meta = checkState' (hasDrawMeta meta) $ victoryConditions' n
  where n = size meta

place' :: Int -> Board -> Mark -> Board -- Could use either to represent no action -> propogate and dont check for victory, efficiency B O I S
place' i (Board tics tacs Tic) Tic
  | notElem i tics && notElem i tacs = Board (i:tics) tacs (next Tic)
  | otherwise = Board tics tacs Tic
place' i (Board tics tacs Tac) Tac
  | notElem i tics && notElem i tacs = Board tics (i:tacs) (next Tac)
  | otherwise = Board tics tacs Tac
place' _ b _ = b

index' :: Int -> Location -> Int
index' size (col,row)
  | n >= size * size = error "Index out of bounds" -- Dont error
  | otherwise = n
  where n = row * size + col

indexInv' :: Int -> Int -> Location
indexInv' size n
  | n >= size * size = error "Index out of bounds"
  |otherwise = (col, row)
  where (row, col) = n `divMod` size

hasDraw' :: Int -> Board -> Bool
hasDraw' n board =
  let len xs ys = length xs + length ys
  in n * n <= len (tics board) (tacs board)

victoryConditions' :: Int -> [[Int]]
victoryConditions' n = rightDiag : leftDiag : values ++ transpose values
  where values = [[x*n..x*n+n-1] | x <- [0..n-1]]
        rightDiag = [x+(n*x) | x <- [0..n-1]]
        leftDiag = [(n-1) * x | x <- [1..n]]

checkState' :: (Board -> Bool) -> [[Int]] -> Board -> State
checkState' checkDraw b (Board tics tacs turn)
  | checkDraw (Board tics tacs turn) = Draw
  | noice b tics = Victory Tic
  | noice b tacs = Victory Tac
  | otherwise = Playing
  where noice xxs xs = any (all (`elem` xs)) xxs
