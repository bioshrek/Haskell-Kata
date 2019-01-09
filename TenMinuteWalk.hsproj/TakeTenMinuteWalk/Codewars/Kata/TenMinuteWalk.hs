module Codewars.Kata.TenMinuteWalk where
  
import Data.Maybe (catMaybes)
  
data Move = West | East | North | South
  deriving (Eq, Ord)
  
type Position = (Int, Int)

char2Move :: Char -> Maybe Move
char2Move 'w' = Just West
char2Move 'e' = Just East
char2Move 'n' = Just North
char2Move 's' = Just South
char2Move _ = Nothing

move :: Move -> Position -> Position
move West (x, y) = (x - 1, y)
move East (x, y) = (x + 1, y)
move North (x, y) = (x, y + 1)
move South (x, y) = (x, y - 1)

isValidWalk :: [Char] -> Bool
isValidWalk xs@[x1, x2, x3, x4, x5, x6, x7, x8, x9, x10] = (10 == length moves) && (src == dest)
  where
    moves = catMaybes $ char2Move <$> xs
    src = (0, 0)
    dest = foldr move src moves
isValidWalk _ = False