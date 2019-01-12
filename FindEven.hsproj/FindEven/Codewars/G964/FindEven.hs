module Codewars.G964.FindEven where

findEvenIndex :: [Int] -> Int
findEvenIndex [] = -1
findEvenIndex (x:xs) = if (l == r) then i else -1
  where
    (i, (l, _, r)) = findEven xs (0, (0, x, sum xs))

findEven :: [Int] -> (Int, (Int, Int, Int)) -> (Int, (Int, Int, Int))
findEven [] x = x
findEven (x:xs) e@(i, (l, m, r)) | l == r = e
                                 | otherwise = findEven xs (i+1, (l+m, x, r-x))