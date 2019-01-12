module HumanTime where
  
import Data.List (unfoldr)

humanReadable :: Int -> String
humanReadable n = foldl compose "" $ padding $ unfoldr decompose (0, n)
  where
    decompose (i, x) = if 0 == x then Nothing else Just (x `mod` base, (i + 1, x `div` base))
      where base = modBase i
    padding = until (\xs -> length xs >= 3) (\xs -> xs ++ [0])
    compose acc x = if null acc then fmtInt x else fmtInt x ++ ":" ++ acc
    
modBase :: Int -> Int
modBase 0 = 60
modBase 1 = 60
modBase _ = 100
    
fmtInt :: Int -> String
fmtInt = paddingZero . show
  where
    paddingZero = until (\xs -> length xs >= 2) (\xs -> '0':xs)
    

