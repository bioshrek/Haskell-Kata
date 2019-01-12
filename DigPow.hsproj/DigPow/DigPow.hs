module DigPow where
  
import Data.List (unfoldr)
  
digpow :: Integer -> Integer -> Integer
digpow n p = if 0 == b then a else -1
  where
    (a, b) = magicSum `divMod` n
    magicSum = fst $ foldr increaseSum (0, p) reversedDigits
    increaseSum x (s, p) = (s + x^p, p+1)
    reversedDigits = unfoldr (\x -> if 0 == x then Nothing else Just (x `mod` 10, x `div` 10)) n