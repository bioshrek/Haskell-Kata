module Scramblies where
  
import Data.Map.Lazy as M
import Data.Maybe (fromMaybe)
import Control.Applicative (liftA2)

scramble :: [Char] -> [Char] -> Bool
scramble s1 s2 = M.foldrWithKey f True map2
  where
    f k a acc = fromMaybe False $ liftA2 (&&) (pure acc) $ liftA2 (>=) (M.lookup k map1) (pure a)
    map1 = string2map s1
    map2 = string2map s2
    
string2map :: [Char] -> Map Char Int
string2map = Prelude.foldr (alter count) empty
  where
    count Nothing = Just 1
    count (Just a) = Just (a + 1)
