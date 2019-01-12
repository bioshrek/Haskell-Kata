module YourOrderPlease where
  
import Data.List (sortBy)
import Data.Char (isDigit)
  
yourOrderPlease :: String -> String
yourOrderPlease = unwords . sortBy ordering . words
  where
    ordering x y = compare (numberIn x) (numberIn y)
    numberIn = filter isDigit