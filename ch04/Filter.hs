myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p (x:xs)
  | p x       = x : myFilter p xs
  | otherwise = myFilter p xs
myFilter _ _  = []

oddList :: [Int] -> [Int]
oddList = myFilter odd
