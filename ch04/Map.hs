import Data.Char (toUpper)

myMap :: (a -> b) -> [a] -> [b]
myMap f (x:xs) = f x : myMap f xs
myMap _ _      = []

square :: [Double] -> [Double]
square xs = map squareOne xs
    where squareOne x = x * x

upperCase :: String -> String
upperCase xs = map toUpper xs
