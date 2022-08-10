-- Exercise 1
safeHead :: [a] -> Maybe a
safeTail :: [a] -> Maybe [a]
safeLast :: [a] -> Maybe a
safeInit :: [a] -> Maybe [a]

safeHead (x:_)  = Just x
safeHead _      = Nothing

safeTail (_:xs) = Just xs
safeTail _      = Nothing

safeLast [x]    = Just x
safeLast (_:xs) = safeLast xs
safeLast _      = Nothing

safeInit []     = Nothing
safeInit xs     = Just (init xs)

-- Exercise 2
splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = []
splitWith p xs = let (y, ys) = break p xs
                 in y : splitWith p (dropWhile p ys)