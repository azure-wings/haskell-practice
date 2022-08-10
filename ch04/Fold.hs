import           Data.Bits                      ( (.&.)
                                                , (.|.)
                                                , shiftL
                                                )
import           Data.Char                      ( ord )

foldL :: (a -> b -> a) -> a -> [b] -> a
foldL step zero (x : xs) = foldL step (step zero x) xs
foldL _    zero []       = zero

foldLSum :: [Integer] -> Integer
foldLSum xs = foldL (+) 0 xs

base :: Int
base = 65521

foldLAdler :: [Char] -> Int
foldLAdler xs = let (a, b) = foldL step (1, 0) xs in (b `shiftL` 16) .|. a
 where
  step (a, b) x =
    let a' = a + (ord x .&. 0xff) in (a' `mod` base, (a' + b) `mod` base)
