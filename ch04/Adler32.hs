import           Data.Bits                      ( (.&.)
                                                , (.|.)
                                                , shiftL
                                                )
import           Data.Char                      ( ord )

base :: Int
base = 65521

adler32_1 :: [Char] -> Int
adler32_1 xs = helper 1 0 xs
 where
  helper a b (x : xs) =
    let a' = (a + (ord x .&. 0xff)) `mod` base
        b' = (a' + b) `mod` base
    in  helper a' b' xs
  helper a b _ = (b `shiftL` 16) .|. a

adler32_2 :: [Char] -> Int
adler32_2 xs = helper (1, 0) xs
 where
  helper (a, b) (x : xs) =
    let a' = (a + (ord x .&. 0xff)) `mod` base
        b' = (a' + b) `mod` base
    in  helper (a', b') xs
  helper (a, b) _ = (b `shiftL` 16) .|. a
