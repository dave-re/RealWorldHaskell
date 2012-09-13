import Control.Monad

zeroMod :: Int -> Int -> Maybe Int
x `zeroMod` n = do
    guard ((x `mod` n) == 0)
    return x