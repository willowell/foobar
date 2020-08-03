module SolarDoomsday where

-- I'm using Natural to ensure that these functions
-- are valid for non-negative values.
-- After all, it is not possible to have a negative distance!
import Numeric.Natural

-- Unfortunately, that also means I have to make this ugly
-- converter to make sqrt happy!
toDouble :: Natural -> Double
toDouble n = fromIntegral (toInteger n) :: Double

-- Determine if a natural number is a perfect square
isSquare :: Natural -> Bool
isSquare n = sq * sq == n
    where sq = floor $ sqrt (toDouble n)

-- Find the largest perfect square in a given natural number
largestSquare :: Natural -> Natural
largestSquare n
    | isSquare n = n
    | otherwise = maximum $ takeWhile (< n) $ map (^2) [1..n]

-- Find the list of perfect squares that will fit in a given natural number,
-- starting with the largest perfect square and working backwards.
largestSquares :: Natural -> [Natural]
largestSquares 0 = [0]
largestSquares 1 = [1]
largestSquares n = go' n
    where
        go' n
            | n == 0 = [] -- if we have zero left, we're done.
            | n == 1 = [1] -- short circuit for 1
            | otherwise = 
                let m = largestSquare n -- find the largest perfect square
                in m : go' (n - m) -- and make a list out of it and whatever's left.


solution :: IO ()
solution = mapM_ (print . largestSquares) [1..100]