module LovelyLuckyLAMBs where

import Control.Monad
import Data.List

import Lib

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

fibs' :: [Integer]
fibs' = tail fibs

powOf2Gen :: [Integer]
powOf2Gen = map (2^) [0..]

takeUnder :: (Ord a, Num a) => a -> [a] -> [a]
takeUnder n xs = last $ takeWhile ((<= n) . sum) $ inits xs

takeUnder' = flip takeUnder

lengthDiff :: [a] -> [a] -> Int
lengthDiff xs ys = length xs - length ys

generous = fibs'

stingy = powOf2Gen

solution :: IO ()
solution = do
    let domain = [1..333]

    {-
        enumerate :: [b] -> [a] -> [(a, b)]
        enumerate ys xs = zip xs ys

        enum1 :: (Enum b, Num b) => [a] -> [(a, b)]
        enum1 = enumerate [1..]
    -}
    let result = enum1 $ zipWith lengthDiff (map (takeUnder' generous) domain) (map (takeUnder' stingy) domain)

    forM_ result (\(x, i) -> 
        putStrLn ("For " ++ show i ++ " LAMBs, we can hire (" ++ show x ++ ") more henchmen by being stingy rather than generous."))

    putStrLn "Goodbye!!"
