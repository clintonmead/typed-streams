module Main (main) where

import qualified Data.Generic.Enum as E
import Data.Stream.Typed (RunTimeStream)
import Data.Semigroup (Semigroup((<>)))

import Data.Bits ((.&.))
import Data.Foldable (foldl')

import Criterion

n :: Int
n = 10000000

f :: Int -> Int
f x = x*(x .&. 3)

g :: Int -> Int
g x = x*(x .&. 7)

xs :: Int -> RunTimeStream Int
xs n = E.enumFromTo 1 n

xl :: Int -> [Int]
xl n = [1..n]

sumG :: (Functor t, Foldable t, Semigroup (t Int)) => t Int -> Int
sumG x = foldl' (+) 0 ((fmap f x) <> (fmap g x))

sumS n = sumG (xs n)
sumL n = sumG (xl n)

fast :: Int -> Int
fast n = go g (go f 0 1 n) 1 n where
  go :: (Int -> Int) -> Int -> Int -> Int -> Int
  go f = go' where
    go' :: Int -> Int -> Int -> Int
    go' acc s i = if i == 0 then acc else let next_acc = acc + f s in next_acc `seq` go' next_acc (s + 1) (i - 1)

main :: IO ()
main = do
  putStrLn ("n = " ++ show n)
  putStrLn $ "Hand coded strict loop (Result = " ++ show (fast n) ++ " ):"
  benchmark (nf fast n)
  putStrLn $ "Using lists (Result = " ++ show (sumL n) ++ " ):"
  benchmark (nf sumL n)
  putStrLn $ "Using streams (Result = " ++ show (sumS n) ++ " ):"
  benchmark (nf sumS n)

