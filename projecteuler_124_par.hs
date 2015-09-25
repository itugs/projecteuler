module Main where

import qualified Data.MemoCombinators as Memo
import Data.Numbers.Primes
import Data.Ratio
import Data.List
import Control.Parallel.Strategies

eulerTotientLength :: Int -> Int
eulerTotientLength = Memo.integral eulerTotientLength'
 where eulerTotientLength' n
        | n < 2 = 1
        | otherwise = 1 + (eulerTotientLength $ eulerTotient n)

-- https://en.wikipedia.org/wiki/Euler's_totient_function#Euler.27s_product_formula
eulerTotient :: Int -> Int
eulerTotient = Memo.integral eulerTotient'
 where eulerTotient' n
        | n < 2 = 1
        | otherwise = let inner n = 1 - (1 % n)
                      in ceiling ((fromIntegral n) * (product $ map (inner) (nub $ prime_factors n)))

prime_factors :: Integral t => t -> [t]
prime_factors n = factor n primes
 where factor n (p:ps) | isPrime n = [n]
                       | p > n = []
                       | n `mod` p == 0 = p : factor (n `div` p) (p:ps)
                       | otherwise = factor n ps

main :: IO ()
main = do
   let solution = runEval $ do
       s1 <- rpar (sum (filter (\n -> eulerTotientLength (n - 1) == 24) $ takeWhile (< 10000000) primes))
       s2 <- rpar (sum (filter (\n -> eulerTotientLength (n - 1) == 24) $ takeWhile (< 20000000) $ dropWhile (< 10000000) primes))
       s3 <- rpar (sum (filter (\n -> eulerTotientLength (n - 1) == 24) $ takeWhile (< 30000000) $ dropWhile (< 20000000) primes))
       s4 <- rpar (sum (filter (\n -> eulerTotientLength (n - 1) == 24) $ takeWhile (< 40000000) $ dropWhile (< 30000000) primes))
       rseq s1
       rseq s2
       rseq s3
       rseq s4
       return (s1 + s2 + s3 + s4)

   print $ solution
