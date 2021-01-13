import Data.List
import Data.Array

isPrime :: Int -> Bool
isPrime n = if n == 1 then False else primeTest n maxVal 2
    where
    maxVal = iSqrt n

--recursive helper function for isPrime
primeTest :: Int -> Int -> Int -> Bool
primeTest n maxVal factor =
    if factor <= maxVal
        then if n `mod` factor == 0
            then False
            else primeTest n maxVal (factor + 1)
    else True

primes :: [Int]
primes = 2 : [ x | x <- filter isPrime [3..] ]

isPrimeFast :: Int -> Bool
isPrimeFast n = if n == 1 then False else primeTestFast n maxVal primesFast 0
    where
    maxVal = iSqrt n

--recursive helper function for isPrimeFast
primeTestFast :: Int -> Int -> [Int] -> Int -> Bool
primeTestFast n maxVal factors index =
    if (factors !! index) <= maxVal
        then if n `mod` (factors !! index) == 0
            then False
            else primeTestFast n maxVal factors (index + 1)
    else True

primesFast :: [Int]
primesFast =
    2 : [ x | x <- filter isPrimeFast [3..] ]

lcsLength :: String -> String -> Int
lcsLength s1 s2 = length(a!((1 + length s2), (1 + length s1))) where
    a = array ((1,1), ((1 + length s2), (1 + length s1)))
         ([((1,j), "") | j <- [1..(1 + length s1)]] ++
          [((i,1), "") | i <- [2..(1 + length s2)]] ++
          [((i,j), if s1!!(j - 2) == s2!!(i - 2) then
              a!(i - 1, j - 1) ++ [s1!!(j - 2)]
              else
                  if length(a!(i, j - 1)) >= length(a!(i - 1, j))
                      then a!(i, j - 1)
                      else a!(i - 1, j))
                      | i <- [2..(1 + length s2)], j <- [2..(1 + length s1)]])

iSqrt :: Int -> Int
iSqrt n = floor(sqrt(fromIntegral n))

main = putStrLn(show(lcsLength "Artist" "Artsy"))
