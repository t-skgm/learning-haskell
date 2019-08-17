{-# OPTIONS -Wall -Werror #-}

module Sec2 where

-- #2

-- 2.1

removeNonUpperCase :: [Char] -> [Char]
removeNonUpperCase st = [ c | c <- st, c `elem` ['A'..'Z']]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

-- 2.2

factorial :: Integer -> Integer
factorial n = product [1..n]

circumference :: Float -> Float
circumference r = 2 * pi * r

circumference' :: Double -> Double
circumference' r = 2 * pi * r

-- 2.4

{-
Eq, Ord, Show, Read, Enum, Bounded
Num, Floating (Float, Double), Integral (Int, Integer)

> length [1, 2] + 3.2
<interactive>:17:17: error:
  • No instance for (Fractional Int) arising from the literal ‘3.2’

> fromIntegral (length [1, 2]) + 3.2
5.2

> :t fromIntegral (length [1, 2])
fromIntegral (length [1, 2]) :: Num b => b
-}
