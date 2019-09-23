{-# OPTIONS -Wall -Werror #-}

module Sec5 where

multThree :: Int -> Int -> Int -> Int
-- multThree :: Int -> (Int -> (Int -> Int))
multThree x y z = x * y * z
-- let multTwoWithNine = multThree 9
--   multTwoWithNine 2 3

compareWithHundred :: Int -> Ordering
-- compareWithHundred x = compare 100 x
compareWithHundred = compare 100

divideByTen :: (Floating a) => a -> a
divideByTen = (/10) -- (/) => `Section`

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])

subtract4 :: (Num a) => a -> a
subtract4 = (subtract 4) -- you can't use (-4)

-- 5.2
