{-# OPTIONS -Wall -Werror #-}

module Main where

main :: IO ()
main = putStrLn "Hello, world"

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

-- #3

-- 3.1

lucky :: Int -> String
lucky 7 = "LUCKY NUMBER SEVEN!!"
lucky _ = "Sorry, you're not out of luck, pal!"

sayMe :: Int -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe _ = "Not between 1 and 5"

factorial' :: Int -> Int
factorial' 0 = 1
factorial' n = n * factorial' (n - 1)

charName :: Char -> String
charName 'a' = "Albert"
charName 'b' = "Broseph"
charName 'c' = "Cecil"
-- > charName 'x' => Pattern match(es) are non-exhaustive
charName _ = "Who are you?"

addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
-- without pattern match
--   addVectors a b = (fst a + fst b, snd a + and b)
-- with pattern match
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- equal fst
first :: (a, b, c) -> a
first (x, _, _) = x
-- equal snd
second :: (a, b, c) -> b
second (_, y, _) = y
-- !!
third :: (a, b, c) -> c
third (_, _, z) = z

-- let xs = [(1,3),(4,3),(2,4),(5,3),(5,6),(3,1)]
-- > [a+b | (a, b) <- xs]
-- [4,7,6,8,11,4]
-- > [x*100+3 | (x, 3) <- xs]
-- [103,403,503]

head' :: [a] -> a
head' [] = error "Can't call head on an empty list, dummy!"
head' (x:_) = x
-- > head' [1, 2, 3]
-- 1

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
-- (x:y:_) !
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y

-- as pattern
firstLetter :: String -> String
firstLetter "" = "Empty string, whoops!"
firstLetter allchar@(x:_) = "The first letter of " ++ allchar ++ " is " ++ [x]

-- 3.2

