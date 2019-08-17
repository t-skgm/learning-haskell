-- {-# OPTIONS -Wall -Werror #-}

module Sec3 where

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

{-}
bmiTell :: Double -> String
bmiTell bmi
    | bmi <= 18.5 = "You're underweight, you emo, you!"
    | bmi <= 25.0 = "You're supposedly normal.\
                    \ Pffft, I bet you're ugly!"
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
    | otherwise = "You're a whale, congratulations!"
-}
bmiTell :: Double -> Double -> String
bmiTell weight height
    | weight / height ^ (2 :: Int) <= 18.5 = "You're underweight, you emo, you!"
    | weight / height ^ (2 :: Int) <= 25.0 = "You're supposedly normal.\
                                             \ Pffft, I bet you're ugly!"
    | weight / height ^ (2 :: Int) <= 30.0 = "You're fat!\
                                             \ Lose some weight, fatty!"
    | otherwise                            = "You're a whale, congratulations!"

max' :: (Ord a) => a -> a -> a
max' a b
    | a <= b    = b
    | otherwise = a

compare' :: (Ord a) => a -> a -> Ordering
a `compare'` b
    | a == b    = EQ
    | a <= b    = LT
    | otherwise = GT

-- 3.3

{-
bmiTell' :: Double -> Double -> String
bmiTell' weight height
    | bmi <= 18.5 = "You're underweight, you emo, you!"
    | bmi <= 25.0 = "You're supposedly normal.\
                    \ Pffft, I bet you're ugly!"
    | bmi <= 30.0 = "You're fat!\
                    \ Lose some weight, fatty!"
    | otherwise   = "You're a whale, congratulations!"
    where bmi = weight / height ^ (2 :: Int)
-}

bmiTell' :: Double -> Double -> String
bmiTell' weight height
    | bmi <= skinny = "You're underweight, you emo, you!"
    | bmi <= normal = "You're supposedly normal.\
                      \ Pffft, I bet you're ugly!"
    | bmi <= fat    = "You're fat! Lose some weight, fatty!"
    | otherwise     = "You're a whale, congratulations!"
    where bmi = weight / height ^ (2 :: Int)
          skinny = 18.5
          normal = 25.0
          fat = 30.0

{-
greet :: String -> String
greet "Juan" = niceGreeting ++ " Juan!"
greet "Fernando" = niceGreeting ++ " Fernando!"
-- => badGreetingはスコープ外
greet name = badGreeting ++ " " ++ name
    where niceGreeting = "Hello! So very nice to see you,"
          badGreeting = "Oh! Pfft. It's you."
-}

badGreeting :: String
badGreeting = "Oh! Pfft. It's you."
niceGreeting :: String
niceGreeting = "Hello! So very nice to see you,"

greet :: String -> String
greet "Juan" = niceGreeting ++ " Juan!"
greet "Fernando" = niceGreeting ++ " Fernando!"
greet name = badGreeting ++ " " ++ name

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname

initials' (f:_) (l:_) = [f] ++ ". " ++ [l] ++ "."

calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [bmi w h | (w, h) <- xs]
    where bmi weight height = weight / height ^ (2 :: Int)

-- 3.4

cylinder :: Double -> Double -> Double
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea  = pi * r ^ 2
    in sideArea + 2 * topArea

-- 4 * (let a = 9 in a + 1) + 2

calcBmis' :: [(Double, Double)] -> [Double]
calcBmis' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]

calcBmis'' :: [(Double, Double)] -> [Double]
calcBmis'' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi > 25.0]
-- `(w, h) <- xs` is called "generator". generator can't refer to `let`-ed variables

-- 3.5

head_ :: [a] -> a
head_ [] = error "No head for empty lists!"
head_ (x:_) = x
-- equals
head_' :: [a] -> a
head_'  xs = case xs of [] -> error "No head for empty lists!"
                        (x:_) -> x

describeList :: [a] -> String
describeList ls = "The list is "
                  ++ case ls of [] -> "empty."
                                [x] -> "a singleton list."
                                xs -> "a longer list."

describeList' :: [a] -> String
describeList' ls = "The list is " ++ what ls
    where what [] = "empty."
          what [x] = "a singleton list."
          what xs = "a longer list."
