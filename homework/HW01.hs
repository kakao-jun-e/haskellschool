{-# OPTIONS_GHC -Wall #-}
module HW01 where

-- Exercise 1 -----------------------------------------

-- Get the last digit from a number
lastDigit :: Integer -> Integer
lastDigit = flip mod 10

-- Drop the last digit from a number
dropLastDigit :: Integer -> Integer
dropLastDigit = flip div 10

-- Exercise 2 -----------------------------------------

toRevDigits :: Integer -> [Integer]
toRevDigits digits
  | digits <= 0 = []
  | otherwise   = getLast digits : getRest digits
  where getLast = lastDigit
        getRest = (toRevDigits . dropLastDigit)

-- Exercise 3 -----------------------------------------

-- Double every second number in a list starting on the left.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (x:xs) = x : (2 * head xs) : doubleEveryOther (tail xs)

-- Exercise 4 -----------------------------------------

-- Calculate the sum of all the digits in every Integer.
sumDigits :: [Integer] -> Integer
sumDigits = foldl (\acc x -> acc + (lastDigit x + dropLastDigit x)) 0

-- Exercise 5 -----------------------------------------

-- Validate a credit card number using the above functions.
luhn :: Integer -> Bool
luhn card_number = (==) 0 (lastDigit . sumDigits . doubleEveryOther $ toRevDigits card_number)

-- Exercise 6 -----------------------------------------

-- Towers of Hanoi for three pegs
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move] -- disc from to temp
hanoi 0 _ _ _ = []
hanoi n a c b = hanoi (n - 1) a b c ++ [(a, c)] ++ hanoi (n - 1) b c a
