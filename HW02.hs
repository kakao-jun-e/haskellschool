{-# OPTIONS_GHC -Wall #-}
module HW02 where

import Data.List

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches secret guess = length $ filter (==True) $ zipWith (==) secret guess

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors guess = map (\color -> sum [1 | x<-[color], y<-guess, x==y]) colors

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches secret guess = sum $ zipWith (min) (countColors secret) (countColors guess)

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove secret guess = Move guess exact_count nonexact_count
  where exact_count = (exactMatches secret guess)
        nonexact_count = (matches secret guess) - exact_count

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent move secret = (==) move $ getMove secret guess
  where Move guess _ _ = move

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes move = filter (isConsistent move)

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes len
  | len == 0 = []
  | len == 1 = group colors
  | otherwise = concatMap (\color -> map (color:) $ allCodes (len-1)) colors

-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve secret = getMoves $ allCodes $ length secret
  where
    getMoves guesses
      | length guesses == 1 = [move]
      | otherwise = move : (getMoves $ filterCodes move guesses)
      where
        move = (getMove secret $ head guesses)

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined
