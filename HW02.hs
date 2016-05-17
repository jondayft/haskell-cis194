{-# OPTIONS_GHC -Wall #-}
module HW02 where

import Data.List  
import Control.Monad 

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
exactMatches actual guess = length . filter (uncurry (==)) $ zip actual guess

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors guess = map (\color -> length . filter (==color) $ guess) colors

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches actual guess = sum . min (countColors actual) $ countColors guess

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove secret guess = Move guess exactMatch regularMatch
  where exactMatch = exactMatches secret guess
        regularMatch = matches secret guess - exactMatch

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent move@(Move moveCode _ _) code = move == getMove code moveCode 

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes = filter . isConsistent

-- Exercise 6 -----------------------------------------

allCodes :: () -> [Code]
allCodes = permutations colors

nAllCodes :: Int -> [Code]
nAllcodes n = replicateM n colors

-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve = undefined

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined
