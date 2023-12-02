-- AOC 2023: Day 1
-- Given a list of newline separated strings containing ASCII digits
-- interspersed among other characters take the first and last ASCII
-- digit from each line as a 2 digit number then sum all of them together
-- 
-- Example (Part 1):
-- treb7uchet -> 77
-- 1abc2 -> 12
-- (Output) 89
--
-- Part 2: English names are also intersperced with the digits and
-- are also valid for making up the first and last digits.
--
-- Example (Part 2):
-- 4nineeightseven2 -> 42
-- zoneight234 -> 14
-- (Output) 56
--
{-# LANGUAGE OverloadedStrings #-}

import Data.Char
import qualified Data.Text as T
import qualified Data.Text.IO as TI

stripalpha :: T.Text -> T.Text
stripalpha = T.filter (`elem` ['0' .. '9'])

tocode :: T.Text -> Int
tocode st =
    let h = T.head st
        t = T.last st
        z = ord '0'
     in 10 * (ord h - z) + (ord t - z)

replmap :: (T.Text, T.Text) -> (T.Text -> T.Text)
replmap (f, s) = T.replace f s

repstrnums :: T.Text -> T.Text
repstrnums =
    let pairs =
            [ ("one", "o1e")
            , ("two", "t2o")
            , ("three", "t3e")
            , ("four", "4")
            , ("five", "5e")
            , ("six", "6")
            , ("seven", "7n")
            , ("eight", "e8t")
            , ("nine", "n9e")
            ]
     in foldr1 (.) $ map replmap pairs

getcal :: [T.Text] -> [Int]
getcal = map $ tocode . stripalpha

main = do
    contents <- TI.getContents
    print $ sum $ getcal $ T.lines contents
    print $ sum $ getcal $ T.lines $ repstrnums contents
