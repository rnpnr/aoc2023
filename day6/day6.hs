-- AOC 2023: Day 6 - Wait For It
--
-- Given a set of (time, distance) pairs, an acceleration of 1
-- dist/time^2 (units don't matter), and the rule that while
-- accelerating you are not moving find the number of different ways
-- you can reach the distance (i.e. you accelerate for a some portion
-- of the time and then travel at the achieved velocity for the
-- remainder).
--
-- Example input:
-- Time:      7  15   30
-- Distance:  9  40  200
-- Test Output: 288
--
-- Task 2: There is actually only one time and distance in the
-- example input with some random space in between.
-- Test Output: 71503
--
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TI
import Text.Printf (printf)

texttoint :: T.Text -> Int
texttoint t = read (T.unpack t) :: Int

numlist :: T.Text -> [Int]
numlist = map texttoint . filter (/= "") . T.splitOn " " . T.strip

winningwaits :: (Int, Int) -> Int
winningwaits (t, d) = length $ [x | x <- [1 .. (t - 1)], x * (t - x) > d]

main = do
    contents <- TI.getContents
    let (t, d) = T.breakOn "\n" contents
    let times = T.dropWhile (/= ' ') t
    let dists = T.dropWhile (/= ' ') d
    let pairs = zip (numlist times) (numlist dists)
    let pairs2 =
            ( texttoint $ T.filter (/= ' ') times
            , texttoint $ T.filter (/= ' ') dists)
    printf "Task 1: %8d\n" (foldl (*) 1 $ map winningwaits pairs)
    printf "Task 2: %8d\n" (winningwaits pairs2)
