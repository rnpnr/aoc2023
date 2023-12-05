-- AOC 2023: Day 4 - Scratchcards
--
-- Suppose you have a list of prescratched scratchcards and thus
-- know both the numbers on the card and the list of winning numbers
-- for the card. The data looks like:
--
-- Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
-- Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
-- Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
-- Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
-- Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
-- Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
--
-- Task 1: A cards score starts at 0 and is doubled for each winning
-- number (in this case 0 * 2 = 1) i.e. after the first win left shift
-- the result once for each additional win. Provide the sum of scores.
-- Test Output: 13
--
-- Task 2: Each win gives you more of the subsequent scratchcards.
-- Card 1 (4 winning numbers) gives you an additional copy of each of
-- Cards 2-5. Sum up the total number of cards by the end.
-- Test Output: 30
--
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TI
import Text.Printf (printf)

shiftsum :: Num a => [a] -> a
shiftsum [] = 0
shiftsum (x:xs) = foldl (\a _ -> a * 2) 1 xs

tointlists :: ([T.Text], [T.Text]) -> ([Int], [Int])
tointlists (h, t) =
    let f = map (\x -> read x :: Int) . filter (/= "") . tail . map T.unpack
     in (f h, f t)

numpairs :: T.Text -> ([Int], [Int])
numpairs =
    tointlists . span (/= "|") . T.splitOn " " . T.tail . T.dropWhile (/= ':')

gamescore :: T.Text -> Int
gamescore st =
    let (w, n) = numpairs st
     in shiftsum $ filter (`elem` w) n

countcards :: [(Int, Int)] -> [(Int, Int)]
countcards [] = []
countcards c =
    let h = head c
        t = tail c
        p =
            zip
                ((replicate (snd h) (fst h)) ++ (repeat 0))
                (replicate (length t) 0)
        nt = zipWith (\x y -> ((fst x) + (fst y), snd y)) p t
     in [h] ++ (countcards nt)

gamewins :: [T.Text] -> [Int]
gamewins = map (\(a, b) -> length $ filter (`elem` a) b) . map numpairs

main = do
    contents <- TI.getContents
    let games = T.lines contents
    printf "Task 1: %8d\n" (sum $ map gamescore games)
    let cardcount = map fst $ countcards $ zip (repeat 1) $ gamewins games
    printf "Task 2: %8d\n" (sum cardcount)
