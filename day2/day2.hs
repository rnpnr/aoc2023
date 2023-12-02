-- AOC 2023: Day 2
--
-- There is a game where you want to determine the number of each
-- colour of cube in bag. The bag contains red, green, and blue
-- cubes.
--
-- Task 1: Grab a random handful of cubes and count each colour.
-- Do this a few times per game and perform many games. This will
-- result in a list of data such as:
--
-- Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
-- ...
-- Game N: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
--
-- After this data was collected you are told that the bag
-- contained only 12 red, 13 green, and 14 blue cubes. Provide
-- the sum of all the valid game's numbers.
--
-- Task 2: The power of a set of cubes is the minimum number of
-- each colour needed to make a game valid. Provide the sum of
-- the powers of all games.
--
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import Text.Printf (printf)

splitgame :: T.Text -> (Int, [(Int, T.Text)])
splitgame st =
    let (gn, t) = T.breakOn (T.pack ": ") st
        n = read $ T.unpack $ snd $ T.break (== ' ') gn :: Int
        tuples =
            map to_int_text . map (T.break (== ' ')) $
            T.splitOn (T.pack ", ") $
            T.replace (T.pack ": ") (T.pack "") $
            T.replace (T.pack ";") (T.pack ",") t
     in (n, tuples)

to_int_text :: (T.Text, T.Text) -> (Int, T.Text)
to_int_text (f, s) = (read (T.unpack f) :: Int, s)

get_colour_list :: T.Text -> [(Int, T.Text)] -> [Int]
get_colour_list c l = map fst $ filter ((== c) . snd) l

get_rgb_triple :: [(Int, T.Text)] -> ([Int], [Int], [Int])
get_rgb_triple l =
    let r = get_colour_list (T.pack " red") l
        g = get_colour_list (T.pack " green") l
        b = get_colour_list (T.pack " blue") l
     in (r, g, b)

isvalidgame :: (Int, [(Int, T.Text)]) -> Int
isvalidgame (n, t) =
    let (r, g, b) = get_rgb_triple t
     in if (null $ filter (> 12) r) &&
           (null $ filter (> 13) g) && (null $ filter (> 14) b)
            then n
            else 0

gamepower :: (Int, [(Int, T.Text)]) -> Int
gamepower (_, t) =
    let (r, g, b) = get_rgb_triple t
     in (foldr1 max r) * (foldr1 max g) * (foldr1 max b)

main = do
    contents <- TI.getContents
    let games = map splitgame $ T.lines contents
    printf "Task 1: %8d\n" (sum $ map isvalidgame games)
    printf "Task 2: %8d\n" (sum $ map gamepower games)
