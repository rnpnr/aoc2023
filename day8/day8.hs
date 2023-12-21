-- AOC 2023: Day 8 - Haunted Wasteland
--
-- Suppose you have a graph where each node contains two edges and
-- a list of instructions telling you how to navigate the graph. Find
-- the number of steps necessary to get from node "AAA" to node
-- "ZZZ". If after following all the instructions you are not at node
-- "ZZZ" loop back to the first instruction and continue until
-- reaching "ZZZ"
-- Example input:
--
-- RL
--
-- AAA = (BBB, CCC)
-- BBB = (DDD, EEE)
-- CCC = (ZZZ, GGG)
-- DDD = (DDD, DDD)
-- EEE = (EEE, EEE)
-- GGG = (GGG, GGG)
-- ZZZ = (ZZZ, ZZZ)
--
-- Test Output: 2
--
-- Task 2: Suppose instead that you want to start at all nodes
-- "**A" and want to find the _shortest_ length necessay until all
-- paths end on "**Z" simultaneously. (No reasonable test input for
-- this part that works with the actual input).
--
import Data.Char
import System.Environment

readb26 :: String -> Int
readb26 = foldl (\a n -> a * 26 + (ord n - ord 'A')) 0 . map toUpper

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort lhs ++ [x] ++ qsort rhs
  where
    lhs = filter (< x) xs
    rhs = filter (>= x) xs

splitOn :: Char -> String -> [String]
splitOn c "" = []
splitOn c s =
    if skipfirst
        then [] ++ (splitOn c $ tail s)
        else [f] ++ splitOn c r
  where
    skipfirst = c == head s
    (f, r) = span (/= c) s

pathfind :: Ord a => a -> [(a, (a, a))] -> Int -> Int -> (a, a)
pathfind i ps l h
    | i == midv = midp
    | i < midv = pathfind i ps l (midpoint - 1)
    | i > midv = pathfind i ps (midpoint + 1) h
  where
    midpoint = div (l + h) 2
    (midv, midp) = ps !! midpoint

tokeyval :: String -> (Int, (Int, Int))
tokeyval s = (k, val)
  where
    (ks, vals) = span (/= '=') s
    k = readb26 ks
    val =
        (\(l, r) -> (readb26 l, readb26 $ tail r)) $
        break (== ',') $ drop 2 $ init vals

toflist :: String -> [((a, a) -> a)]
toflist "" = []
toflist (x:xs)
    | x == 'L' = [fst] ++ toflist xs
    | x == 'R' = [snd] ++ toflist xs

traversenet :: Int -> Int -> [((Int, Int) -> Int)] -> [(Int, (Int, Int))] -> Int
traversenet loc dest (p1:pr) network
    | loc == dest = 0
    | otherwise = 1 + traversenet nextloc dest (pr ++ [p1]) network
  where
    nextloc = p1 $ pathfind loc network 0 (length network - 1)

-- hardcoded to look for last char == 'Z'
travnet2 :: Int -> [((Int, Int) -> Int)] -> [(Int, (Int, Int))] -> Int
travnet2 loc (p1:pr) network
    | done = 0
    | otherwise = 1 + travnet2 nextloc (pr ++ [p1]) network
  where
    done = mod loc 26 == readb26 "Z"
    nextloc = p1 $ pathfind loc network 0 (length network - 1)

-- In this problem instruction length is a factor of all paths;
-- this doesn't have to be so but is for this specific input.
-- Should technically verify that plens[n] / ilen is prime
-- and that ilen and ilen^2 aren't in plens but this is
-- all satisfied for this input
getlcm :: Int -> [Int] -> Int
getlcm ilen plens = foldl (*) ilen $ map (\x -> div x ilen) plens

parse [] = getContents
parse fs = concat `fmap` mapM readFile fs

main = do
    fs <- getArgs >>= parse
    let (path, net) =
            (\(a, b) -> (a, splitOn '\n' b)) $
            span (/= '\n') $ filter (/= ' ') fs
    let nmap = qsort $ map tokeyval net
    let pmap = toflist path
    let plen = traversenet (readb26 "AAA") (readb26 "ZZZ") pmap nmap
    let slocs = filter (\x -> mod x 26 == readb26 "A") $ map fst nmap
    let pplen = getlcm (length pmap) $ map (\x -> travnet2 x pmap nmap) slocs
    putStrLn $ "Task 1: " ++ show plen
    putStrLn $ "Task 2: " ++ show pplen
