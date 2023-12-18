-- AOC 2023: Day 7 - Camel Cards
--
-- Camel Cards is a game like poker except each player is dealt 5
-- cards from the start and the order they are received in is
-- important. In this example a list of hands are given along with a
-- bid amount for each hand. The hands should be ranked and sorted -
-- if their are N hands the worst hand will be assigned rank 1 and
-- the best will be assigned rank N+1. Anytime two hands are equal
-- (eg. both are full house) then the cards should be compared from
-- left to right with and the first hand with a higher value card
-- wins (Aces are highest).
--
-- Example input:
-- 32T3K 765 -- one pair, rank 1
-- T55J5 684 -- three of a kind, rank 4 (T < Q)
-- KK677 28  -- two pair, rank 3 (K > T)
-- KTJJT 220 -- two pair, rank 2 (T < K)
-- QQQJA 483 -- three of a kind, rank 5 (Q > T)
-- Test Output: 6404
--
-- Task 2: 'J' cards are now jokers. For the purpose of creating
-- hands they are wild cards. For comparing equal scoring hands they
-- have the lowest value.
-- Same Input:
-- 32T3K 765 -- one pair, rank 1
-- T55J5 684 -- four of a kind, rank 3
-- KK677 28  -- two pair, rank 2
-- KTJJT 220 -- four of a kind, rank 5
-- QQQJA 483 -- four of a kind, rank 4
-- Test Output: 5905
--
type Rank = Int

type Hand = String

type Bid = Int

data CamelHand =
    CamelHand Rank Hand Bid
    deriving (Eq, Show)

data Cond a =
    a :? a

infixl 0 ?

infixl 1 :?

(?) :: Bool -> Cond a -> a
True ? (x :? _) = x
False ? (_ :? y) = y

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort lhs ++ [x] ++ qsort rhs
  where
    rhs = filter (< x) xs
    lhs = filter (>= x) xs

cameltuple :: CamelHand -> (Rank, Hand, Bid)
cameltuple (CamelHand r h b) = (r, h, b)

cardval :: Bool -> Char -> Int
cardval p2 c
    | c == 'A' = 14
    | c == 'K' = 13
    | c == 'Q' = 12
    | c == 'J' = p2 ? 1 :? 11
    | c == 'T' = 10
    | c == '9' = 9
    | c == '8' = 8
    | c == '7' = 7
    | c == '6' = 6
    | c == '5' = 5
    | c == '4' = 4
    | c == '3' = 3
    | c == '2' = 2

cardcounts :: Hand -> [Int]
cardcounts [] = []
cardcounts (x:xs) = [lf] ++ cardcounts rem
  where
    lf = (+ 1) . length $ filter (== x) xs
    rem = filter (/= x) xs

classifyhand :: [Int] -> Int
classifyhand (lh:lt)
    | ll == 1 = 6 -- five of a kind
    | ll == 2 = (lh == 4) ? 5 :? 4 -- four of a kind or full house
    | ll == 3 = (lh == 3) ? 3 :? 2 -- three of a kind or two pairs
    | ll == 4 = 1 -- one pair
    | otherwise = 0
  where
    ll = length lt + 1

handscore :: Bool -> Hand -> Int
handscore p2 hand = classifyhand hn
  where
    f = qsort . cardcounts
    nc = p2 ? (f $ filter (/= 'J') hand) :? f hand
    jc = length $ filter (== 'J') hand
    hn =
        not p2 ?
        nc :? [null nc ? 0 :? head nc + jc] ++ (null nc ? [] :? tail nc)

cmpeqhand :: Bool -> Hand -> Hand -> Int
cmpeqhand p2 (lh:lt) (rh:rt)
    | val == 0 = cmpeqhand p2 lt rt
    | otherwise = val
  where
    val = (cardval p2 lh) - (cardval p2 rh)

cmphand :: Bool -> Hand -> Hand -> Int
cmphand p2 lh rh
    | val == 0 = cmpeqhand p2 lh rh
    | otherwise = val
  where
    val = handscore p2 lh - handscore p2 rh

inserthand :: Bool -> [CamelHand] -> CamelHand -> [CamelHand]
inserthand p2 chands newchand
    | null chands = [newchand]
    | cmp > 0 = [n_n_chand] ++ chands
    | cmp < 0 = [n_o_chand] ++ inserthand p2 (tail chands) newchand
  where
    hCH = head chands
    (_, nhand, nbid) = cameltuple newchand
    (orank, ohand, obid) = cameltuple hCH
    cmp = cmphand p2 nhand ohand
    n_o_chand = CamelHand (orank + 1) ohand obid
    n_n_chand = CamelHand (orank + 1) nhand nbid

makecamelhand :: String -> CamelHand
makecamelhand = (\(c, b) -> CamelHand 1 c (read $ tail b)) . span (/= ' ')

handwinnings :: CamelHand -> Int
handwinnings (CamelHand r _ b) = r * b

main = do
    contents <- getContents
    let hands = map makecamelhand $ lines contents
    let handsp1 = foldl (inserthand False) [] hands
    let handsp2 = foldl (inserthand True) [] hands
    putStrLn $ "Task 1: " ++ show (foldl (+) 0 $ map handwinnings handsp1)
    putStrLn $ "Task 2: " ++ show (foldl (+) 0 $ map handwinnings handsp2)
