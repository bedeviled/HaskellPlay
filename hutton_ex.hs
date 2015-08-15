import System.Environment
import Data.Char

main :: IO()
main = getArgs >>= print . head

--halve :: [a] -> ([a],[a])
--halve xs = recur ([],xs)
--  where recur (as,bs) | length as == length bs = (as,bs)
--                      | otherwise = recur (as ++ [head bs], tail bs)

safetailCond :: [a] -> [a]
safetailCond xs = if null xs then [] else tail xs

safetailGuard :: [a] -> [a]
safetailGuard xs | null xs = []
            | otherwise = tail xs

safetailPat :: [a] -> [a]
safetailPat [] = []
safetailPat xs = tail xs

myOr1 :: Bool -> Bool -> Bool
True `myOr1` True = True
True `myOr1` False = True
False `myOr1` True = True
False `myOr1` False = False

myOr2 :: Bool -> Bool -> Bool
False `myOr2` False = False
_ `myOr2` _ = True


myOr3 :: Bool -> Bool -> Bool
True `myOr3` _  = True
False `myOr3` b = b

--myAnd1 :: Bool -> Bool -> Bool
--p `myAnd1` q = if p then q else False

--myAnd2 :: Bool -> Bool -> Bool
--p `myAnd2` q = if p then
--                if q then True
--                  else False
--               else False


--mult :: Integer -> Integer -> Integer -> Integer
--mult = \z -> (\y -> (\x -> x * y * z))

sumOfSquares :: Integer -> Integer
sumOfSquares n | n <= 0 = n
               | otherwise = sum [x*x | x <- [1 .. n]]

myReplicate1 :: Integer -> a -> [a]
myReplicate1 n x = [x | _ <- [1 .. n]]

pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x,y,z) | x <- [1 .. n], y <- [1 .. n], z <- [1 .. n], x*x + y*y == z*z]

factors :: Int -> [Int]
factors n = [x | x <- [1 .. n], n `mod` x == 0]

perfects :: Int-> [Int]
perfects n = [x | x <- [1 .. n], sum [y | y <- factors x, y < x] == x]

--concat [[(x,y) | y <- [4,5,6]] | x <- [1,2,3]]
--[(1,4),(1,5),(1,6),(2,4),(2,5),(2,6),(3,4),(3,5),(3,6)]


find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k',v) <- t, k == k']

positions :: Eq a => a -> [a] -> [Int]
positions k t = find k (zip t [0 .. n])
                  where n = length t - 1

scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x*y | (x,y) <- zip xs ys]

let2int :: Char -> Int
let2int c | isLower c = ord c - ord 'a' + 26
          | isUpper c = ord c - ord 'A'

int2let :: Int -> Char
int2let n | n < 26    = chr (ord 'A' + n)
          | otherwise = chr (ord 'a' - 26 + n)


shift :: Int -> Char -> Char
shift n c | isAlpha c = int2let ((let2int c + n) `mod` 52)
          | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

table :: [Float]
table = [8.2, 1.5, 2.8, 4.3, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4,
         6.4, 7.5, 1.9, 0.1, 6.0, 6.3, 9.1, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

alphas :: String -> Int
alphas xs = length [x | x <- xs, isAlpha x]

countIgnoreCase :: Char -> String -> Int
countIgnoreCase x xs = length [x' | x' <- xs, toLower x == toLower x']

freqs :: String -> [Float]
freqs xs = [percent (countIgnoreCase x xs) n | x <- ['a' .. 'z']]
           where n = alphas xs

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o-e)^^2)/e | (o,e) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

crack :: String -> String
crack xs = encode (-factor) xs
  where
    factor = head (positions (minimum chitab) chitab)
    chitab = [chisqr (rotate n table') table | n <- [0 .. 25]]
    table' = freqs xs

-- Chapter 6 exercises
myExp :: Int -> Int -> Int
0 `myExp` _     = 0
_ `myExp` 0     = 1
myExp n m = n * myExp n (m-1)
-- myExp 2 3
-- 2 * (myExp 2 2)
-- 2 * (2 * (myExp 2 1))
-- 2 * (2 * (2 * (myExp 2 0)))
-- 2 * (2 * (2 * (1)))
-- 8

-- length [1,2,3]
-- 1 + (length [2,3])
-- 1 + (1 + (length [3]))
-- 1 + (1 + (1 + (length [])))
-- 1 + (1 + (1 + (0)))
-- 3

-- drop 3 [1,2,3,4,5]
-- drop 2 [2,3,4,5]
-- drop 1 [3,4,5]
-- drop 0 [4,5]
-- [4,5]

-- init [1,2,3]
-- 1 : (init [2.3])
-- 1 : (2 : (init [3]))
-- 1 : (2 : ([]))
-- 1 : 2 : []
-- [1,2]

myAnd :: [Bool] -> Bool
myAnd [b] = b
myAnd (b:bs) = b && myAnd bs

myConcat :: [[a]] -> [a]
myConcat [[]]     = []
myConcat [xs] = xs
myConcat (xs:xss) = xs ++ myConcat xss

myReplicate2 :: Int -> a -> [a]
myReplicate2 0 _ = []
myReplicate2 n x = x : myReplicate2 (n-1) x

myNth :: [a] -> Int -> a
myNth (x:xs) 0 = x
myNth (x:xs) n = myNth xs (n-1)

myElem :: Eq a => a -> [a] -> Bool
myElem x [] = False
myElem x (y:ys) | x == y    = True
                | otherwise = myElem x ys

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) | x < y     = x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys

-- merge [2,5,6] [1,3,4]
-- 1 : (merge [2,5,6] [3,4])
-- 1 : (2 : (merge [5,6] [3,4]))
-- 1 : (2 : (3 : (merge [5,6] [4])))
-- 1 : (2 : (3 : (4 : (merge [5,6] []))))
-- 1 : (2 : (3 : (4 : [5,6])))
-- [1,2,3,4,5,6]
halve :: [a] -> ([a],[a])
halve [] = ([],[])
halve xs = (take n xs, drop n xs)
  where n = quot (length xs) 2

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort left) (msort right)
  where left = fst halves
        right = snd halves
        halves = halve xs

-- msort [3,1,2,5,4,6]
-- merge (msort [3,1,2]) (msort [5,4,6])
-- merge (merge (msort [3]) (msort [1,2])) (merge (msort [5]) (msort [4,6]))
-- merge (merge ([3]) (merge (msort [1]) (msort [2])) (merge ([5]) (merge (msort [4]) (msort [6]))))
-- merge (merge [3] (merge [1] [2])) (merge [5] (merge [4] [6]))
-- merge (merge [3] [1,2]) (merge [5] [4,6])
-- merge [1,2,3] [4,5,6]
-- [1,2,3,4,5,6]

mySum :: Num a => [a] -> a
mySum [] = 0
mySum (x:xs) = x + mySum xs

myTake :: Int -> [a] -> [a]
myTake 0 xs = []
myTake n [] = []
myTake n (x:xs) = x : myTake (n-1) xs

myLast :: [a] -> a
myLast [x] = x
myLast (x:xs) = myLast xs

-- Chapter 7 exercises
-- [f x | x <- xs, p x]
-- map f . filter p

-- all, any, takeWhile, dropWhile
myAll :: (a -> Bool) -> [a] -> Bool
myAll p = foldr (\x b -> p x && b) True

myAny :: (a -> Bool) -> [a] -> Bool
myAny p = not . myAll (not . p)

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile p [] = []
myTakeWhile p (x:xs) | p x = x : myTakeWhile p xs
                     | otherwise = []

myDropWhile :: (a -> Bool) -> [a] -> [a]
myDropWhile p [] = []
myDropWhile p (x:xs) | p x = myDropWhile p xs
                     | otherwise = x:xs

-- map f, filter p using foldr
myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\x xs -> f x : xs) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p = foldr pp []
  where pp x xs | p x = x : xs
                | otherwise = xs

dec2int :: [Int] -> Int
dec2int = foldl (\m n -> 10*m + n) 0

compose :: [a -> a] -> (a->a)
compose = foldr (.) id

-- sumsqreven = compose [sum, map (^2), filter even]
-- is incorrect because while map (^2) and filter even take type a = [b] to type a = [b]
-- sum takes type c = [a] to type a != c.

myCurry :: ((a,b) -> c) -> a -> b -> c
myCurry f x y = f (x,y)

myUncurry :: (a -> b -> c) -> ((a,b) -> c)
myUncurry f = \(x,y) -> f x y

unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x | p x = []
               | otherwise = h x : unfold p h t (t x)

type Bit = Int

int2bin :: Int -> [Bit]
int2bin = unfold (==0) (`mod`2) (`div`2)

chop8 :: [Bit] -> [[Bit]]
chop8 = unfold null (take 8) (drop 8)

myOtherMap :: (a -> b) -> [a] -> [b]
myOtherMap f = unfold null (f . head) tail

myIterate :: (a -> a) -> a -> [a]
myIterate = unfold (const False) id
