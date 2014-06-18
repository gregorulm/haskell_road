{-
Gregor Ulm
2014-06-18
-}

import Data.List

{-
minAny :: Ord a => [a] -> a
minAny xs | null xs   = error "empty list"
          | otherwise = foldl min (head xs) (tail xs) 

maxAny :: Ord a => [a] -> a
maxAny xs | null xs   = error "empty list"
          | otherwise = foldl max (head xs) (tail xs) 
-}

fAny :: Ord a => (a -> a -> a) -> [a] -> a
fAny f xs | null xs   = error "empty list"
          | otherwise = foldl f (head xs) (tail xs) 

minAny :: Ord a => [a] -> a
minAny = fAny min

maxAny :: Ord a => [a] -> a
maxAny = fAny max

removeFirst :: (Eq a) => a -> [a] -> [a]
removeFirst _ []     = []
removeFirst y (x:xs) | y == x    = xs
                     | otherwise = x : removeFirst y xs

sortAny :: (Ord a) => [a] -> [a]
sortAny [] = []
sortAny xs = m : sortAny (removeFirst m xs)
    where m = minAny xs

average :: [Integer] -> Double 
average [] = error "empty list" 
average xs = fromIntegral (sum xs) / fromIntegral (length xs)

countLetter :: Char -> String -> Int
countLetter l = length . filter (== l)

blowup :: String -> String
blowup xs = concatMap (uncurry replicate) (zip [1..] xs)

sortString :: [String] -> [String]
sortString = sortAny

prefix :: String -> String -> Bool
prefix [] ys         = True
prefix (x:xs) []     = False
prefix (x:xs) (y:ys) = (x==y) && prefix xs ys

substring :: String -> String -> Bool
substring xs [] = False
substring xs ys = prefix xs ys || substring xs (tail ys) 

lengths :: [[a]] -> [Int]
lengths = map length

sumLengths :: [[a]] -> Int
sumLengths = sum . lengths
