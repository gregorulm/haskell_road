{-
Gregor Ulm
2014-06-25
-}

import Data.List

naturals = [0..]

smallSquares = [ n^2 | n <- naturals, n < 100 ]


reverseList :: [a] -> [a]
reverseList lst = reverse' lst []
    where reverse' []     acc = acc
          reverse' (x:xs) acc = reverse' xs (x:acc)

splitList :: [a] -> [([a],[a])]
splitList xs = split' [] (length xs - 1)
    where split' acc 0 = acc
          split' acc n = split' ((take n xs, drop n xs):acc) (n-1)

listDiff :: (Eq a) => [a] -> [a] -> [a]          
listDiff xs ys | null ys
                 || null xs = xs
               | otherwise  = let check = head ys
                              in if head ys `elem` xs
                                 then listDiff (delete check xs) (tail ys)
                                 else listDiff xs (tail ys)  
