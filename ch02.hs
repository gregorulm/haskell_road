{-
Gregor Ulm
2014-06-24
-}

import Data.List

truthValues :: [(Bool, Bool)]
truthValues = [(True, True),(True, False),(False, True), (False, False)]

infix 1 <=>
(<=>) :: Bool -> Bool -> Bool
x <=> y = x == y 

infixr 2 <+>
(<+>) :: Bool -> Bool -> Bool
x <+> y = x /= y 

-- exclusive or equals not(P <=> Q)
eq1 :: Bool
eq1 = all (\(p, q) -> p <+> q == not p <=> q) truthValues

-- (P <+> Q) <+> Q == P
eq2 :: Bool
eq2 = all (\(p, q) -> (p <+> q) <+> q == p) truthValues

unique :: (a -> Bool) -> [a] -> Bool
unique p xs = length  (filter (== True) evaluations) == 1
    where evaluations = map p xs
-- e.g. unique even [1,3,4]

parity :: [Bool] -> Bool
parity = even . length . filter (== True)
-- e.g. parity [True, False, True]

evenNR :: (a -> Bool) -> [a] -> Bool
evenNR p = parity . map p
-- e.g. evenNR even [2,3,4,5]
