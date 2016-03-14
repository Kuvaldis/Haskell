import Data.List
-- only nub and sort are imported
-- import Data.List (nub, sort)
-- import everything except nub
-- import Data.List hiding (nub)
-- qualified import. Means you have to write Data.Map.<function name> explicitly or some short name using 'as'.
-- Needed to prevent name clashes
-- import qualified Data.Map as M

-- num removes duplicates
numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

-- put given element in between elements in the list
-- intersperse '.' "MONKEY" == "M.O.N.K.E.Y"
-- put given element in between elements in the list, compose and flatten
-- intercalate " " ["hey", "you", "there"] == "hey you there"
-- concats
-- concat ["foo","bar","car"] == "foobarcar"
-- maps and concats
-- concatMap (replicate 4) [1..3] == [1,1,1,1,2,2,2,2,3,3,3,3]
-- performs and for each element. there are also or, any, all,
-- and $ map (>4) [5,6,7,8] == False
-- iterates. gets 1, then 2, then 4 etc. infinitely
-- iterate (*2) 1

-- count uniques number
numUniquesWithValues :: (Ord a) => [a] -> [(a, Int)]
numUniquesWithValues xs = map (\l@(x:xs) -> (x, length l)) . group . sort $ xs
-- numUniquesWithValues [1,1,1,1,2,2,2,2,3,3,2,2,2,5,6,7] == [(1,4),(2,7),(3,2),(5,1),(6,1),(7,1)]

search :: (Eq a) => [a] -> [a] -> Bool
search needle haystack =
    let nlen = length needle
    -- tails: "ABC" -> ["ABC", "BC", "C", ""]
    in  foldl (\acc x -> if take nlen x == needle then True else acc) False (tails haystack)