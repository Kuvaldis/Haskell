doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y
doubleSmallNumber x = if x > 100 then x else x * 2
boomBangs xs = [if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]
length' xs = sum [1 | _ <- xs]
removeNonUppercase st = [c | c <- st, c `elem` ['A'..'Z']]
removeNonUppercase' :: [Char] -> [Char]
removeNonUppercase' st = [c | c <- st, c `elem` ['A'..'Z']]
factorial :: Integer -> Integer
factorial n = product [1..n]
lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"
factorial' :: (Integral a) => a -> a
factorial' 0 = 1
factorial' n = n * factorial' (n - 1)

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors a b = (fst a + fst b, snd a + snd b)
-- or pattern matching way
addVectors1 (a1, a2) (b1, b2) = (a1 + b1, a2 + b2)

-- pattern matching
first (a, b, c) = a
second (a, b, c) = b
third (a, b, c) = c
-- ignore other params
first1 (a, _, _) = a
second1 (_, b, _) = b
third1 (_, _, c) = c

-- sums pairs
pairSum xs = [a + b | (a, b) <- xs]

-- head of the list
head' [a] = a
head' [] = error "Empty list"
head' (h:_) = h

recursiveLength :: (Num b) => [a] -> b
recursiveLength [] = 0
recursiveLength (_:tail) = 1 + recursiveLength tail

recursiveSum :: (Num a) => [a] -> a
recursiveSum [] = 0
recursiveSum (head:tail) = head + recursiveSum tail
-- keep the whole string in variable 'input'
capital :: String -> String
capital "" = "Empty string"
capital input@(head:tail) = "The first letter of " ++ input ++ " is " ++ [head]