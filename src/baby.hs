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

-- guards
bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
    | bmi <= skinny = "You're underweight, you emo, you!"
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"
    | bmi <= fat = "You're fat! Lose some weight, fatty!"
    | otherwise   = "You're a whale, congratulations!"
-- where
-- pattern matching also works
    where bmi = weight / height ^ 2
          (skinny, normal, fat) = (18.5, 25.0, 30.0)
-- inline
max' :: (Ord a) => a -> a -> a
max' a b | a > b = a | otherwise = b
-- compare
myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
    | a > b = GT
    | a == b = EQ
    | otherwise = LT

-- where
initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname
describeListWhere :: [a] -> String
describeListWhere xs = "This is " ++ what xs
    where what [] = "empty."
          what [x] = "a singleton list."
          what xs = "a longer list."

-- list of tuples of two real floats
calcBims :: (RealFloat a) => [(a, a)] -> [a]
calcBims xs = [bmi w h | (w, h) <- xs]
    where bmi weight height = weight / height ^ 2

-- let binding
cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^ 2
    in sideArea + 2 * topArea
-- unsafe use let with list comprehensions
calcBims1 :: (RealFloat a) => [(a, a)] -> [a]
calcBims1 xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0]

-- case
head'1 :: [a] -> a
head'1 xs = case xs of [] -> error "No head for empty lists!"
                       (x:_) -> x
describeListCase :: [a] -> String
describeListCase xs = "This is " ++ case xs of [] -> "empty."
                                               [x] -> "a singleton list."
                                               xs -> "a longer list."