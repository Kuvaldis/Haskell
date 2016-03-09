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

-- maximum recursive
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "Empty list"
maximum' [x] = x
maximum' (x:xs)
    | x > tailMaximum = x
    | otherwise = tailMaximum
    where tailMaximum = maximum' xs

-- replicate recursive
replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
    | n <= 0    = []
    | otherwise = x : replicate' (n - 1) x

-- take recursive
take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0   = []
take' _ []     = []
take' n (x : xs) = x : take' (n - 1) xs

-- reverse recursive
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

-- zip recursive
zip' :: [a] -> [a] -> [(a, a)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

-- elem recursive
elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
    | a == x    = True
    | otherwise = elem' a xs

-- quick sort
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smaller  = quicksort [a | a <- xs, a <= x]
        bigger   = quicksort [a | a <- xs, a > x]
    in smaller ++ [x] ++ bigger

-- using sections in parentheses in infix functions. no need to supply param
divideByTen :: (Floating a) => a -> a
divideByTen = ( / 10)

-- higher order function
multThree :: (Num a) => a -> a -> a -> a
multThree a b c = a * b * c
multTwoWithNine = multThree 9
multWithEighteen = multTwoWithNine 2

-- passing function to another
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)
-- applyTwice (multThree 2 3) 2 == 72

-- zip with a function
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ _ [] = []
zipWith' _ [] _ = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys
-- zipWith' (+) [1, 20] [3, 1] == [4, 21]
-- zipWith' (*) (replicate' 5 2) [1..] == [2,4,6,8,10]

-- flip params
flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
    where g x y = f y x
-- (flip' (/)) 1 2 == 2.0

-- filter
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
    | p x       = x : filter' p xs
    | otherwise = filter' p xs

-- Collatz sequence
chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
    | odd n = n:chain (n * 3 + 1)
    | even n = n:chain (n `div` 2)
numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
    where isLong xs =
