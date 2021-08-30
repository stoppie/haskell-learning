-- not - negation, not True == False
-- /= inequality symbol
-- div 92 10 == 92 `div` 10 integral division
-- function names can't begin with capitalized letters
-- ++ lists/string concatenation
-- 1:[2, 3, 4] == [1, 2, 3, 4] and it's faster than ++
-- indexing [1, 2, 5, 4] !! 3 == 5
-- >=, <=, <, >, == are lexicographical for lists
-- list functions: head (first element), tail (without first element)
--                 last, init (without last element)
--                 null (check if list is empty), reverse
--                 take num list (takes first num elements from list)
--                 drop num list (drop first num elements from list)
--                 obj `elem` list (checks if obj is inside list)
-- [1..n] == [1, 2, ..., n], ['a'..'d'] == ['a', 'b', 'c', 'd']
-- [2, 4..20] == [2, 4, 6, .., 20], [20, 19..1] == [20, 19, 18, ..., 1]
-- cycle [1,2,3] == [1, 2, 3, 1, 2, 3, ...]
-- repeat 5 == [5, 5, 5, ...]
-- replicate 3 10 == [10, 10, 10]
-- list comprehension: [x*2 | x <- [1..10]] == [2, 4, 6, .., 20]
--                     [x*2 | x <- [1..10], x `mod` 7 == 2, odd x] == [4, 18]
--                     [x*y | x <- [2,5], y <- [8,10]] == [16, 20, 40, 50]
-- fst (2, 5) == 2
-- snd (5, "Hello") == "Hello"
-- zip [1, 2, 3] [1] == [(1, 1), (2, 1), (3, 1)]

doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100
    then x
    else x*2

doubleSmallNumber' x = (if x > 100 then x else x*2) + 1

conanO'Brien = "It's a-me, Conan O'Brien!"

boomBangs xs = [if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

length' xs = sum [1 | _ <- xs]

removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

addVectors :: Num a => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

head' :: [a] -> a
head' [] = error "Can't call head on an empty list, dummy!"
head' (x:_) = x

-- head'' :: [a] -> a  
-- head'' xs = case xs of [] -> error "No head for empty lists!"  
--                       (x:_) -> x

capital :: String -> String
capital "" = "Empty string, whoops!"
capital all@(x:xs) = "The first letter of " ++ all ++ " is" ++ [x]

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height  
    | bmi <= skinny = "You're underweight, you emo, you!"  
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= fat    = "You're fat! Lose some weight, fatty!"  
    | otherwise     = "You're a whale, congratulations!"  
    where bmi = weight / height ^ 2  
          skinny = 18.5  
          normal = 25.0  
          fat = 30.0

initials :: String -> String -> String  
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."  
    where (f:_) = firstname  
          (l:_) = lastname

calcBmis :: (RealFloat a) => [(a, a)] -> [a]  
calcBmis xs = [bmi w h | (w, h) <- xs]  
    where bmi weight height = weight / height ^ 2

describeList :: [a] -> String  
describeList xs = "The list is " ++ what xs  
    where what [] = "empty."  
          what [x] = "a singleton list."  
          what xs = "a longer list."

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs)
    | x > maxTail = x
    | otherwise = maxTail
    where maxTail = maximum' xs

replicate' :: Int -> a -> [a]
replicate' n x
    | n <= 0 = []
    | otherwise = x:replicate' (n - 1) x

take' :: Int -> [a] -> [a]
take' n _
    | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x:take' (n - 1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

zip' :: [a] -> [b] -> [(a, b)]
zip' x y
    | null x || null y = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) =   
    let smallerSorted = quicksort [a | a <- xs, a <= x]  
        biggerSorted = quicksort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted