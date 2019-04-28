multThree :: Int -> (Int -> (Int -> Int))
multThree x y z = x * y * z

compareWithHundred :: Int -> Ordering
compareWithHundred x = compare 100 x

-- Here is another compareWithHundred method, which is EQUIVALENT to above!
-- partially applied compareWithHundred function
compareWithHundred' :: Int -> Ordering
compareWithHundred' = compare 100

-- partially applied infix function using sections
divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A' .. 'Z'])

-- practicing using higher-order functions
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> b -> a -> c
flip' f y x = f x y

-- The "Functional Programmer's Toolbox"
    -- 1.) MAP
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

    -- 2.) FILTER
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
    | p x       = x : filter' p xs
    | otherwise = filter' p xs

-- Different implementation of quicksort than Chapter 4; uses filter
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
    let smallerOrEqual = filter (<= x) xs
        larger = filter (> x) xs
    in quicksort smallerOrEqual ++ [x] ++ quicksort larger

-- more examples of map and filter:
    -- function to check largest number < 100000 divisible by 3829
largestDivisible :: Integer
largestDivisible = head (filter p [100000, 99999 ..])
    where p x = x `mod` 3829 == 0

-- takeWhile function is IMPORTANT

-- these two functions produce the same output; one uses list comprehensions,
-- the other uses filer and map
-- sum (takeWhile (<10000) (filter odd (map (^2) [1..])))
-- sum (takeWhile (<10000) [m | m <- [n^2 | n <- [1..]], odd m])

-- Collats Chain:
chain :: Integer -> [Integer]
chain 1 = [1]
chain n
    | even n = n:chain (n `div` 2)
    | odd n = n:chain (n*3 + 1)

-- Collats Chain problem: for all starting numbers between 1 and 100,
-- how many Collats chains have a length greater than 15?
numLongChains :: Int
numLongChains = length (filter isLong (map chain [1 .. 100]))
    where isLong xs = length xs > 15

-- numLongChains with lambda instead of where binding
numLongChains' :: Int
numLongChains' = length (filter (\xs -> length xs > 15) (map chain [1 .. 100]))

-- addThree using pattern matching
addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

-- addThree in currying notation
addThree' :: Int -> Int -> Int -> Int
addThree' = \x -> \y -> \z -> x + y + z

-- flip in currying notation. Note this one is much more readable in
-- currying notation than addThree, which is sort of a "gimmick" to show
-- that you can write that function in currying notation.
flip'' :: (a -> b -> c) -> b -> a -> c
flip'' f = \x y -> f y x

-- zipWith (flip'' (++)) ["love you", "love me"] ["i ", "you "]
-- map (flip'' subtract 20) [1,2,3,4]

-- we can use lambdas this way in our own functions when we want to make
-- it explicit that your functions are meant to be partially applied and
-- then passed on to other functions as a parameter


-- Folds
-- 		using a left fold (pattern matching)
sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

--      using a left fold (currying)
sum'' :: (Num a) => [a] -> a
sum'' = foldl (+) 0

-- CURRYING TIP: generally, if you have a function like foo a = bar b a,
-- you can rewrite it like foo = bar b because of currying

--		using a right fold to implement map
map'' :: (a -> b) -> [a] -> [b]
map'' f xs = foldr (\x acc -> f x : acc) [] xs

--		using a left fold to implement map
map''' :: (a -> b) -> [a] -> [b]
map''' f xs = foldl (\acc x -> acc ++ [f x]) [] xs

-- however, ++ is much slower than :, so usually use right folds
-- when building up new lists from a list

-- can use foldr on infinite lists, but not foldl
elem' :: (Eq a) => a -> ([a] -> Bool)
elem' y ys = foldr (\x acc -> if x == y then True else acc) False ys

-- foldr1 and foldl1 assume the accumulator to be the first value in the
-- list, and then start the fold with the element next to it
maximum' :: (Ord a) => [a] -> a
maximum' = foldl1 max

-- reverse using foldl
reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []

-- note that we can also write it like this:
reverse'' :: [a] -> [a]
reverse'' = foldl (flip (:)) []

-- product using foldl
product' :: (Num a) => [a] -> a
product' = foldl (*) 1

-- filter using foldr
filter'' :: (a -> Bool) -> [a] -> [a]
filter'' p = foldr (\x acc -> if p x then x : acc else acc) []

-- last using foldl1 - doesn't use accumulator, just sets it to new val every
-- time. once it reaches the last value, it returns that value
last' :: [a] -> a
last' = foldl1 (\_ x -> x)

-- and function using foldr to demonstrate how it works on infinite lists
and' :: [Bool] -> Bool
and' xs = foldr (&&) True xs

--		Scan - like folding, but it reports all the intermediate
--				accumulator states in the form of a list.
--			  - used to monitor the progress of a function that can be
--				implemented as a fold
--			  - includes scanl, scanr, scanl1, and scanr1

-- How mant elements does it take for the sum of the square roots of all
-- natural numbers to exceed 1,000?
sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1 ..]))) + 1


-- 		Function Application Operator ($)
-- 		- allows function of right to be used as paramter for another function
--		  because of its right associativity
--		- (convenience functions that lets us write fewer parantheses)
--				sqrt & 3 + 4 + 9 == sqrt (3 + 4 + 9)
--				sum $ filter (>10) $ map (*2) [2..10] == sum (filter (>10) (map (*2) [2..10]))

-- apart from getting rid of parantheses, $ lets us treat function application
-- like just another funciton. This allows us to, for instance, map function
-- application over a list of functions, like this:
-- map ($ 3) [(4+), (10*), (^2), sqrt]

-- 		Function Composition: (f o g)(x) = f(g(x))
--     map (\x -> negate (abs x)) [5,-3,-6,7,-3,2,-19,24]
--	== map (negate . abs) [5,-3,-6,7,-3,2,-19,24]

-- -function composition is right-associative, so we can compose many functions at a time
--   f (f (z x)) == (f . g . z)

--		map (\xs -> negate (sum (tail xs))) [[1..5],[3..6],[1..7]]
--	==  map (negate . sum . tail) [[1..5],[3..6],[1..7]]


--			Function Composition with Multiple Parameters
-- 		sum (replicate 5 (max 6.7 8.9))
-- 	 == (sum . replicate 5) (max 6.7 8.9)
--   == sum . replicate 5 $ max 6.7 8.9

-- To rewrite an expression with a lot of parantheses using function
-- composition, we can start by first writing out the innermost funciton
-- and its parameters. Then we put a $ before it and compose all the functions
-- that came before by writing them without their last parameter and putting
-- dots between them.

-- 		replicate 2 (product (map (*3) (zipWith max [1,2] [4,5]])))
-- 	 == replicate 2 . product . map (*3) $ zipWith max [1,2] [4,5]

--			Point-Free Style
fn x = ceiling (negate (tan (cos (max 50 x)))) -- non-point free style
fn' = ceiling . negate . tan . cos . max 50

-- now, we can rewrire our oddSquareSum function more concisely
oddSquareSum' :: Integer
oddSquareSum' = sum . takeWhile (<10000) . filter odd $ map (^2) [1..]

