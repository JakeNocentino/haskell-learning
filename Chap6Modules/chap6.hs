--			CHAPTER 6: MODULES
-- use Hoogle to search for functions and find out where they're located
-- import Data.List hiding (nub) (useful with dealing with name clashes)
-- import qualified Data.Map as M (another way to deal with name clashes; if
--		Data.Map and Prelude share a functions name, must call M.Func for Data.Map)
-- :m + Data.List Data.Map Data.Set
import Data.List
import Data.Char
import qualified Data.Map as Map
import qualified Geometry.Sphere as Sphere
import qualified Geometry.Cuboid as Cuboid
import qualified Geometry.Cube as Cube

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

wordNums :: String -> [(String, Int)]
wordNums = map (\ws -> (head ws, length ws)) . group . sort . words

-- same as "isInfixOf" already defined in Data.List
isIn :: (Eq a) => [a] -> [a] -> Bool
needle `isIn` haystack = any (needle `isPrefixOf`) (tails haystack)

-- Data.Char - contains useful functions for shifting Chars
-- ord and chr - convert chars to their correspond unicode and vice versa

encode :: Int -> String -> String
encode offset msg = map (\c -> chr $ ord c + offset) msg

-- same encode function, using function composition
encode' :: Int -> String -> String
encode' offset msg = map (chr . (+ offset) . ord) msg

decode :: Int -> String -> String
decode shift msg = map (\c -> chr $ ord c - shift) msg
-- OR, this BETTER AND MORE CONCISE VERSION
decode' :: Int -> String -> String
decode' shift msg = encode (negate shift) msg

-- folds, while powerful, can cause a stack overflow error
-- here is how Haskell evaluates "foldl (+) 0 [1,2,3]", which demonstrates Haskell's laziness
-- foldl (+) 0 [1,2,3] = 
-- foldl (+) (0 + 1) [2,3] =
-- foldl (+) ((0 + 1) + 2) [3] =
-- foldl (+) (((0 + 1) + 2) + 3) [] =
-- ((0 + 1) + 2) + 3  =
-- (1 + 2) + 3 =
-- 3 + 3 =
-- 6

-- strict version of foldl, foldl', is in Data.List module. Doesn't defer computation, so stack overflow error less likely to occur

-- Data.Char has function "digitToInt"

-- finding sum of numbers digits
digitSum :: Int -> Int
digitSum = sum . map digitToInt . show
-- SAME AS: "digitSum n = sum . map digitToInt $ show n"

-- Maybe - a value that holds 0 elements or 1 element
-- Nothing - a value that holds nothing
-- Just - a value that holds something

firstTo40 :: Maybe Int
firstTo40 = find (\x -> digitSum x == 40) [1 ..]

firstTo :: Int -> Maybe Int
firstTo n = find (\x -> digitSum x == n) [1 ..]

-- Association Lists (Dictionaries)
phoneBook = 
    [("betty", "555-2938")
    ,("betty", "342-2492")
    ,("bonnie", "452-2928")
    ,("patsy", "493-2928")
    ,("patsy", "943-2929")
    ,("patsy", "827-9162")
    ,("lucille", "205-2921")
    ,("wendy", "939-8282")
    ,("penny", "853-2492")
    ,("penny", "555-2111")
    ]

findKey :: (Eq k) => k -> [(k, v)] -> v
findKey key xs = snd . head . filter (\(k, v) -> key == k) $ xs
-- but this program will crash if the desired key is not in the list.
-- Hence, we change the type return to Maybe v, and return Nothing or Just v
findKey' :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey' key [] = Nothing
findKey' key ((k, v):xs)
    | key == k  = Just v
    | otherwise = findKey' key xs
-- We can also (should also) use a fold for this type of explicit recursion
findKey'' :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey'' key xs = foldr (\(k, v) acc -> if key == k then Just v else acc) Nothing xs

-- It turns out that findKey = lookup (from Data.List)
-- Data.Map offers many better functions for this type of work, however
-- Map.fromList turns an association list into a map
-- whenever using maps, always use functions from Data.Map (lookUp, insert, etc)

-- New Map of previous phoneBook associated list
phoneBook' :: Map.Map String String
phoneBook' = Map.fromList $
    [("betty", "555-2938")
    ,("bonnie", "452-2928")
    ,("patsy", "493-2928")
    ,("lucille", "205-2921")
    ,("wendy", "939-8282")
    ,("penny", "853-2492")
    ]

-- Map.lookup "betty" phoneBook'
-- let newBook = Map.insert "grace" "341-9021" phoneBook'
-- Map.size phonebook'

-- used to convert our string of ints at the v to a list of ints
string2digits :: String -> [Int]
string2digits = map digitToInt . filter isDigit

-- fromListWith works like fromWith, but instead of discaring duplicate keys, it uses a funciton supplied to it to decide what to do with them

phoneBookToMap :: (Ord k) => [(k, String)] -> Map.Map k String
phoneBookToMap xs = Map.fromListWith add xs
    where add number1 number2 = number1 ++ ", " ++ number2

phoneBookToMap' :: (Ord k) => [(k, a)] -> Map.Map k [a]
phoneBookToMap' xs = Map.fromListWith (++) $ map (\(k, v) -> (k, [v])) xs


