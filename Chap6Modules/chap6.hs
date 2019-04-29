--			CHAPTER 6: MODULES
-- use Hoogle to search for functions and find out where they're located
-- import Data.List hiding (nub) (useful with dealing with name clashes)
-- import qualified Data.Map as M (another way to deal with name clashes; if
--		Data.Map and Prelude share a functions name, must call M.Func for Data.Map)
-- :m + Data.List Data.Map Data.Set
import Data.List
import Data.Char

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

