--			CHAPTER 6: MODULES
-- use Hoogle to search for functions and find out where they're located
-- import Data.List hiding (nub) (useful with dealing with name clashes)
-- import qualified Data.Map as M (another way to deal with name clashes; if
--		Data.Map and Prelude share a functions name, must call M.Func for Data.Map)
-- :m + Data.List Data.Map Data.Set
import Data.List (nub, sort)

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub