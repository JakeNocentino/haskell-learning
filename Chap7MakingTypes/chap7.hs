import qualified Data.Map as Map
import qualified Data.List as DataList

-- data Bool = True | False
-- data Int = -2147483648 | -2147483647 | ... | -1 | 0 | 1 | 2 | ... | 2147483647

-- this is type Shape with value constructors Circle and Rectangle
-- (also deriving the typeclass Shows)
data Shape' = Circle' Float Float Float | Rectangle' Float Float Float Float
    deriving (Show)

area' :: Shape' -> Float
area' (Circle' _ _ r) = pi * r^2
area' (Rectangle' x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)

-- value constructors are functions, so we can map them, partially apply them, and so on
-- ex.) map (Cirlce 10 20) [4,5,6,7]

-- we will now make our Shape data type better, using an intermediate data type Point
data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

area :: Shape -> Float
area (Circle _ r) = pi * r^2
area (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x + a) (y + b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b
    = Rectangle (Point (x1 + a) (y1 + b)) (Point (x2 + a) (y2 + b))

baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r

baseRect :: Float -> Float -> Shape
baseRect width height = Rectangle (Point 0 0) (Point width height)

-- we can also export our data types in our custom modules:
-- module Shapes
-- ( Point(..)     (..) means exporting all value constructors
-- , Shape(..)
-- , area
-- , nudge,
-- ,baseCircle
-- , baseRect
-- ) where

-- Not exporting the value constructors of our data types makes them more abstract, since we're hiding their implementation
-- Data.Map uses this approach

-- Record Syntax
-- first name, last name, age, height, phone #, favorite ice cream flavor
data Person' = Person' String String Int Float String String deriving (Show)

firstName' :: Person' -> String
firstName' (Person' firstName' _ _ _ _ _) = firstName'

lastName' :: Person' -> String
lastName' (Person' _ lastName' _ _ _ _) = lastName'

age' :: Person' -> Int
age' (Person' _ _ age' _ _ _) = age'

height' :: Person' -> Float
height' (Person' _ _ _ height' _ _) = height'

phoneNumber' :: Person' -> String
phoneNumber' (Person' _ _ _ _ phoneNumber' _) = phoneNumber'

flavor' :: Person' -> String
flavor' (Person' _ _ _ _ _ flavor') = flavor'

-- Hold up... there is a MUCH BETTER way to write all of this!
-- Uee "record syntax" to clean up the above code

-- data type Person
data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     , height :: Float
                     , phoneNumber :: String
                     , flavor :: String } deriving (Show)

-- data type Car
-- note that this data type is not worth paranetizing, because it will almost always be a String String Int and nothing more
data Car = Car { company :: String
               , model :: String
               , year :: Int
               } deriving (Show)

tellCar :: Car -> String
tellCar (Car {company = c, model = m, year = y}) = 
    "This " ++ c ++ " " ++ m ++ " was made in " ++ show y
-- when using record syntax, you can do this:
-- Car {company = "Ford", model = "Mustang", year = 1967}
-- and the fields don't have to be in order

-- Type Parameters and Type Constructors
data IntMaybe = INothing | IJust Int
data StringMaybe = SNothing | SJust String
data ShapeMaybe = ShNothing | ShJust Shape

-- type parametizing is useful when having a container of stuff and not caring what that stuff is (like tempaltes in C++)

-- Haskell convention - NEVER add type class constraints in data declarations

-- implementing a 3D Vector type - parameterized type bc it can support Int, Integer, Double, etc
data Vector a = Vector a a a deriving (Show)

vplus :: (Num a) => Vector a -> Vector a -> Vector a
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)

dotProd :: (Num a) => Vector a -> Vector a -> a
(Vector i j k) `dotProd` (Vector l m n) = i*l + j*m + k*n

vmult :: (Num a) => Vector a -> a -> Vector a
(Vector i j k) `vmult` m = Vector (i*m) (j*m) (k*m)

-- Deriving Instances
data Athlete = Athlete { firstNameAth :: String
                       , lastNameAth :: String
                       , ageAth :: Int
                       } deriving (Eq, Show, Read)

mj = Athlete {firstNameAth = "Michael", lastNameAth = "Jordan", ageAth = 48}
lbj = Athlete {firstNameAth = "LeBron", lastNameAth = "James", ageAth = 34}
lm = Athlete {firstNameAth = "Lionell", lastNameAth = "Messi", ageAth = 35}

-- can read this athelete using "read br :: Athlete" to explicitly infer its Athlete type
br = "Athlete { firstNameAth = \"Babe\"" ++
              ", lastNameAth = \"Ruth\"" ++
              ", ageAth = 85}"

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
    deriving (Eq, Ord, Show, Read, Bounded, Enum)

-- Type Synonyms
-- type String = [Char]
-- the 'type' keyword defines a synonym for ane existing type, which is created using the keyword 'data'
-- toUpperString :: [Char] -> [Char] == toUpperString :: String -> String

-- Going back to our phonebook, we can use type synonyms to convey the information in an easier way to read:
type PhoneNumber = String
type Name = String
type PhoneBook = [(Name, PhoneNumber)]

phoneBook :: [(Name, PhoneNumber)]
phoneBook =
    [("betty", "555-2938")
    ,("bonnie", "452-2928")
    ,("patsy", "493-2928")
    ,("lucille", "205-2928")
    ,("wendy", "939-8282")
    ,("penny", "853-2492")
     ]

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnumber pbook = (name, pnumber) `elem` pbook

-- Parameterizing Type Synonyms
type AssocList k v = [(k, v)]

-- Partially applied type parameters
-- the IntMap type constructor takes one parameter, and that is the type of what the integers will point to
type IntMap v = Map.Map Int v
type IntMap' = Map.Map Int

-- another cool data type example
data Either' a b = Left' a | Right' b deriving (Eq, Ord, Read, Show)

-- The result type of Either a b is usually used to tell us why some function failed.
-- Left a can tell us something about the possible failure, and Right b is the successful computation type.
-- as an example:

data LockerState = Taken | Free deriving (Show, Eq)
type Code = String
type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map = case Map.lookup lockerNumber map of
    Nothing -> Left $ "Locker " ++ show lockerNumber ++ " doesn't exist!"
    Just (state, code) -> if state /= Taken
      then Right code
      else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"

lockers :: LockerMap
lockers = Map.fromList
    [(100,(Taken, "ZD39I"))
     ,(101,(Free, "JAH3I"))
     ,(103,(Free, "IQSA9"))
     ,(105,(Free, "QOTSA"))
     ,(109,(Taken, "893JJ"))
     ,(110,(Taken, "99292"))
     ]

-- Recursive Data Structures
data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)
-- Cons is another word for :
-- In lists, : is actually a constructor that takes a value and another list and returns a list
-- ghci> Empty
-- Empty
-- ghci> 5 `Cons` Empty
-- Cons 5 Empty
-- ghci> 4 `Cons` (5 `Cons` Empty)
-- Cons 4 (Cons 5 Empty)
-- ghci> 3 `Cons` (4 `Cons` (5 `Cons` Empty))
-- Cons 3 (Cons 4 (Cons 5 Empty))
-- We called our Cons constructor in an infix manner so you can see how it’s just like :. Empty is like [], and 4 `Cons` (5 `Cons` Empty) is like 4:(5:[]).

-- Improving Our List
infixr 5 :-:
data List' a = Empty' | a :-: (List' a) deriving (Show, Read, Eq, Ord)
-- infixr = infix operation with right assocation, 5 = fixity value

-- Let's make a function that adds two of our lists together (how ++ is defined for normal lists)
infixr 5 ^++
(^++) :: List' a -> List' a -> List' a
Empty' ^++ ys = ys
(x :-: xs) ^++ ys = x :-: (xs ^++ ys)

{- Now let's implement a binary search tree to get a better feel for
 recursive data structures in Haskell! 

  *Cool Tidbit: Set and Maps from Data.Set and Data.Map are implemented using trees
-}
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)

-- some functions for building our Binary Search Tree

-- utility function for returning an empty tree
singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

-- inserting an element in a tree
treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
  | x == a = Node x left right
  | x < a  = Node a (treeInsert x left) right
  | x > a  = Node a left (treeInsert x right)

-- searching for an element in a Binary Search Tree
treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
  | x == a = True
  | x < a  = treeElem x left
  | x > a  = treeElem x right

{- Now, we'll have some fun with our tree using foldr! -}
-- let nums = [8,6,4,1,7,3,5]
-- let numsTree = foldr treeInsert EmptyTree nums
-- 8 `treeElem` numsTree


-- TYPE CLASSES 102
-- defining the 'Eq' type classes:
{- class Eq' a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
  x == y = not (x /= y)
  x /= y = not (x == y) -}


-- writing instances of type classes by hand (instead of deriving) for nice funcitonality
data TrafficLight = Red | Yellow | Green

-- satisfies minimal complete definition for Eq!
instance Eq TrafficLight where
  Red == Red = True
  Green == Green = True
  Yellow == Yellow = True
  _ == _ = False

-- satisfies the minimal complete definition for Show!
instance Show TrafficLight where
  show Red = "Red Light"
  show Yellow = "Yellow Light"
  show Green = "Green Light"

-- how Haskell derives the isntance of Maybe (using class constraints on Maybe's contents)
{- instance (Eq m) => Eq (Maybe m) where
    Just x == Just y = x == y
    Nothing == Nothing = True
    _ == _ = False
-}


--  A YES-NO TYPE CLASS: implementing JavaScript-esque boolean function evaluation
class YesNo a where
  yesno :: a -> Bool

-- 0 evaluates to false, any other integer to true
instance YesNo Int where
  yesno 0 = False
  yesno _ = True

 -- empty lists evaluate to false, any other size list to true
instance YesNo [a] where
  yesno [] = False
  yesno _  = True

-- bool itself also holds truthness... but what is id?? It's just a standard library function that
-- takes a parameter and returns the same thing, which is what we would write here anyways.
instance YesNo Bool where
  yesno = id

-- let's make "Maybe a" an instance of YesNo as well. Note no class constraint because we don't
-- case about Maybe's contents; just if it's "Just" or "Nothing." Note that we still had to write
-- out "Maybe a."
instance YesNo (Maybe a) where
  yesno (Just _) = True
  yesno Nothing  = False

-- let's make our Tree type an instance of YesNo
instance YesNo (Tree a) where
  yesno EmptyTree = False
  yesno _ = True

-- and also our TrafficLight type!
instance YesNo TrafficLight where
  yesno Red = False
  yesno _ = True

-- now play around with it!

-- mimicking JavaScripts "if-statements" with our YesNo typeclass
yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesnoVal yesResult noResult =
  if yesno yesnoVal
    then yesResult
    else noResult


-- THE FUNCTOR TYPE CLASS: see book for clear descriptions

-- making our Tree type an instance of Functor (because it's a type constructor with 
-- one type parameter)
instance Functor Tree where
  fmap f EmptyTree = EmptyTree
  fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)

class MyFunctor f where
  fmap' :: (a -> b) -> f a -> f b

instance (Ord k) => MyFunctor (Map.Map k) where
  fmap' = Map.map
