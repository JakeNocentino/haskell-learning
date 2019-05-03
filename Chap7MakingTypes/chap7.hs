-- data Bool = True | False
-- data Int = -2147483648 | -2147483647 | ... | -1 | 0 | 1 | 2 | ... | 2147483647

-- this is type Shape with value constructors Circle and Rectangle
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

-- Type Parameters and Typr Constructors
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



