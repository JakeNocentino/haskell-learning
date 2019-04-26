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