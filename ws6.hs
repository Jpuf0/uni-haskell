{- Week6.hs
 This module illustrates the use of functions as values
-}

import Data.Char

twice :: (Int -> Int) -> Int -> Int
twice f x = f (f x)

multiply :: Int -> Int -> Int
multiply x y = x * y

double :: Int -> Int
double = multiply 2

doubleAll :: [Int] -> [Int]
doubleAll = map (*2)

areDigits :: String -> [Bool]
areDigits = map isDigit

keepPositive :: [Int] -> [Int]
keepPositive = filter (>0)

keepDigits :: String -> String
keepDigits = filter isDigit

addUp :: [Int] -> Int
addUp = foldr (+) 0 

myConcat :: [[a]] -> [a]
myConcat = foldr (++) []

{- Write a function mult10 that multiplies every element of a list by 10. -}
mult10 :: [Int] -> [Int]
mult10 numbers = map (*10) numbers 

-- map applies a function to every element of the list

{- Write a function onlyLowerCase that uses the isLower function to remove any charac-
ter from a string that is not a lowercase letter. -}
onlyLowerCase :: String -> String
onlyLowerCase str = filter isLower str 

--Filter takes a preidcate and a list and returns a new list of values where the preidcate is true


{- Write a one-line definition for the orAll function from last week. See the definition
for andAll in the first worked example. -}
orAll :: [Bool] -> Bool
orAll = foldr (||) False

-- foldr takes a list a fold function, and an initial value and returns the accumulated value

{- Write a one-line definition for sumSquares (from worksheet 3). -}
sumSquares :: [Int] -> Int
sumSquares = foldr (+) 0 . map (^2)

{- Write a function zeroToTen that takes a list of integers and only keeps those that are
between 0 and 10 (inclusive) -}
zeroToTen :: [Int] -> [Int]
zeroToTen = filter (>= 0) . filter (<= 0)

{- Write a function squareRoots with the signature below. squareRoots should return a
list of the square roots of all the non-negative values in the given list -}
squareRoots :: [Float] -> [Float]
-- squareRoots xs = map sqrt (filter (>= 0) xs) 
squareRoots =  map sqrt . filter (>= 0) 

{- Write a function countBetween with the signature below. The function counts the
number of items in a list that are between specified lower- and upper–bounds values
(those equal to either bound should be counted). -}
countBetween :: Float -> Float -> [Float] -> Int
countBetween lower upper numbers = length (filter (\x -> x >= lower && x <= upper) numbers) 

{- Write a function alwaysPositive that tests whether applying a given function to all
the elements of a list results only in positive values. -}

-- Version 1
{- 
alwaysPositive1 :: (Float -> Float) -> [Float] -> Bool
alwaysPositive1 _ [] = True
alwaysPositive1 f (x:xs) = f x > 0 && alwaysPositive1 f xs
-}

-- Version 2
{-
alwaysPositive2 :: (Float -> Float) -> [Float] -> Bool
alwaysPositive2 f xs = all (\x -> f x > 0) xs
-}

-- Version 3
{- 
alwaysPositive3 :: (Float -> Float) -> [Float] -> Bool
alwaysPositive3 f xs = null [x | x <- xs, f x <= 0]
-}
alwaysPositive :: (Float -> Float) -> [Float] -> Bool
-- alwaysPositive f numbers = length (filter (\x -> f x > 0) numbers) == length numbers
alwaysPositive f numbers  = length (filter (> 0) (map f(numbers))) == length numbers

{- Write a function productSquareRoots that returns the product of the square roots of
all non-negative numbers in the given list: -}
productSquareRoots :: [Float] -> Float
productSquareRoots xs = foldr (*) 1 (map sqrt (filter (>= 0) xs))

{- Write a recursive polymorphic function removeFirst that removes the first element of
a list that has a certain property. -}
removeFirst :: (a -> Bool) -> [a] -> [a]
removeFirst _ [] = []
removeFirst p (x:xs)
    | p x = xs
    | otherwise = x : removeFirst p xs

{- Using yourremoveFirst function and and other function from the Prelude that you
find useful, write a one-line removeLast function that removes the last element of a
list that has a certain property. -}
removeLast :: (a -> Bool) -> [a] -> [a]
removeLast p xs = reverse $ removeFirst p (reverse xs)

{- Using filter and a lambda expression, write an alternative solution to exercise 5 -}
zeroToTen2 :: [Int] -> [Int]
zeroToTen2 numbers = filter (\x -> x >= 0 && x <= 10) numbers   
