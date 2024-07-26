import Prelude hiding (fst, snd, head, tail, sum, concat, reverse, zip)
import GHC.Base (BCO)
import Data.List (subsequences)

type StudentMark = (String, Int)

testData :: [StudentMark]
testData =
    [ 
     ("John", 53),
     ("Sam", 16),
     ("Kate", 85),
     ("Jill", 65),
     ("Bill", 37),
     ("Amy", 22),
     ("Jack", 41),
     ("Sue", 71)
    ]

fst (x,_)       = x
snd (_,y)       = y

head (x:_)      = x
tail (_:xs)     = xs

absFirst :: [Int] -> Int
absFirst []     = -1
absFirst (x:xs) = abs x

sum :: [Int] -> Int 
sum []     = 0
sum (x:xs) =   x + sum xs

doubleAll :: [Int] -> [Int]
doubleAll []      = []
doubleAll (x:xs)  = 2*x : doubleAll xs

concat :: [[a]] -> [a]
concat []         = []
concat (x:xs)     = x ++ concat xs

reverse :: [a] -> [a]
reverse []      = []
reverse (x:xs)  = reverse xs ++ [x]

zip :: [a] -> [b] -> [(a,b)]
zip (x:xs) (y:ys)  = (x,y) : zip xs ys
zip _ _            = []

headPlusOne :: [Int] -> Int
headPlusOne [] = -1
headPlusOne (x:xs) = x + 1

duplicateHead :: [a] -> [a]
duplicateHead [] = []
duplicateHead (x:xs) = x:(x:xs)

rotate :: [a] -> [a]
rotate (x:xs) = head xs : x : tail xs

listLength :: [a] -> Int
listLength [] = 0
listLength (_:xs) = 1 + listLength xs

multAll :: [Int] -> Int
multAll [] = 1
multAll (x:xs)  = x * multAll xs

andAll :: [Bool] -> Bool
andAll [] = True
andAll (x:xs) = x && andAll xs

orAll :: [Bool] -> Bool
orAll [] = True
orAll (x:xs) = x || orAll xs

countIntegers :: Int -> [Int] -> Int
countIntegers _ [] = 0
countIntegers number (x:xs) 
    | number == x = 1 + countIntegers number xs
    | otherwise = countIntegers number xs

removeAll :: Int -> [Int] -> [Int]
removeAll _ [] = []
removeAll number (x:xs) 
    | number == x = removeAll number xs
    | otherwise = x : removeAll number xs

removeAllButFirst :: Int -> [Int] -> [Int]
removeAllButFirst _ [] = []
removeAllButFirst number (x:xs)
    | number == x = x : removeAll number xs
    | otherwise = x : removeAllButFirst number xs

listMarks :: String -> [StudentMark] -> [Int]
listMarks _ [] = []
listMarks student (x:xs)
    | student == fst x = snd x : listMarks student xs
    | otherwise = listMarks student xs

sorted :: [Int] -> Bool 
sorted [] = True
sorted [_] = True
sorted (x:xs)
    | x <= head xs = sorted xs
    | otherwise = False

prefix :: [Int] -> [Int] -> Bool 
prefix [] _ = True 
prefix _ [] = False
prefix (x:xs) (y:ys)
    | x == y = prefix xs ys 
    | otherwise = False

subSequence :: [Int] -> [Int] -> Bool
subSequence [] _ = True 
subSequence _ [] = False
subSequence (x:xs) (y:ys)
    | x == y = prefix (x:xs) (y:ys) 
    | otherwise = subSequence xs ys 
