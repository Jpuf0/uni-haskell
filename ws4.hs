import Data.Char

type StudentMark = (String, Int)

betterStudent :: StudentMark -> StudentMark -> String
betterStudent (s1,m1) (s2,m2) 
    | m1 >= m2          = s1
    | otherwise         = s2

marks:: [StudentMark] -> [Int]
marks stmks = [ mk | (st,mk) <- stmks ]

pass :: [StudentMark] -> [String]
pass stmks = [ st | (st,mk) <- stmks, mk >= 40 ]

-- An example list of student marks
testData :: [StudentMark]
testData = [("John", 53), ("Sam", 16), ("Kate", 85), ("Jill", 65),
            ("Bill", 37), ("Amy", 22), ("Jack", 41), ("Sue", 71)]

addPairs :: [(Int,Int)] -> [Int]
addPairs pairList = [ i+j | (i,j) <- pairList ]

minAndMax :: Int -> Int -> (Int,Int)
minAndMax x y 
    | x <= y            = (x,y)
    | otherwise         = (y,x)

{- Write a function sumDifference that returns the sum and difference of two numbers
as a tuple. -}
sumDifference :: Int -> Int -> (Int, Int)
sumDifference x y = (x + y, x - y)

{-Students are given ‘A’ for 70% or higher, ‘B’ for 60+, ‘C’ for 50+, ‘D’ for 40+, and ‘F’
for everything else. Write a function that grades a given StudentMark-}
grade :: StudentMark -> Char
grade (name, mark)
    | mark > 100 || mark < 0 = error "Mark must be between 0 and 100"
    | mark >= 70 = 'A'
    | mark >= 60 = 'B'
    | mark >= 50 = 'C'
    | mark >= 40 = 'D'
    | otherwise = 'F'

{- The mark for late assignments or exam retakes is capped at 40%. Write a function
with the following signature that caps a student’s mark (see the test cases) -}
capMark :: StudentMark -> StudentMark
capMark (name, mark)
    | mark > 100 || mark < 0 = error "Mark must be between 0 and 100"
    | mark >= 40 = (name, 40)
    | otherwise = (name, mark)

{- Write firstNumbers which returns a list of integers from 1 to the given argument. -}
firstNumbers :: Int -> [Int]
firstNumbers n = [ i | i <- [1..n] ]

{- Using firstNumbers write firstSquares that returns the list of first n squares given a
positive integer n. -}
firstSquares :: Int -> [Int]
firstSquares n = [ i * i | i <- firstNumbers n ]

{- Using a list comprehension, write a function that capitalizes a given string. -}
capitalize :: String -> String
capitalize s = [ toUpper c | c <- s ]

{- Using a list comprehension, write a function onlyDigits that only keeps the numerical
characters in a string. -}
onlyDigits :: String -> String
onlyDigits s = [ c | c <- s, isDigit c ]

{- Using your capMark function and a list comprehension, write a function that caps the
mark of each student on a given list of StudentMarks. -}
capMarks :: [StudentMark] -> [StudentMark]
capMarks marks = [ capMark mark | mark <- marks ]

{- Using your grade function and a list comprehension, write a function that grades every
student on a given list of StudentMarks. -}
gradeStudents :: [StudentMark] -> [(String, Char)]
gradeStudents marks = [ (name, grade (name, mark)) | (name, mark) <- marks ]

{- Write a function duplicate that repeats a string a given number of times -}
-- Guard Solution
duplicate_G :: String -> Int -> String
duplicate_G str n
  | n <= 0 = ""
  | n == 1 = str
  | otherwise = str ++ duplicate_G str (n - 1)

-- List Comprehension Solution
duplicate_L :: String -> Int -> String
duplicate_L str n = concat [str | _ <- [1..n]]

{- Using a list comprehension, write a function that lists all the divisors of a number. -}
divisors :: Int -> [Int]
divisors n = [ i | i <- [1..n], n `mod` i == 0 ]

{- Using your divisors function, write a function that checks whether a number is prime. -}
isPrime :: Int -> Bool
isPrime n
  | n <= 1 = False
  | otherwise = divisors n == [1,n]

{- Using list comprehensions, write a polymorphic function (i.e., a function that works
for any type a and b) that makes a list of pairs into a pair of lists. -}
split :: [(a,b)] -> ([a],[b])
split pairs = (fstset, sndset)
  where
    fstset = [ fst pair | pair <- pairs ]
    sndset = [ snd pair | pair <- pairs ]