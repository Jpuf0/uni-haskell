{- Using guards, write a function sign that returns 1 for positive, -1 for negative and 0
for zero-valued arguments as shown in the table: -}
sign :: Int -> Int
sign a
  | a < 0 = -1
  | a == 0 = 0
  | otherwise = 1
{- Write a function howManyEqual that checks how many of its 3 arguments are equal. -}
howManyEquals :: Int -> Int -> Int -> Int 
howManyEquals x y z
  | x == y && y == z = 3
  | x == y || y == z || x == z = 2
  | otherwise = 0

{- Given a square and the length of one of its sides, the length of its diagonal can be
calculated using the following formula:

diagonalLength = sqrt(2 * side^2)

Write a function sumDiagonalLengths that takes the side length of three different
squares as its arguments. It should calculate the length of the each diagonal using the
above formula and return the sum of these values. -}
sumDiagonalLengths :: Float -> Float -> Float -> Float
sumDiagonalLengths a b c = diagonalLength a + diagonalLength b + diagonalLength c
  where
    diagonalLength side = sqrt(2 * side^2)

{- A taxi company calculates fares based on the distance travelled. Fares start at £2.20.
50p is added for each kilometre covered for the first 10 kilometres; and 30p is added
for each additional kilometre. Write a function taxiFare that takes the distance in
kilometres and returns the fare in pounds. -}
taxiFare :: Int -> Float
taxiFare a
  | a <= 10 = 2.2 + 0.5 * fromIntegral a
  | otherwise = 2.2 + 0.5 * 10 + 0.3 * fromIntegral (a - 10)

{- Write a function howManyAboveAverage that takes three integers as its arguments and
returns how many of them are above the average. -}
howManyAboveAverage :: Int -> Int -> Int -> Int
howManyAboveAverage a b c
  | a > average && b > average && c > average = 3
  | a > average && b > average || a > average && c > average || b > average && c > average = 2
  | otherwise = 0
  where
    average = (a + b + c) / 3

{- Write a function validDate that takes integers representing a day and a month. Then
it should return True if and only if the date is valid. -}
validDate :: Int -> Int -> Bool
validDate a b
  | b < 1 || b > 12 = False
  | b == 2 = a >= 1 && a <= 28
  | b == 4 || b == 6 || b == 9 || b == 11 = a >= 1 && a <= 30
  | otherwise = a >= 1 && a <= 31

{- Assuming that all years divisible by 4 are leap years (29 days in February), write a
function daysInMonth which returns the number of days in a given month and year. -}
daysInMonth :: Int -> Int -> Int
daysInMonth month year
  | month == 1 || month == 3 || month == 5 || month == 7 || month == 8 || month == 10 || month == 12 = 31
  | month == 4 || month == 6 || month == 9 || month == 11 = 30
  | month == 2 = if isLeapYear year then 29 else 28
  where
    isLeapYear y = y `mod` 4 == 0