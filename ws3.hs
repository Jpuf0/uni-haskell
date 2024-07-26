-- We don't import '||' from the prelude, so that we can
-- define our own version

import Prelude hiding ((||), (&&), gcd)

-- The following line declares the || operator (which we are about to
-- re-define) to be right associative and to have precedence 2. This
-- is necessary in order for expressions such as False || x > 2 to be
-- valid (e.g. it sets the precedence of || to be lower than >).

infixr 2 ||
infixr 3 &&

-- A naive re-implementation of the Prelude operator ||
(||) :: Bool -> Bool -> Bool
True || True = True
False || True = True
True || False = True
False || False = False


{-Write three definitions of the && operator (and) using pattern matching. See the reimplementations of the || operator (or) for an example.
Hide the default definition of && by updating your import statement as shown below:
import Prelude hiding ((||), (&&))
Also include the fixity declaration for this function by adding the following line which
forces complex Boolean expressions to be correctly interpreted:
infixr 3 &&
Make sure you have three definitions for || are similar to the definitions given for &&.-}
(&&) :: Bool -> Bool -> Bool
True && True = True
False && True = False
True && False = False
False && False = False

-- An alternative re-implementation
--(||) :: Bool -> Bool -> Bool
--False || False   = False
--_ || _           = True

-- Another alternative re-implementation
--(||) :: Bool -> Bool -> Bool
--True || _     =  True
--False || a    = a

fact :: Int -> Int
fact n
  | n == 0 = 1
  | n > 0 = n * fact (n - 1)
  | otherwise = error "factorials not defined for negative ints"

mult :: Int -> Int -> Int
mult n m
  | n == 0 = 0
  | n > 0 = m + mult (n - 1) m
  | otherwise = - mult (- n) m

divide :: Int -> Int -> Int
divide n m
  | n < m = 0
  | otherwise = 1 + divide (n - m) m

fibonacci :: Int -> Int
-- fibonacci n
-- | n == 0 = 0
-- | n == 1 = 1
-- | otherwise = fibonacci (n - 1) + fibonacci (n - 2)
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

{- nor :: Bool -> Bool -> Bool
nor False False = True
nor False True = False
nor True False = False
nor True True = False -}

nor :: Bool -> Bool -> Bool
nor False x = not x
nor True _ = False

{-Using pattern matching, define the exOr function (XOR gate). Verify it using the truth
table provided.-}
exOr :: Bool -> Bool -> Bool
exOr False False = False
exOr False True = True
exOr True False = True
exOr True True = False

{-. Using pattern matching, define an ifThenElse function that takes three arguments. If
the first argument evaluates to True, ifThenElse should return the second argument,
otherwise it should return the third (see the table).-}
ifThenElse :: Bool -> Int -> Int -> Int
ifThenElse True b c = b
ifThenElse a b c = c

{-Use pattern matching to write a function daysInMonth that takes an Int (assumed to
be between 1 and 12) and returns the number of days in the corresponding month.
Your goal is to make your solution as short as possible, but do not use concepts that
are not covered yet (e.g., lists).-}
daysInMonth :: Int -> Int
daysInMonth 2 = 28
daysInMonth 4 = 30
daysInMonth 6 = 30
daysInMonth 9 = 30
daysInMonth 11 = 30
daysInMonth _ = 31

{- Once you have written and tested your daysInMonth function, write a new (simpler)
version of validDate from the previous worksheet (guards or patterns should not be
needed). -}
validDate :: Int -> Int -> Bool
validDate day month = month >= 1 && month <= 12 && day >= 1 && day <= daysInMonth month

{-Write a recursive function sumNumbers that takes a non-negative number n (as Int)
and returns the sum of all integers from 1 to n.-}
sumNumbers :: Int -> Int 
sumNumbers 0 = 0
sumNumbers n = n + sumNumbers (n - 1)

{- Write a recursive function sumSquares that takes a number n and returns the sum of
the squared numbers from 1 to n (i.e., 1^2 + 2^2 + . . . + n^2). -} 
sumSquares :: Int -> Int
sumSquares 0 = 0
sumSquares n = n * n + sumSquares (n - 1)

{- Using multiplication, write a recursive power function that raises the first argument to
the power of the second. You cannot use the power operator (^) for your solution. -}
power :: Int -> Int -> Int
power _ 0 = 1
power x n = x * power x (n - 1)

{- Write a recursive function sumFromTo that given two non-negative Ints, returns the
sum of all numbers between them. The second argument should be no smaller than
the first, otherwise sumFromTo should return 0 (see the table). -}
sumFromTo :: Int -> Int -> Int
sumFromTo x y
  | x > y = 0
  | otherwise = x + sumFromTo (x + 1) y

{- The greatest common divisor (GCD) of two non-negative can be defined as:
• If they are equal: their common value
• If they are not equal: the GCD of the following two values:
  – the positive difference between the two numbers
  – the smaller number
Write a gcd function that takes two non-negative integers and returns their GCD; your
function should use the above definition of GCD. -}
gcd :: Int -> Int -> Int
gcd a b
  | a == b = a 
  | a > b = gcd (a - b) b
  | otherwise = gcd a (b - a)

{- The integer square root of a positive integer n is the largest integer whose square is
less than or equal to n -}

{- Add the following definition to your Week3.hs file and save it -}
intSquareRoot :: Int -> Int
intSquareRoot n = findRoot n n

{- Now define a recursive function findRoot that given two arguments n and s, returns
the square root of n starting from the guess s. -}

findRoot :: Int -> Int -> Int
findRoot n s
  | s * s <= n = s
  | otherwise = findRoot n (s - 1)