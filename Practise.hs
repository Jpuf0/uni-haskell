sumOfAllElements :: [Int] -> Int
sumOfAllElements (x : xs) = x + sumOfAllElements xs

sumOfAllElements2 :: [Int] -> Int
sumOfAllElements2 = foldr (+) 0

--higher order function, operation, starting point

productOfList :: [Int] -> Int
productOfList (x : xs) = x * productOfList xs

productOfList2 :: [Int] -> Int
productOfList2 = foldr (*) 1


reverseList :: String -> String
reverseList [] = []
reverseList (x : xs) = reverseList xs ++ [x]

-- ++ concats the 2 lists together (no change in order)

lengthList :: [Int] -> Int
lengthList x = length x 

pallindromeCheck :: String -> Bool
pallindromeCheck word1 = reverse1 == word1
    where 
        reverse1 = reverseList word1
    

