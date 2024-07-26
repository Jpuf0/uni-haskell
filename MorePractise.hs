
timesTen :: Int -> Int
timesTen x = x * 10

sumThree :: Int -> Int -> Int -> Int
sumThree x y z = x + y + z

areaOfCircle :: Float -> Float
areaOfCircle x = pi * x * x

volumeOfCylinder :: Float -> Float -> Float
volumeOfCylinder x h = areaOfCircle x * h

distance :: Float -> Float -> Float -> Float -> Float
distance x1 y1 x2 y2 = sqrt (difference x1 x2 + difference y1 y2)
  where
    difference x y = (x - y) ^ 2

threeDifferent :: Int -> Int -> Int -> Bool
threeDifferent x y z = x /= y && y /= z

divisibleBy :: Int -> Int -> Bool
divisibleBy x y = mod x y == 0

averageThree :: Int -> Int -> Int -> Float
averageThree x y z = fromIntegral (x + y + z) / 3

isEven :: Int -> Bool
isEven x = mod x 2 == 0

absolute :: Int -> Int
absolute x = if x <= 0 then -(x) else x

absolute2 :: Int -> Int
absolute2 x
  | x <= 0 = -(x)
  | otherwise = x

sign :: Int -> Int
sign x
  | x > 0 = 1
  | x < 0 = -1
  | otherwise = 0

howManyEqual :: Int -> Int -> Int -> Int
howManyEqual x y z
  | x == z && x == y = 3
  | x == y || x == z || y == z = 2
  | otherwise = 0

sumDiagonalLengths :: Float -> Float -> Float -> Float
sumDiagonalLengths x y z = diag (x + y + z)
  where
    diag x = sqrt (2 * x ^ 2)

taxiFare :: Int -> Float
taxiFare x
  | x <= 10 = 2.2 + (fromIntegral x * 0.5)
  | x > 10 = 2.2 + (10 * 0.5) + ((fromIntegral x - 10) * 0.3)

howManyAboveAverage :: [Float] -> Int
howManyAboveAverage [] = 0 -- Base case: empty list
howManyAboveAverage (x : xs)= countAboveAverage (x : xs) (avg xs)

  where 
    avg xs = (foldr (+) 0 xs) / fromIntegral (length xs)

    countAboveAverage [] _ = 0 
    countAboveAverage (x : xs) average
      | x > average = 1 + countAboveAverage xs average
      | otherwise = countAboveAverage xs average

