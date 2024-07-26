circumferenceOfCircle :: Float -> Float
circumferenceOfCircle d = pi * d

sideOfCylinder :: Float -> Float -> Float
sideOfCylinder d h = circumferenceOfCircle d * h

canDrink :: Int -> Bool
canDrink age = age >= 18

all3CanDrink :: Int -> Int -> Int -> Bool
all3CanDrink a b c = canDrink a && canDrink b && canDrink c

timesTen :: Int -> Int
timesTen a = 10 * a

sumThree :: Int -> Int -> Int -> Int
sumThree a b c = a + b + c

areaOfCircle :: Float -> Float
areaOfCircle a = pi * (a * a)

volumeOfCylinder :: Float -> Float -> Float
volumeOfCylinder a b = areaOfCircle a * b

distance :: Float -> Float -> Float -> Float -> Float
distance a b c d = sqrt(((a - c)*(a - c)) + ((b - d)*(b - d)))

threeDifferent :: Int -> Int -> Int -> Bool
threeDifferent a b c = a /= b && a /= c && b /= c  

divisibleBy :: Int -> Int -> Bool
divisibleBy a b = (mod a b) == 0

isEven :: Int -> Bool
isEven a = divisibleBy a 2

averageThree :: Int -> Int -> Int -> Float
averageThree a b c = fromIntegral (a + b + c) /3

absolute :: Int -> Int
absolute a = if a < 0 then a - (a * 2) else a