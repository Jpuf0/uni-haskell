-- Day algebraic type
data Day = Mon | Tue | Wed | Thur | Fri | Sat | Sun
  deriving (Eq, Ord, Show, Read)

-- Alternative definitions of isWeekend function
isWeekend :: Day -> Bool
isWeekend Sat = True
isWeekend Sun = True
isWeekend _ = False

isWeekend2 day = day == Sat || day == Sun

isWeekend3 day = day >= Sat

-- Copy of StudentMark type synonym from worksheet 4
data StudentMark = Student String Int
  deriving (Eq, Show)

betterStudent :: StudentMark -> StudentMark -> String
betterStudent (Student s1 m1) (Student s2 m2)
  | m1 >= m2 = s1
  | otherwise = s2

-- Shapes algebraic type
data Shape = Circle Float | Rectangle Float Float

area :: Shape -> Float
area (Circle r) = pi * r * r
area (Rectangle h w) = h * w

-- Address algebraic type (note that a constructor can have
-- the same name as the type).
data Address = Address Building String
  deriving (Show)

data Building = Name String | Number Int
  deriving (Show)

-- Binary tree algebraic type
data Tree = Null | Node Int Tree Tree
  deriving (Show)

-- Binary tree test data
testTree = Node 20 (Node 3 (Node 12 Null Null) (Node 7 Null Null)) (Node 8 (Node 4 (Node 6 Null Null) Null) Null)

-- Binary search tree test data
testSearchTree = Node 5 (Node 1 Null Null) (Node 8 (Node 7 Null Null) Null)

height :: Tree -> Int
height Null = 0
height (Node _ st1 st2) = 1 + max (height st1) (height st2)

sumValues :: Tree -> Int
sumValues Null = 0
sumValues (Node n st1 st2) = n + sumValues st1 + sumValues st2

--Exercise 1
data Month = January | February | March | April | May | June | July | August | September | October | November | December deriving (Eq, Ord,Show,Read)
data Season = Summer | Autumn | Spring | Winter deriving (Eq, Ord,Show,Read)

--Exercise 2
season :: Month -> Season 
season month 
    | month == December || month == January || month == February = Winter
    | month == March || month == April || month == May = Spring
    | month == June || month == July || month == August = Summer
    | month == September || month == October || month == November = Autumn


--Exercise 3
numberOfDays :: Month -> Int -> Int 
numberOfDays month year 
    | month == April || month == June || month == September || month == November = 30
    | month == February && (mod year 4 == 0) = 29
    | month == February && (mod year 4 /= 0) = 28
    | otherwise = 31


--Exercise 4
data Point = Point Float Float 
    deriving (Show)
    --The 2 floats are the 2 numbers that make up a coordinate

data Shape = Circle Float |
             Rectangle Float Float
    deriving (Show)
    --For cirle the float is the radius 
    --For rectangle the float is the width and height 
    --These are parameters so need to be added whenever a shape is created 


--Exercise 5
data PositionedShape = PositionedShape Shape Point
    deriving (Show)
--This combines a shape with its center point 
 

--Exercise 6
move :: PositionedShape -> Float -> Float -> PositionedShape
move (PositionedShape shape (Point x y)) dx dy = PositionedShape shape (Point (x + dx) (y + dy))
--Takes the input and combines it into a positionedShape
    --shape will be pattered matched 
    --Point needs to be separated because we need to use x and y in the function, if we didnt need x and y separately 
    --it could be a variable like shape

--let positionedShape = PositionedShape (Circle 5.0) (Point 2.0 3.0)
--move positionedShape 1.0 (-2.0)


--move (PositionedShape (Circle 5.0) (Point 2.0 3.0)) 1.0 (-2.0)


--Practise 
area :: Shape -> Float
area (Circle r) = pi * r * r
area (Rectangle w h) = w * h
--This is better done with pattern matching because there is a different number of inputs

perimeter :: Shape -> Float
perimeter (Circle r) = 2 * pi * r
perimeter (Rectangle w h) = 2 * w + h


 

data Tree = Null | 
     Node Int Tree Tree
     deriving (Show, Eq)

-- Binary tree test data
testTree = Node 20 (Node 3 (Node 12 Null Null) (Node 7 Null Null))
                  (Node 8 (Node 4 (Node 6 Null Null) Null) Null)

-- Binary search tree test data
testSearchTree =  Node 5 (Node 1 Null Null)
                         (Node 8 (Node 7 Null Null) Null)

testBinaryTree = Node 5 (Node 1 Null Null) (Node 8 (Node 7 Null Null) Null)



numberOfNodes :: Tree -> Int
numberOfNodes Null = 0 
numberOfNodes (Node _ left right) = 1 + numberOfNodes left + numberOfNodes right

isMember :: Int -> Tree -> Bool
isMember _ Null = False
isMember number (Node nodeValue left right) 
    |number == nodeValue = True
    |otherwise = isMember number left || isMember number right


leaves :: Tree -> [Int]
leaves Null = []
leaves (Node number left right)
    | left == Null && right == Null = [number]
    | left == Null = leaves right  
    | right == Null = leaves left   
    | otherwise = leaves left ++ leaves right


inOrder :: Tree -> [Int]
inOrder Null = []  -- Base case: Empty tree
inOrder (Node number left right) =
    inOrder left ++ [number] ++ inOrder right

preOrder :: Tree -> [Int]
preOrder Null = [] -- Base case: Empty tree
preOrder(Node number left right) = [number] ++ preOrder left ++ preOrder right

postOrder :: Tree -> [Int]
postOrder Null = []
postOrder (Node number left right) = postOrder left ++ postOrder right ++ [number]

insert :: Int -> Tree -> Tree
insert number Null = Node number Null Null
insert number (Node value left right)
    | value < number = Node number (insert value left) right 
    | otherwise  = Node number left (insert value right)

listToSearchTree :: [Int]  -> Tree
listToSearchTree [] = Null
listToSearchTree (x : xs) = insert x (listToSearchTree xs)


binaryTreeSort :: [Int] -> [Int]
binaryTreeSort numbers = inOrder (listToSearchTree numbers)


--this puts each element of the list into at binary tree, then puts it back into a list to get the list into order 