-- data Degree = Ordinary | Third | LowerSecond | UpperSecond | First 
-- 	deriving (Eq, Ord, Show, Read)

-- isgGood :: Degree -> Bool
-- isGood degree = degree == Uppersecond || degree == First


-- -- isGood' = Degree -> Bool 
-- -- isGood' = (>= UpperSecond)

-- -- type Person = (String, Int)
-- -- date Person = Person String Int
-- --     deriving (Eq, Show)

-- people :: [Person]
-- people = [Person "Jane" 17,
--           Person "Maria" 19,
--           Person "Tim" 16,
--           Person "Sam" 25]

-- name :: Person -> String 
-- -- name (person n _) = name

-- age :: Person -> Int
-- -- age (person _ a) = age

-- --could use record syntax


-- names :: [Person] -> [String]
-- names = map name

-- data Shape = Circle Float | Rectangle Float Float


-- perimeter :: Shape -> Float
-- perimeter (Circle r) = 2 * pi * record
-- perimeter (Rectangle h w) = 2 * (h + w)

-- data Tree = Null | 
--      Node Int Tree Tree
--      deriving (Show)

-- tree1 :: Tree
-- tree1 = Node 3 Null Null

-- tree2 :: Tree 
-- tree2 = Node 4 tree1 (Node 7 Null (Node 5 Null))


-- height :: Tree -> Int
-- height Null = 0
-- height (Node _ st1 st2) = 1 + max (height sleft) (height right)