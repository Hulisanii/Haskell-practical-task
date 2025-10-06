--- by Hulisani Muravha 
module Main where
-- 1. type synonyms
type Name = String
type Age  = Int

-- 2. simple Person ADT
data Person = Person Name Age
  deriving (Show, Eq)

-- 3. Maybe-like custom type (for demonstration)
data MyMaybe a = MyNothing | MyJust a
  deriving (Show, Eq)

-- 4. Either-like custom type
data MyEither a b = MyLeft a | MyRight b
  deriving (Show, Eq)

-- 5. functions on Person
isAdult :: Person -> Bool
isAdult (Person _ age) = age >= 18

greetPerson :: Person -> String
greetPerson (Person name _) = "Hi, " ++ name ++ "!"

-- 6. converting between MyMaybe and Maybe
toMaybe :: MyMaybe a -> Maybe a
toMaybe MyNothing  = Nothing
toMaybe (MyJust x) = Just x

fromMaybe' :: Maybe a -> MyMaybe a
fromMaybe' Nothing  = MyNothing
fromMaybe' (Just x) = MyJust x

-- 7. operate on MyEither
eitherToTuple :: MyEither a b -> (Maybe a, Maybe b)
eitherToTuple (MyLeft a)  = (Just a, Nothing)
eitherToTuple (MyRight b) = (Nothing, Just b)

-- 8. recursive ADT: binary tree

data MyTree a = MyEmpty | MyNode a (MyTree a) (MyTree a)
  deriving (Show, Eq)

treeSize :: MyTree a -> Int
treeSize MyEmpty = 0
treeSize (MyNode _ l r) = 1 + treeSize l + treeSize r

treeInsert :: (Ord a) => a -> MyTree a -> MyTree a
treeInsert x MyEmpty = MyNode x MyEmpty MyEmpty
treeInsert x (MyNode v l r)
  | x == v    = MyNode v l r
  | x < v     = MyNode v (treeInsert x l) r
  | otherwise = MyNode v l (treeInsert x r)

-- main demonstrates ADTs and type synonyms:
main :: IO ()
main = do
  let p = Person "Hulisani" 23  -- type synonym Name used via Person constructor
  putStrLn "Chapter 10: Types & ADTs demo"
  putStrLn $ "Person: " ++ show p
  putStrLn $ "isAdult: " ++ show (isAdult p)
  putStrLn $ "greetPerson: " ++ greetPerson p

  -- MyMaybe demo
  let mm1 = MyJust 5
      mm2 = MyNothing :: MyMaybe Int
  putStrLn $ "toMaybe (MyJust 5): " ++ show (toMaybe mm1)
  putStrLn $ "toMaybe MyNothing: " ++ show (toMaybe mm2)

  -- MyEither demo
  putStrLn $ "eitherToTuple (MyLeft \"err\"): " ++ show (eitherToTuple (MyLeft "err"))
  putStrLn $ "eitherToTuple (MyRight 42): " ++ show (eitherToTuple (MyRight 42))

  -- MyTree demo
  let values = [7,3,9,1,5]
      t = foldr treeInsert MyEmpty values
  putStrLn $ "Tree built from " ++ show values ++ ": " ++ show t
  putStrLn $ "treeSize: " ++ show (treeSize t)
