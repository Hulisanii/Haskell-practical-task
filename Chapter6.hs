----by Hulisani Muravha
module Main where

-- TASK 1: custom data type Point
data Point = Point { px :: Int, py :: Int } deriving (Read)

-- TASK 2: Eq instance (by coordinates)
instance Eq Point where
  (Point x1 y1) == (Point x2 y2) = x1 == x2 && y1 == y2

-- TASK 3: Show instance (custom format)
instance Show Point where
  show (Point x y) = "Point(" ++ show x ++ "," ++ show y ++ ")"

-- TASK 4: Ord instance (lexicographic)
instance Ord Point where
  compare (Point x1 y1) (Point x2 y2) = compare (x1, y1) (x2, y2)

-- TASK 5: Functor instance for Tree (reuse Tree from Chapter5)
data Tree a = E | N a (Tree a) (Tree a) deriving (Eq, Show)

instance Functor Tree where
  fmap _ E = E
  fmap f (N x l r) = N (f x) (fmap f l) (fmap f r)

-- TASK 6: Foldable instance for Tree (inorder)
instance Foldable Tree where
  foldMap _ E = mempty
  foldMap f (N x l r) = foldMap f l <> f x <> foldMap f r

-- TASK 7: Monoid for a Sum wrapper
newtype SumA = SumA { getSumA :: Int } deriving (Show, Eq)

instance Semigroup SumA where
  (SumA a) <> (SumA b) = SumA (a + b)

instance Monoid SumA where
  mempty = SumA 0

-- TASK 8: typeclass Printable and instance for Point
class Printable a where
  pPrint :: a -> String

instance Printable Point where
  pPrint = show

instance Printable Int where
  pPrint = show

-- TASK 9: convertible typeclass (example)
class Convertible a b where
  convert :: a -> b

instance Convertible Int String where
  convert = show

-- TASK 10: using typeclass constraints in function
describe :: (Printable a, Show a) => a -> String
describe x = "Printable: " ++ pPrint x ++ " | Show: " ++ show x

-- Demo main
main :: IO ()
main = do
  putStrLn "Chapter 6 demos:"
  let p1 = Point 1 2
      p2 = Point 2 3
  putStrLn $ "p1 = " ++ show p1 ++ ", p2 = " ++ show p2
  putStrLn $ "p1 == p2? " ++ show (p1 == p2)
  putStrLn $ "p1 < p2? " ++ show (p1 < p2)
  let t = N 1 (N 0 E E) (N 2 E E) :: Tree Int
  putStrLn $ "tree fmap (+1) -> " ++ show (fmap (+1) t)
  putStrLn $ "foldMap SumA on tree -> " ++ show (foldMap (SumA . id) t)
  putStrLn $ "SumA mempty <> SumA 5 = " ++ show (mconcat [mempty, SumA 5])
  putStrLn $ "describe p1 = " ++ describe p1
  putStrLn $ "convert (5 :: Int) :: String = " ++ (convert (5 :: Int) :: String)
