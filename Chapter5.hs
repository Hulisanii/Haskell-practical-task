-----by Hulisani Muravha
module Main where

-- TASK 1: define a binary tree type
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Eq)

-- TASK 2: insert into a binary search tree
insertBST :: Ord a => a -> Tree a -> Tree a
insertBST x Empty = Node x Empty Empty
insertBST x (Node y left right)
  | x == y = Node y left right
  | x < y  = Node y (insertBST x left) right
  | x > y  = Node y left (insertBST x right)

-- TASK 3: build tree from list
fromList :: Ord a => [a] -> Tree a
fromList = foldr insertBST Empty

-- TASK 4: treeMap (map over values in tree)
treeMap :: (a -> b) -> Tree a -> Tree b
treeMap _ Empty = Empty
treeMap f (Node x l r) = Node (f x) (treeMap f l) (treeMap f r)

-- TASK 5: treeFold (inorder fold)
treeFold :: (b -> a -> b) -> b -> Tree a -> b
treeFold _ acc Empty = acc
treeFold f acc (Node x l r) =
  let accL = treeFold f acc l
      accM = f accL x
  in treeFold f accM r

-- TASK 6: treeToList (inorder)
treeToList :: Tree a -> [a]
treeToList = treeFold (\acc x -> acc ++ [x]) []

-- TASK 7: Maybe usage: safeHead
safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

-- TASK 8: Either usage: safeDiv
safeDiv :: (Eq a, Fractional a) => a -> a -> Either String a
safeDiv _ 0 = Left "Division by zero"
safeDiv a b = Right (a / b)

-- TASK 9: Expression AST and evaluator
data Expr = Const Int | Add Expr Expr | Mul Expr Expr deriving (Show, Eq)

eval :: Expr -> Int
eval (Const n)   = n
eval (Add a b)   = eval a + eval b
eval (Mul a b)   = eval a * eval b

-- TASK 10: prettyPrint expression (simple)
pretty :: Expr -> String
pretty (Const n) = show n
pretty (Add a b) = "(" ++ pretty a ++ " + " ++ pretty b ++ ")"
pretty (Mul a b) = "(" ++ pretty a ++ " * " ++ pretty b ++ ")"

-- Demo main
main :: IO ()
main = do
  putStrLn "Chapter 5 demos:"
  let nums = [7,3,9,1,5]
  let tree = fromList nums
  putStrLn $ "fromList " ++ show nums ++ " -> tree: " ++ show tree
  putStrLn $ "treeToList (inorder) = " ++ show (treeToList tree)
  putStrLn $ "treeMap (*2) tree -> " ++ show (treeMap (*2) tree)
  putStrLn $ "safeHead [] = " ++ show (safeHead ([] :: [Int]))
  putStrLn $ "safeHead [10,20] = " ++ show (safeHead [10,20 :: Int])
  putStrLn $ "safeDiv 10 2 = " ++ show (safeDiv 10 2 :: Either String Double)
  putStrLn $ "safeDiv 1 0 = " ++ show (safeDiv 1 0 :: Either String Double)
  let expr = Add (Const 2) (Mul (Const 3) (Const 4)) -- 2 + 3*4
  putStrLn $ "expr pretty = " ++ pretty expr ++ ", eval = " ++ show (eval expr)
