---by Hulisani Muravha 
module Main where

-- TASK 1: factorial (recursive)
-- Step: base case 0 -> 1; recursive step n * factorial (n-1)
factorial :: (Eq a, Num a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- TASK 2: fibonacci (naive recursion)
-- Step: fib 0 = 0; fib 1 = 1; fib n = fib (n-1) + fib (n-2)
fibonacci :: (Eq a, Num a, Num b, Ord a) => a -> b
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

-- TASK 3: sumList (recursive)
sumList :: Num a => [a] -> a
sumList []     = 0
sumList (x:xs) = x + sumList xs

-- TASK 4: productList (recursive)
productList :: Num a => [a] -> a
productList []     = 1
productList (x:xs) = x * productList xs

-- TASK 5: length' (recursive)
length' :: [a] -> Int
length' []     = 0
length' (_:xs) = 1 + length' xs

-- TASK 6: elemExists (recursive)
elemExists :: Eq a => a -> [a] -> Bool
elemExists _ [] = False
elemExists y (x:xs) = (y == x) || elemExists y xs

-- TASK 7: reverse' (recursive)
reverse' :: [a] -> [a]
reverse' []     = []
reverse' (x:xs) = reverse' xs ++ [x]

-- TASK 8: map' (recursive)
map' :: (a -> b) -> [a] -> [b]
map' _ []     = []
map' f (x:xs) = f x : map' f xs

-- TASK 9: filter' (recursive)
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
  | p x       = x : filter' p xs
  | otherwise = filter' p xs

-- TASK 10: take' (recursive)
take' :: Int -> [a] -> [a]
take' _ [] = []
take' n _
  | n <= 0 = []
take' n (x:xs) = x : take' (n - 1) xs

-- Demo main
main :: IO ()
main = do
  putStrLn "Chapter 3 demos:"
  putStrLn $ "factorial 6 = " ++ show (factorial 6)
  putStrLn $ "fibonacci 10 = " ++ show (fibonacci 10 :: Int)
  putStrLn $ "sumList [1..5] = " ++ show (sumList [1..5])
  putStrLn $ "productList [1..5] = " ++ show (productList [1..5])
  putStrLn $ "length' \"hello\" = " ++ show (length' "hello")
  putStrLn $ "elemExists 3 [1,2,3] = " ++ show (elemExists 3 [1,2,3])
  putStrLn $ "reverse' [1,2,3] = " ++ show (reverse' [1,2,3])
  putStrLn $ "map' (^2) [1,2,3] = " ++ show (map' (^2) [1,2,3 :: Int])
  putStrLn $ "filter' odd [1..10] = " ++ show (filter' odd [1..10])
  putStrLn $ "take' 3 [10,20,30,40] = " ++ show (take' 3 [10,20,30,40 :: Int])
