---by Hulisani Muravha 
-- 1. sumList: recursive sum
sumList :: Num a => [a] -> a
sumList []     = 0
sumList (x:xs) = x + sumList xs

-- 2. productList: recursive product
productList :: Num a => [a] -> a
productList []     = 1
productList (x:xs) = x * productList xs

-- 3. myLength: recursive length
myLength :: [a] -> Int
myLength []     = 0
myLength (_:xs) = 1 + myLength xs

-- 4. myReverse: naive recursive reverse
myReverse :: [a] -> [a]
myReverse []     = []
myReverse (x:xs) = myReverse xs ++ [x]

-- 5. takeWhile' : implementation using recursion
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs)
  | p x       = x : takeWhile' p xs
  | otherwise = []

-- 6. quickSort (recursive example)
quickSort :: (Ord a) => [a] -> [a]
quickSort []     = []
quickSort (p:xs) = quickSort smaller ++ [p] ++ quickSort larger
  where
    smaller = [x | x <- xs, x <= p]
    larger  = [x | x <- xs, x > p]

-- 7. tails (generate list of suffixes)
tails' :: [a] -> [[a]]
tails' [] = [[]]
tails' xs@(_:xs') = xs : tails' xs'

-- A simple main that demonstrates the functions:
main :: IO ()
main = do
  -- do: starts a sequence of IO actions
  let lst = [5,2,9,1,7,3] -- let: local binding for pure code
  putStrLn "Chapter 7: Lists & Recursion demo"
  putStrLn $ "list: " ++ show lst
  putStrLn $ "sumList: " ++ show (sumList lst)
  putStrLn $ "productList: " ++ show (productList lst)
  putStrLn $ "myLength: " ++ show (myLength lst)
  putStrLn $ "myReverse: " ++ show (myReverse lst)
  putStrLn $ "takeWhile (<5): " ++ show (takeWhile' (<5) lst)
  putStrLn $ "quickSort: " ++ show (quickSort lst)
  putStrLn $ "tails: " ++ show (tails' lst)
