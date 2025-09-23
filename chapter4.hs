----by Hulisani Muravha 
module Main where

-- TASK 1: map using foldr
mapFoldr :: (a -> b) -> [a] -> [b]
mapFoldr f = foldr (\x acc -> f x : acc) []

-- TASK 2: filter using foldr
filterFoldr :: (a -> Bool) -> [a] -> [a]
filterFoldr p = foldr (\x acc -> if p x then x:acc else acc) []

-- TASK 3: foldl implementation (explicit, left fold)
myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl _ acc []     = acc
myFoldl f acc (x:xs) = myFoldl f (f acc x) xs

-- TASK 4: quicksort using filter (higher-order style)
quicksort :: Ord a => [a] -> [a]
quicksort []     = []
quicksort (p:xs) =
  let left  = filter (< p) xs
      right = filter (>= p) xs
  in quicksort left ++ [p] ++ quicksort right

-- TASK 5: function composition example (apply two functions)
composeExample :: (b -> c) -> (a -> b) -> a -> c
composeExample f g = f . g

-- TASK 6: curry and uncurry
myCurry :: ((a, b) -> c) -> a -> b -> c
myCurry f a b = f (a, b)

myUncurry :: (a -> b -> c) -> (a, b) -> c
myUncurry f (a, b) = f a b

-- TASK 7: applyN (apply a function n times)
applyN :: Int -> (a -> a) -> a -> a
applyN n f x
  | n <= 0    = x
  | otherwise = applyN (n - 1) f (f x)

-- TASK 8: zipWith' (re-implement)
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

-- TASK 9: iterate' (produce infinite list by repeatedly applying f)
iterate' :: (a -> a) -> a -> [a]
iterate' f x = x : iterate' f (f x)

-- TASK 10: all/any using folds
allFold :: (a -> Bool) -> [a] -> Bool
allFold p = foldr (\x acc -> p x && acc) True

anyFold :: (a -> Bool) -> [a] -> Bool
anyFold p = foldr (\x acc -> p x || acc) False

-- Demo main
main :: IO ()
main = do
  putStrLn "Chapter 4 demos:"
  putStrLn $ "mapFoldr (*2) [1,2,3] = " ++ show (mapFoldr (*2) [1,2,3 :: Int])
  putStrLn $ "filterFoldr even [1..6] = " ++ show (filterFoldr even [1..6 :: Int])
  putStrLn $ "myFoldl (+) 0 [1..5] = " ++ show (myFoldl (+) 0 [1..5 :: Int])
  putStrLn $ "quicksort [3,1,4,1,5,9] = " ++ show (quicksort [3,1,4,1,5,9 :: Int])
  putStrLn $ "composeExample (show . (+1)) 5 = " ++ show ((composeExample show (+1)) 5)
  putStrLn $ "myCurry fstPair 3 4 = " ++ show ((myCurry (\(a,b) -> a + b) 3 4) :: Int)
  putStrLn $ "applyN 3 (+2) 0 = " ++ show (applyN 3 (+2) 0)
  putStrLn $ "zipWith' (*) [1,2] [3,4] = " ++ show (zipWith' (*) [1,2 :: Int] [3,4 :: Int])
  putStrLn $ "take 5 of iterate' (*2) 1 = " ++ show (take 5 (iterate' (*2) 1 :: [Int]))
  putStrLn $ "allFold (>0) [1,2,3] = " ++ show (allFold (>0) [1,2,3 :: Int]) ++
             ", anyFold (==2) [1,2,3] = " ++ show (anyFold (==2) [1,2,3 :: Int])
