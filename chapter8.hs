----by Hulisani Muravha
-- 1. doubleAll using map
doubleAll :: Num a => [a] -> [a]
doubleAll = map (*2)

-- 2. filterEven using filter
filterEven :: Integral a => [a] -> [a]
filterEven = filter even

-- 3. sumWithFold using foldl
sumWithFold :: Num a => [a] -> a
sumWithFold = foldl (+) 0

-- 4. maxWithFold using foldr1
maxWithFold :: (Ord a) => [a] -> a
maxWithFold = foldr1 max

-- 5. compose example and applyNTimes
compose :: (b -> c) -> (a -> b) -> a -> c
compose f g x = f (g x)

applyNTimes :: Int -> (a -> a) -> a -> a
applyNTimes n f x
  | n <= 0    = x
  | otherwise = applyNTimes (n-1) f (f x)

-- 6. zipWith example: pairwise sum
pairwiseSum :: Num a => [a] -> [a] -> [a]
pairwiseSum = zipWith (+)

-- 7. example of higher-order returning function
adder :: Int -> (Int -> Int)
adder n = \x -> n + x  -- lambda: \x -> ...

-- main demonstrates them:
main :: IO ()
main = do
  let xs = [1..6]
  putStrLn "Chapter 8: Higher-order functions demo"
  putStrLn $ "original: " ++ show xs
  putStrLn $ "doubleAll: " ++ show (doubleAll xs)
  putStrLn $ "filterEven: " ++ show (filterEven xs)
  putStrLn $ "sumWithFold: " ++ show (sumWithFold xs)
  putStrLn $ "maxWithFold: " ++ show (maxWithFold xs)
  putStrLn $ "applyNTimes (3) (*2) 1: " ++ show (applyNTimes 3 (*2) 1)
  putStrLn $ "pairwiseSum [1,2,3] [4,5,6]: " ++ show (pairwiseSum [1,2,3] [4,5,6])
  putStrLn $ "adder 10 applied to 7: " ++ show ((adder 10) 7)

