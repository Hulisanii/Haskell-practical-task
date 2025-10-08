--By Hulisani Muravha 
module Main where

-- HC6T1: Factorial (Recursive)
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- HC6T2: Fibonacci (Recursive)
fibonacci :: Int -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)
fibonacciFast :: Int -> Integer
fibonacciFast n = fibs !! n
  where fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- HC6T3: Sum of Elements Using foldr
sumFoldr :: Num a => [a] -> a
sumFoldr = foldr (+) 0

-- HC6T4: Product of Elements Using foldl
productFoldl :: Num a => [a] -> a
productFoldl = foldl (*) 1

-- HC6T5: Reverse a List (Recursive)
reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]
reverseList' :: [a] -> [a]
reverseList' xs = rev xs []
  where
    rev [] acc     = acc
    rev (x:xs) acc = rev xs (x:acc)
    
-- HC6T6: Element Exists in List
elementExists :: Eq a => a -> [a] -> Bool
elementExists _ [] = False
elementExists x (y:ys)
    | x == y    = True
    | otherwise = elementExists x ys
elementExistsFold :: Eq a => a -> [a] -> Bool
elementExistsFold x = foldr (\y acc -> x == y || acc) False

-- HC6T7: List Length
listLength :: [a] -> Int
listLength [] = 0
listLength (_:xs) = 1 + listLength xs
listLengthFold :: [a] -> Int
listLengthFold = foldr (\_ acc -> 1 + acc) 0

-- HC6T8: Filter Even Numbers
filterEven :: Integral a => [a] -> [a]
filterEven [] = []
filterEven (x:xs)
    | even x    = x : filterEven xs
    | otherwise = filterEven xs
filterEvenFold :: Integral a => [a] -> [a]
filterEvenFold = foldr (\x acc -> if even x then x:acc else acc) []

-- HC6T9: Map Implementation
myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = f x : myMap f xs
myMapFoldr :: (a -> b) -> [a] -> [b]
myMapFoldr f = foldr (\x acc -> f x : acc) []

-- HC6T10: Digits of a Number (Recursive)
digits :: Int -> [Int]
digits n
    | n < 0     = digits (-n)
    | n < 10    = [n]
    | otherwise = digits (n `div` 10) ++ [n `mod` 10]
digits' :: Int -> [Int]
digits' n = reverse (digitsRev n)
  where
    digitsRev x
        | x < 0     = digitsRev (-x)
        | x < 10    = [x]
        | otherwise = x `mod` 10 : digitsRev (x `div` 10)


-- DEMONSTRATION AND TESTING
main :: IO ()
main = do
    putStrLn "HC6T1:"
    putStrLn $ "factorial 5 = " ++ show (factorial 5)
    putStrLn $ "factorial 10 = " ++ show (factorial 10)
    putStrLn ""
    
    putStrLn "HC6T2:"
    putStrLn $ "fibonacci 10 = " ++ show (fibonacci 10)
    putStrLn $ "fibonacci 15 = " ++ show (fibonacci 15)
    putStrLn $ "fibonacciFast 20 = " ++ show (fibonacciFast 20)
    putStrLn ""
    
    putStrLn "HC6T3:"
    putStrLn $ "sumFoldr [1..10] = " ++ show (sumFoldr [1..10])
    putStrLn $ "sumFoldr [2,4,6,8] = " ++ show (sumFoldr [2,4,6,8])
    putStrLn ""
    
    putStrLn "HC6T4:"
    putStrLn $ "productFoldl [1,2,3,4,5] = " ++ show (productFoldl [1,2,3,4,5])
    putStrLn $ "productFoldl [2,3,4] = " ++ show (productFoldl [2,3,4])
    putStrLn ""
    
    putStrLn "HC6T5:"
    putStrLn $ "reverseList [1,2,3,4,5] = " ++ show (reverseList [1,2,3,4,5])
    putStrLn $ "reverseList' \"hello\" = " ++ show (reverseList' "hello")
    putStrLn ""
    
    putStrLn "HC6T6:"
    putStrLn $ "elementExists 3 [1,2,3,4,5] = " ++ show (elementExists 3 [1,2,3,4,5])
    putStrLn $ "elementExists 6 [1,2,3,4,5] = " ++ show (elementExists 6 [1,2,3,4,5])
    putStrLn $ "elementExistsFold 'e' \"hello\" = " ++ show (elementExistsFold 'e' "hello")
    putStrLn ""
    
    putStrLn "HC6T7:"
    putStrLn $ "listLength [1,2,3,4,5] = " ++ show (listLength [1,2,3,4,5])
    putStrLn $ "listLength \"Haskell\" = " ++ show (listLength "Haskell")
    putStrLn $ "listLengthFold [10,20,30] = " ++ show (listLengthFold [10,20,30])
    putStrLn ""
    
    putStrLn "HC6T8:"
    putStrLn $ "filterEven [1,2,3,4,5,6,7,8,9,10] = " ++ show (filterEven [1,2,3,4,5,6,7,8,9,10])
    putStrLn $ "filterEvenFold [15,22,37,44,59] = " ++ show (filterEvenFold [15,22,37,44,59])
    putStrLn ""
    
    putStrLn "HC6T9:"
    putStrLn $ "myMap (*2) [1,2,3,4,5] = " ++ show (myMap (*2) [1,2,3,4,5])
    putStrLn $ "myMapFoldr (^2) [1,2,3,4] = " ++ show (myMapFoldr (^2) [1,2,3,4])
    putStrLn ""
    
    putStrLn "HC6T10:"
    putStrLn $ "digits 12345 = " ++ show (digits 12345)
    putStrLn $ "digits 987 = " ++ show (digits 987)
    putStrLn $ "digits' 12345 = " ++ show (digits' 12345)
    putStrLn ""
    
  
