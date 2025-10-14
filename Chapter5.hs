--- By: Hulisani Muravha
-- HC5T1 to HC5T10 Solutions

module Main where

-- HC5T1 - Using applyTwice
-- Define a function that takes a function and an integer, then applies the function three times
applyThrice :: (Int -> Int) -> Int -> Int
applyThrice f x = f (f (f x))

-- HC5T2 - Filtering Odd Numbers
-- Use the filter function to extract all odd numbers from 1 to 30
oddNumbers :: [Int]
oddNumbers = filter odd [1..30]

-- HC5T3 - Checking for Uppercase Letters
-- Use any to check if any word starts with an uppercase letter
hasUppercaseWord :: [String] -> Bool
hasUppercaseWord = any (\word -> not (null word) && head word `elem` ['A'..'Z'])

-- HC5T4 - Using Lambda Functions
-- Rewrite biggerThan10 using a lambda function
biggerThan10 :: Int -> Bool
biggerThan10 = (\x -> x > 10)

-- HC5T5 - Partial Application
-- Create a function multiplyByFive that multiplies any number by 5
multiplyByFive :: Int -> Int
multiplyByFive = (*5)

-- HC5T6 - Function Composition
-- Create a function that returns squares filtered to only keep the even ones
evenSquares :: [Int] -> [Int]
evenSquares = filter even . map (^2)

-- HC5T7 - The $ Operator
-- Rewrite using the $ operator for cleaner code
result :: Int
result = sum $ map (*2) $ filter (>3) [1..10]

-- HC5T8 - Point-Free Style
-- Convert addFive to point-free style
addFive :: Int -> Int
addFive = (+5)

-- HC5T9 - Higher-Order Function to Transform a List
-- Applies a given function twice to every element of a list
transformList :: (a -> a) -> [a] -> [a]
transformList f = map (f . f)

-- HC5T10 - Combining Higher-Order Functions
-- Checks if any squared value in a list is greater than 50
hasLargeSquare :: [Int] -> Bool
hasLargeSquare = any (>50) . map (^2)

-- DEMONSTRATION AND TESTING
main :: IO ()
main = do
  putStrLn "HC5T1:"
  print (applyThrice (+1) 5)
  print (applyThrice (*2) 2)
  putStrLn ""

  putStrLn "HC5T2:"
  print oddNumbers
  putStrLn ""

  putStrLn "HC5T3:"
  print (hasUppercaseWord ["apple", "Banana", "cherry"])
  print (hasUppercaseWord ["dog", "cat", "fish"])
  putStrLn ""

  putStrLn "HC5T4:"
  print (biggerThan10 5)
  print (biggerThan10 15)
  putStrLn ""

  putStrLn "HC5T5:"
  print (multiplyByFive 3)
  print (multiplyByFive 10)
  putStrLn ""

  putStrLn "HC5T6:"
  print (evenSquares [1..10])
  putStrLn ""

  putStrLn "HC5T7:"
  print result
  putStrLn ""

  putStrLn "HC5T8:"
  print (addFive 7)
  print (addFive 20)
  putStrLn ""

  putStrLn "HC5T9:"
  print (transformList (+1) [1,2,3])
  print (transformList (*2) [1,2,3])
  putStrLn ""

  putStrLn "HC5T10:"
  print (hasLargeSquare [3,5,6])
  print (hasLargeSquare [2,4,5])
  putStrLn ""

