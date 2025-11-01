
-- By Hulisani Muravha
-- Chapter12Programs.hs
-- Haskell Chapter 12 Practical Tasks (HC12T1–HC12T10)

import Data.List (sort)
import System.IO

------------------------------------------------------------
-- HC12T1: Print a Welcome Message
------------------------------------------------------------
main1 :: IO ()
main1 = putStrLn "Welcome to Haskell Programming!"

------------------------------------------------------------
-- HC12T2: Add Two Numbers
------------------------------------------------------------
addTwoNumbers :: Int -> Int -> Int
addTwoNumbers x y = x + y

main2 :: IO ()
main2 = do
    let result = addTwoNumbers 5 3
    putStrLn $ "Sum: " ++ show result

------------------------------------------------------------
-- HC12T3: Factorial Function
------------------------------------------------------------
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

main3 :: IO ()
main3 = do
    putStrLn "Enter a number for factorial:"
    input <- getLine
    let n = read input :: Integer
    putStrLn $ "Factorial of " ++ show n ++ " is " ++ show (factorial n)

------------------------------------------------------------
-- HC12T4: First 10 Fibonacci Numbers
------------------------------------------------------------
fibonacci :: Int -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

main4 :: IO ()
main4 = do
    let fibs = map fibonacci [0..9]
    putStrLn "First 10 Fibonacci numbers:"
    mapM_ (putStrLn . show) fibs

------------------------------------------------------------
-- HC12T5: Palindrome Checker
------------------------------------------------------------
isPalindrome :: String -> Bool
isPalindrome str = str == reverse str

main5 :: IO ()
main5 = do
    putStrLn "Enter a string to check for palindrome:"
    input <- getLine
    if isPalindrome input
        then putStrLn "It's a palindrome!"
        else putStrLn "It's not a palindrome."

------------------------------------------------------------
-- HC12T6: Sort a List of Integers
------------------------------------------------------------
main6 :: IO ()
main6 = do
    putStrLn "Enter integers separated by spaces:"
    input <- getLine
    let numbers = map read (words input) :: [Int]
    let sorted = sort numbers
    putStrLn $ "Sorted list: " ++ show sorted

------------------------------------------------------------
-- HC12T7: Calculate Circle Area
------------------------------------------------------------
calculateCircleArea :: Double -> Double
calculateCircleArea radius = pi * radius * radius

main7 :: IO ()
main7 = do
    putStrLn "Enter circle radius:"
    input <- getLine
    let radius = read input :: Double
    putStrLn $ "Area: " ++ show (calculateCircleArea radius)

------------------------------------------------------------
-- HC12T8: Merge Two Sorted Lists
------------------------------------------------------------
mergeLists :: Ord a => [a] -> [a] -> [a]
mergeLists [] ys = ys
mergeLists xs [] = xs
mergeLists (x:xs) (y:ys)
    | x <= y    = x : mergeLists xs (y:ys)
    | otherwise = y : mergeLists (x:xs) ys

main8 :: IO ()
main8 = do
    let list1 = [1,3,5,7]
    let list2 = [2,4,6,8]
    let merged = mergeLists list1 list2
    putStrLn $ "List 1: " ++ show list1
    putStrLn $ "List 2: " ++ show list2
    putStrLn $ "Merged: " ++ show merged

------------------------------------------------------------
-- HC12T9: Read and Print File Content
------------------------------------------------------------
main9 :: IO ()
main9 = do
    putStrLn "Enter filename:"
    filename <- getLine
    result <- tryReadFile filename
    case result of
        Right content -> do
            putStrLn "File content:"
            putStrLn content
        Left errorMsg -> putStrLn errorMsg
  where
    tryReadFile :: String -> IO (Either String String)
    tryReadFile fname = do
        exists <- doesFileExist fname
        if exists
            then fmap Right (readFile fname)
            else return (Left "File does not exist")
    
    doesFileExist :: String -> IO Bool
    doesFileExist fname = do
        result <- try (readFile fname) :: IO (Either IOException String)
        return $ case result of
            Left _ -> False
            Right _ -> True

------------------------------------------------------------
-- HC12T10: Mathematical Operations Module
------------------------------------------------------------
-- This would typically be in a separate file MathOperations.hs
-- but for simplicity, we'll define it here

module MathOperations where

add :: Num a => a -> a -> a
add = (+)

multiply :: Num a => a -> a -> a
multiply = (*)

power :: (Integral b) => a -> b -> a
power x n = product (replicate n x)

isPrime :: Int -> Bool
isPrime n
    | n <= 1 = False
    | otherwise = null [x | x <- [2..floor (sqrt (fromIntegral n))], n `mod` x == 0]

-- Main function to demonstrate the module
main10 :: IO ()
main10 = do
    putStrLn "Math Operations Demo:"
    putStrLn $ "5 + 3 = " ++ show (MathOperations.add 5 3)
    putStrLn $ "5 * 3 = " ++ show (MathOperations.multiply 5 3)
    putStrLn $ "2 ^ 4 = " ++ show (MathOperations.power (2::Int) 4)
    putStrLn $ "Is 17 prime? " ++ show (MathOperations.isPrime 17)

------------------------------------------------------------
-- Run all examples
------------------------------------------------------------
main :: IO ()
main = do
    putStrLn "=== Chapter 12: Haskell Programs and Modules ==="
    putStrLn "Running HC12T1: Welcome Message"
    main1
    putStrLn "\nRunning HC12T2: Add Two Numbers"
    main2
    putStrLn "\nRunning HC12T3: Factorial Function"
    main3
    putStrLn "\nRunning HC12T4: First 10 Fibonacci Numbers"
    main4
    putStrLn "\nRunning HC12T5: Palindrome Checker"
    main5
    putStrLn "\nRunning HC12T6: Sort a List of Integers"
    main6
    putStrLn "\nRunning HC12T7: Calculate Circle Area"
    main7
    putStrLn "\nRunning HC12T8: Merge Two Sorted Lists"
    main8
    putStrLn "\nRunning HC12T9: Read and Print File Content"
    main9
    putStrLn "\nRunning HC12T10: Mathematical Operations Module"
    main10
