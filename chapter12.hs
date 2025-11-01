-- By Hulisani Muravha
module Main where

-- HC12T1 to HC12T10
-- A single file demonstrating all tasks HC12T1..HC12T10

import System.IO
import System.Directory (doesFileExist)
import Control.Exception (catch, IOException)
import Data.Char (toLower)
import qualified MathOperations as MathOps

-- HC12T1: Print a Welcome Message
printWelcome :: IO ()
printWelcome = putStrLn "Welcome to Haskell Programming!"

-- HC12T2: Add Two Numbers
addTwoNumbers :: Int -> Int -> Int
addTwoNumbers x y = x + y

-- HC12T3: Factorial Function (recursive)
factorial :: Int -> Int
factorial 0 = 1
factorial n
    | n < 0     = error "factorial: negative input"
    | otherwise = n * factorial (n - 1)

-- HC12T4: First 10 Fibonacci Numbers (recursive)
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

firstTenFibonacci :: [Int]
firstTenFibonacci = [fibonacci n | n <- [0..9]]

-- HC12T5: Palindrome Checker (uses user input in main)
isPalindrome :: String -> Bool
isPalindrome str =
    let cleaned = map toLower (filter (/= ' ') str)
    in cleaned == reverse cleaned

-- HC12T6: Sort a List of Integers (quick sort)
sortList :: [Int] -> [Int]
sortList []     = []
sortList (p:xs) = sortList lesser ++ [p] ++ sortList greater
  where
    lesser  = [x | x <- xs, x <= p]
    greater = [x | x <- xs, x >  p]

-- HC12T7: Calculate Circle Area
calculateCircleArea :: Floating a => a -> a
calculateCircleArea r
    | r < 0     = error "Radius cannot be negative"
    | otherwise = pi * r * r

-- HC12T8: Merge Two Sorted Lists
mergeLists :: Ord a => [a] -> [a] -> [a]
mergeLists xs [] = xs
mergeLists [] ys = ys
mergeLists (x:xs) (y:ys)
    | x <= y    = x : mergeLists xs (y:ys)
    | otherwise = y : mergeLists (x:xs) ys

-- HC12T9: Read and Print File Content (graceful error handling)
readAndPrintFile :: FilePath -> IO ()
readAndPrintFile path = do
    exists <- doesFileExist path
    if exists
      then do
        -- catch any IO exception while reading and print a message
        content <- readFile path `catch` handler
        putStrLn "----- File Content -----"
        putStrLn content
      else putStrLn ("Error: File does not exist: " ++ path)
  where
    handler :: IOException -> IO String
    handler e = return ("Error reading file: " ++ show e)

-- HC12T10: Mathematical Operations Module (see MathOperations.hs)

-- MAIN FUNCTION: Demonstration and interactive parts
main :: IO ()
main = do
    putStrLn "----- HC12T1: Welcome Message -----"
    printWelcome

    putStrLn "\n----- HC12T2: Add Two Numbers -----"
    putStrLn $ "addTwoNumbers 10 15 = " ++ show (addTwoNumbers 10 15)

    putStrLn "\n----- HC12T3: Factorial Function -----"
    putStrLn $ "factorial 5 = " ++ show (factorial 5)

    putStrLn "\n----- HC12T4: First 10 Fibonacci Numbers -----"
    putStrLn $ show firstTenFibonacci

    putStrLn "\n----- HC12T5: Palindrome Checker (user input) -----"
    putStrLn "Enter a string to check for palindrome (example: Madam):"
    hFlush stdout
    palInput <- getLine
    putStrLn $ "Is \"" ++ palInput ++ "\" a palindrome? " ++ show (isPalindrome palInput)

    putStrLn "\n----- HC12T6: Sort a List of Integers -----"
    putStrLn "Sorting example list [5,2,8,1,3]:"
    putStrLn $ show (sortList [5,2,8,1,3])

    putStrLn "\n----- HC12T7: Circle Area -----"
    putStrLn $ "Area for radius 5 = " ++ show (calculateCircleArea 5)

    putStrLn "\n----- HC12T8: Merge Two Sorted Lists -----"
    putStrLn $ "mergeLists [1,3,5] [2,4,6] = " ++ show (mergeLists [1,3,5] [2,4,6])

    putStrLn "\n----- HC12T9: Read and Print File Content -----"
    putStrLn "Enter file name to read (or press Enter to skip):"
    hFlush stdout
    fileName <- getLine
    if null fileName
      then putStrLn "Skipping file read."
      else readAndPrintFile fileName

    putStrLn "\n----- HC12T10: Using MathOperations Module -----"
    putStrLn $ "MathOps.add 5 3 = " ++ show (MathOps.add 5 3)
    putStrLn $ "MathOps.subtractNum 10 4 = " ++ show (MathOps.subtractNum 10 4)
    putStrLn $ "MathOps.multiply 6 7 = " ++ show (MathOps.multiply 6 7)
    putStrLn $ "MathOps.divide 20 4 = " ++ show (MathOps.divide 20 4)

    putStrLn "\nAll tasks demonstrated. Program complete."


MathOperations
-- By Hulisani Muravha
-- HC12T10: Mathematical Operations Module

module MathOperations (add, subtractNum, multiply, divide) where

add :: Num a => a -> a -> a
add x y = x + y

subtractNum :: Num a => a -> a -> a
subtractNum x y = x - y

multiply :: Num a => a -> a -> a
multiply x y = x * y

divide :: Fractional a => a -> a -> a
divide _ 0 = error "Division by zero"
divide x y = x / y

    
