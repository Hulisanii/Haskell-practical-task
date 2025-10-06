---- by Hulisani Muravha
module Main where
import System.IO (hFlush, stdout)
import Data.Maybe (fromMaybe)
-- 1. greetUser: asks for name and greets
greetUser :: IO ()
greetUser = do
  putStr "Enter your name: "
  hFlush stdout
  name <- getLine -- <- binds result of IO action
  putStrLn $ "Hello, " ++ name ++ "!"

-- 2. readNumbersSum: read a line, parse numbers, print their sum
readNumbersSum :: IO ()
readNumbersSum = do
  putStrLn "Enter space-separated integers:"
  line <- getLine
  let strs = words line
      mayInts = map safeReadInt strs
      ints = map (fromMaybe 0) mayInts
      total = sum ints
  putStrLn $ "Sum (non-parsable -> 0): " ++ show total

-- safe parser to Int with Maybe
safeReadInt :: String -> Maybe Int
safeReadInt s = case reads s of
  [(n,"")] -> Just n
  _        -> Nothing

-- 3. simple file write/read demo (writes and reads a temporary file)
fileDemo :: IO ()
fileDemo = do
  let fname = "chapter9_demo.txt"
  writeFile fname "Hello from Chapter 9!\nLine 2"
  putStrLn $ "Wrote to " ++ fname ++ ", reading it back:"
  contents <- readFile fname
  putStrLn contents

-- 4. using Maybe and Either in pure functions
safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

divideEither :: (Eq a, Fractional a) => a -> a -> Either String a
divideEither _ 0 = Left "Division by zero"
divideEither x y = Right (x / y)

-- main: small interactive menu demonstrating the above
main :: IO ()
main = do
  putStrLn "Chapter 9: IO & simple monads demo"
  putStrLn "1) Greet"
  putStrLn "2) Sum numbers from a line"
  putStrLn "3) File write/read demo"
  putStrLn "4) Demo safeHead / divideEither"
  putStr "Choose option (1-4): "
  hFlush stdout
  opt <- getLine
  case opt of
    "1" -> greetUser
    "2" -> readNumbersSum
    "3" -> fileDemo
    "4" -> do
      putStrLn $ "safeHead [1,2,3] -> " ++ show (safeHead [1,2,3])
      putStrLn $ "safeHead [] -> " ++ show (safeHead ([] :: [Int]))
      putStrLn $ "divideEither 10 2 -> " ++ show (divideEither 10 2)
      putStrLn $ "divideEither 10 0 -> " ++ show (divideEither 10 0)
    _ -> putStrLn "Invalid option - exiting."

