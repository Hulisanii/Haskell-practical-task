
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

-- HC14T2: 
module Main where
import System.Random (randomRIO)

-- HC14T3:
largeNumber1 :: Integer
largeNumber1 = 1_000_000

largeNumber2 :: Double  
largeNumber2 = 3.141_592_653_59

-- HC14T4: 
readStringToInt :: String -> Int
readStringToInt str = read @Int str

-- HC14T5:
data Result a = Success a | Error String

processResult :: Result Int -> String
processResult result = case result of
    Success value@n | n > 100 -> "Large success: " ++ show value
    Success value -> "Success: " ++ show value
    Error msg@message -> "Error occurred: " ++ message
    
-- HC14T8: 
counts :: String -> [(Char, Int)]
counts str = map (\c -> (c, length $ filter (== c) str)) (removeDuplicates str)
  where
    removeDuplicates = foldr (\x acc -> if x `elem` acc then acc else x:acc) []

    
-- HC14T9: PartialTypeSignatures Extension
processList :: _ -> [String]
processList xs = map show xs

withPartialSignature :: _ -> String -> _
withPartialSignature f str = f str

-- HC14T10: Cabal Test Suite
testCounts :: Bool
testCounts = counts "hello" == [('h',1),('e',1),('l',2),('o',1)]

testCountsEmpty :: Bool
testCountsEmpty = counts "" == []

testCountsRepeated :: Bool
testCountsRepeated = counts "aaa" == [('a',3)]

-- Main executable
main :: IO ()
main = do
    putStrLn "HC14T1: Hello, Cabal!"
    
    putStrLn "\nHC14T2: Random Number"
    randomNum <- randomRIO (1, 100) :: IO Int
    putStrLn $ "Random number: " ++ show randomNum
    
    putStrLn "\nHC14T3: Numeric Underscores"
    putStrLn $ "Large number 1: " ++ show largeNumber1
    putStrLn $ "Large number 2: " ++ show largeNumber2
    
    putStrLn "\nHC14T4: Type Applications"
    let number = readStringToInt "42"
    putStrLn $ "Converted number: " ++ show number
    
    putStrLn "\nHC14T5: Pattern Matching with @"
    let result1 = Success 150
        result2 = Success 50
        result3 = Error "Something went wrong"
    putStrLn $ processResult result1
    putStrLn $ processResult result2  
    putStrLn $ processResult result3
    
    putStrLn "\nHC14T8: Character Frequency"
    putStrLn $ "Counts for 'hello': " ++ show (counts "hello")
    putStrLn $ "Counts for 'mississippi': " ++ show (counts "mississippi")
    
    putStrLn "\nHC14T9: Partial Type Signatures"
    putStrLn $ "Process list: " ++ show (processList [1,2,3])
    
    putStrLn "\nHC14T10: Test Results"
    putStrLn $ "Test counts: " ++ show testCounts
    putStrLn $ "Test counts empty: " ++ show testCountsEmpty
    putStrLn $ "Test counts repeated: " ++ show testCountsRepeated





    src/Utils.hs
    module Utils where
greet :: String -> String
greet name = "Hello, " ++ name ++ "!"
addNumbers :: Int -> Int -> Int
addNumbers x y = x + y


cabal-version: 3.0
name:          my-project
version:       0.1.0.0

library
    exposed-modules:     Utils
    build-depends:       base ^>=4.16.0.0
    hs-source-dirs:      src
    default-language:    Haskell2010

executable my-project
    main-is:             Main.hs
    build-depends:       base ^>=4.16.0.0,
                         random
    hs-source-dirs:      app
    default-language:    Haskell2010
    ghc-options:         -Wall

test-suite my-project-test
    type:                exitcode-stdio-1.0
    main-is:             Spec.hs
    build-depends:       base ^>=4.16.0.0,
                         my-project
    hs-source-dirs:      test
    default-language:    Haskell2010


test/Spec.hs
import Main (counts, testCounts, testCountsEmpty, testCountsRepeated)
main :: IO ()
main = do
    putStrLn "Running tests..."
    let tests =
            [ ("testCounts", testCounts)
            , ("testCountsEmpty", testCountsEmpty) 
            , ("testCountsRepeated", testCountsRepeated)
            ]
    let runTest (name, test) = do
            putStr $ name ++ ": "
            if test
                then putStrLn "PASS"
                else putStrLn "FAIL"
    mapM_ runTest tests
    putStrLn "All tests completed!"

    



    
