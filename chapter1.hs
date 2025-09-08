import Data.List (sortOn)

-- HC1T1 - Task 1: Function Composition
double :: Num a => a -> a
double x = x * 2

increment :: Num a => a -> a
increment x = x + 1

doubleThenIncrement :: Num a => a -> a
doubleThenIncrement = increment . double

-- HC1T2 - Task 2: Pure Function Example
circleArea :: Floating a => a -> a
circleArea radius = pi * radius * radius

-- HC1T3 - Task 3: Checking if a Number is Greater than 18
greaterThan18 :: (Ord a, Num a) => a -> Bool
greaterThan18 x = x > 18

-- HC1T4 - Task 4: Composing a Function to Process Player Data
type Player = (String, Int)

extractPlayers :: [Player] -> [String]
extractPlayers players = map fst players

sortByScore :: [Player] -> [Player]
sortByScore players = reverse $ sortOn snd players

topThree :: [Player] -> [Player]
topThree players = take 3 players

getTopThreePlayers :: [Player] -> [Player]
getTopThreePlayers = topThree . sortByScore

-- HC1T5 - Task 5: Laziness in Haskell
infiniteNumbers :: [Int]
infiniteNumbers = [1..]

getFirstN :: Int -> [Int]
getFirstN n = take n infiniteNumbers

-- HC1T6 - Task 6: Using Type Signatures
addNumbers :: Int -> Int -> Int
addNumbers x y = x + y

-- HC1T7 - Task 7: Converting Fahrenheit to Celsius
fToC :: Double -> Double
fToC f = (f - 32) * 5 / 9

-- HC1T8 - Task 8: Higher-Order Functions
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- Main function to demonstrate all tasks
main :: IO ()
main = do
    putStrLn "=== Haskell Practical Tasks ==="
    
    -- Task 1
    putStrLn "\n1. Function Composition:"
    putStrLn $ "double 5: " ++ show (double 5)
    putStrLn $ "increment 10: " ++ show (increment 10)
    putStrLn $ "doubleThenIncrement 5: " ++ show (doubleThenIncrement 5)
    
    -- Task 2
    putStrLn "\n2. Circle Area:"
    putStrLn $ "circleArea 3.0: " ++ show (circleArea 3.0)
    
    -- Task 3
    putStrLn "\n3. Greater than 18:"
    putStrLn $ "greaterThan18 20: " ++ show (greaterThan18 20)
    putStrLn $ "greaterThan18 15: " ++ show (greaterThan18 15)
    
    -- Task 4
    putStrLn "\n4. Player Data Processing:"
    let players = [("Alice", 85), ("Bob", 92), ("Charlie", 78), ("David", 95), ("Eve", 88)]
    putStrLn $ "Original players: " ++ show players
    putStrLn $ "Top three players: " ++ show (getTopThreePlayers players)
    putStrLn $ "Player names: " ++ show (extractPlayers players)
    
    -- Task 5
    putStrLn "\n5. Laziness:"
    putStrLn $ "First 10 numbers: " ++ show (getFirstN 10)
    
    -- Task 6
    putStrLn "\n6. Add Numbers:"
    putStrLn $ "addNumbers 7 3: " ++ show (addNumbers 7 3)
    
    -- Task 7
    putStrLn "\n7. Fahrenheit to Celsius:"
    putStrLn $ "32°F = " ++ show (fToC 32) ++ "°C"
    putStrLn $ "212°F = " ++ show (fToC 212) ++ "°C"
    
    -- Task 8
    putStrLn "\n8. Apply Twice:"
    putStrLn $ "applyTwice double 2: " ++ show (applyTwice double 2)
    putStrLn $ "applyTwice increment 5: " ++ show (applyTwice increment 5)
    
    putStrLn "\n=== All tasks completed! ==="
