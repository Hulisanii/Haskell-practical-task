import Data.Char (toUpper)
import Data.List (sort, group)

-- HC16T1: Reverse a String
reverseString :: String -> String
reverseString = reverse

-- HC16T2: Palindrome Checker
isPalindrome :: String -> Bool
isPalindrome str = str == reverse str

-- HC16T3: Factorial
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- HC16T4: Filter Even Numbers
filterEven :: [Int] -> [Int]
filterEven = filter even

-- HC16T5: Uppercase String
uppercaseString :: String -> String
uppercaseString = map toUpper

-- HC16T6: nth Fibonacci Number
fibonacci :: Int -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

-- More efficient Fibonacci using memoization
fibonacciFast :: Int -> Integer
fibonacciFast n = fibs !! n
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- HC16T7: Element Existence in List
elementExists :: Eq a => a -> [a] -> Bool
elementExists _ [] = False
elementExists x (y:ys)
    | x == y    = True
    | otherwise = elementExists x ys

-- Using built-in elem
elementExists' :: Eq a => a -> [a] -> Bool
elementExists' = elem

-- HC16T8: Insertion Sort
insertionSort :: Ord a => [a] -> [a]
insertionSort [] = []
insertionSort (x:xs) = insert x (insertionSort xs)
  where
    insert x [] = [x]
    insert x (y:ys)
        | x <= y    = x : y : ys
        | otherwise = y : insert x ys

-- HC16T9: Remove Duplicates from List
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs) = x : removeDuplicates (filter (/= x) xs)

-- Using nub from Data.List (more efficient)
removeDuplicates' :: Eq a => [a] -> [a]
removeDuplicates' = foldr (\x acc -> if x `elem` acc then acc else x : acc) []

-- HC16T10: Character Frequency in String
charFrequency :: String -> [(Char, Int)]
charFrequency str = map (\xs -> (head xs, length xs)) $ group $ sort str

-- Alternative implementation without group
charFrequency' :: String -> [(Char, Int)]
charFrequency' str = foldr countChar [] str
  where
    countChar c [] = [(c, 1)]
    countChar c ((char, count):rest)
        | c == char = (char, count + 1) : rest
        | otherwise = (char, count) : countChar c rest

-- Main function to test all implementations
main :: IO ()
main = do
    putStrLn "=== HC16T1: Reverse a String ==="
    putStrLn $ "reverse 'hello' = " ++ show (reverseString "hello")
    putStrLn $ "reverse 'Haskell' = " ++ show (reverseString "Haskell")
    
    putStrLn "\n=== HC16T2: Palindrome Checker ==="
    putStrLn $ "isPalindrome 'racecar' = " ++ show (isPalindrome "racecar")
    putStrLn $ "isPalindrome 'hello' = " ++ show (isPalindrome "hello")
    putStrLn $ "isPalindrome 'madam' = " ++ show (isPalindrome "madam")
    
    putStrLn "\n=== HC16T3: Factorial ==="
    putStrLn $ "factorial 5 = " ++ show (factorial 5)
    putStrLn $ "factorial 7 = " ++ show (factorial 7)
    putStrLn $ "factorial 0 = " ++ show (factorial 0)
    
    putStrLn "\n=== HC16T4: Filter Even Numbers ==="
    putStrLn $ "filterEven [1,2,3,4,5,6,7,8,9,10] = " ++ show (filterEven [1,2,3,4,5,6,7,8,9,10])
    putStrLn $ "filterEven [11,13,15] = " ++ show (filterEven [11,13,15])
    
    putStrLn "\n=== HC16T5: Uppercase String ==="
    putStrLn $ "uppercaseString 'hello world' = " ++ show (uppercaseString "hello world")
    putStrLn $ "uppercaseString 'Haskell Programming' = " ++ show (uppercaseString "Haskell Programming")
    
    putStrLn "\n=== HC16T6: nth Fibonacci Number ==="
    putStrLn "First 15 Fibonacci numbers (slow version):"
    mapM_ (\n -> putStrLn $ "fibonacci " ++ show n ++ " = " ++ show (fibonacci n)) [0..10]
    
    putStrLn "\nFirst 15 Fibonacci numbers (fast version):"
    mapM_ (\n -> putStrLn $ "fibonacciFast " ++ show n ++ " = " ++ show (fibonacciFast n)) [0..10]
    
    putStrLn "\n=== HC16T7: Element Existence in List ==="
    putStrLn $ "elementExists 5 [1,2,3,4,5] = " ++ show (elementExists 5 [1,2,3,4,5])
    putStrLn $ "elementExists 10 [1,2,3,4,5] = " ++ show (elementExists 10 [1,2,3,4,5])
    putStrLn $ "elementExists 'a' \"Haskell\" = " ++ show (elementExists 'a' "Haskell")
    putStrLn $ "elementExists 'x' \"Haskell\" = " ++ show (elementExists 'x' "Haskell")
    
    putStrLn "\n=== HC16T8: Insertion Sort ==="
    putStrLn $ "insertionSort [5,2,8,1,9,3] = " ++ show (insertionSort [5,2,8,1,9,3])
    putStrLn $ "insertionSort [9,8,7,6,5,4,3,2,1] = " ++ show (insertionSort [9,8,7,6,5,4,3,2,1])
    putStrLn $ "insertionSort ['z','a','m','b'] = " ++ show (insertionSort "zamb")
    
    putStrLn "\n=== HC16T9: Remove Duplicates from List ==="
    putStrLn $ "removeDuplicates [1,2,2,3,4,4,4,5] = " ++ show (removeDuplicates [1,2,2,3,4,4,4,5])
    putStrLn $ "removeDuplicates \"Hello World\" = " ++ show (removeDuplicates "Hello World")
    putStrLn $ "removeDuplicates' [1,1,2,2,3,3,4,4] = " ++ show (removeDuplicates' [1,1,2,2,3,3,4,4])
    
    putStrLn "\n=== HC16T10: Character Frequency in String ==="
    putStrLn $ "charFrequency \"hello\" = " ++ show (charFrequency "hello")
    putStrLn $ "charFrequency \"programming\" = " ++ show (charFrequency "programming")
    putStrLn $ "charFrequency \"Haskell\" = " ++ show (charFrequency "Haskell")
    putStrLn $ "charFrequency' \"mississippi\" = " ++ show (charFrequency' "mississippi")
    
    -- Additional comprehensive tests
    putStrLn "\n=== Additional Tests ==="
    
    -- Test palindrome with different cases
    putStrLn $ "isPalindrome 'A man a plan a canal Panama' (ignoring case and spaces) = " ++ 
               show (isPalindrome (filter (/= ' ') (map toUpper "A man a plan a canal Panama")))
    
    -- Test with larger numbers
    putStrLn $ "factorial 10 = " ++ show (factorial 10)
    putStrLn $ "fibonacciFast 20 = " ++ show (fibonacciFast 20)
    
    -- Performance comparison
    putStrLn "\nPerformance comparison for Fibonacci:"
    putStrLn "Calculating fibonacciFast 30 (instant)..."
    putStrLn $ "Result: " ++ show (fibonacciFast 30)
    
    putStrLn "Calculating fibonacci 30 (slower)..."
    putStrLn $ "Result: " ++ show (fibonacci 30)

-- Helper function for interactive testing
interactiveTest :: IO ()
interactiveTest = do
    putStrLn "\n=== Interactive Testing ==="
    
    putStr "Enter a string to reverse: "
    str <- getLine
    putStrLn $ "Reversed: " ++ reverseString str
    
    putStr "Enter a string to check for palindrome: "
    pal <- getLine
    putStrLn $ "Is palindrome: " ++ show (isPalindrome pal)
    
    putStr "Enter a number for factorial: "
    numStr <- getLine
    case readMaybe numStr of
        Just num -> putStrLn $ "Factorial: " ++ show (factorial num)
        Nothing -> putStrLn "Invalid number"
    
    putStr "Enter numbers separated by spaces to filter evens: "
    numsStr <- getLine
    let nums = map read (words numsStr) :: [Int]
    putStrLn $ "Even numbers: " ++ show (filterEven nums)

-- Utility function for safe reading
readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
    [(x, "")] -> Just x
    _ -> Nothing
