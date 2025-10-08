--By Hulisani Muravha 
module Main where
-- HC3T1 - Task 1: Check if a number is positive, negative, or zero
checkNumber :: Int -> String
checkNumber n =
    if n > 0 then "Positive"
    else if n < 0 then "Negative"
    else "Zero"
-- HC3T2 - Task 2: Determine grade based on score using guards
grade :: Int -> String
grade score
    | score >= 90 = "A"
    | score >= 80 = "B"
    | score >= 70 = "C"
    | score >= 60 = "D"
    | otherwise   = "F"
-- HC3T3 - Task 3: Convert RGB color to hex string using let bindings
rgbToHex :: (Int, Int, Int) -> String
rgbToHex (r, g, b) =
    let toHex n = let hex = showHex n ""
                  in if length hex == 1 then '0':hex else hex
    in "#" ++ toHex r ++ toHex g ++ toHex b
showHex :: Int -> String -> String
showHex 0 acc = if null acc then "0" else acc
showHex n acc =
    let digit = case n `mod` 16 of
                 10 -> 'a'
                 11 -> 'b'
                 12 -> 'c'
                 13 -> 'd'
                 14 -> 'e'
                 15 -> 'f'
                 x  -> head (show x)
    in showHex (n `div` 16) (digit:acc)
-- HC3T4 - Task 4: Calculate triangle area using Heron's formula
triangleArea :: Float -> Float -> Float -> Float
triangleArea a b c =
    let s = (a + b + c) / 2  -- semi-perimeter
    in sqrt (s * (s - a) * (s - b) * (s - c))
-- HC3T5 - Task 5: Determine triangle type using guards
triangleType :: Float -> Float -> Float -> String
triangleType a b c
    | a == b && b == c           = "Equilateral"
    | a == b || b == c || a == c = "Isosceles"
    | otherwise                  = "Scalene"

-- HC3T6 - Advanced Task 6: Check leap year using if-then-else
isLeapYear :: Int -> Bool
isLeapYear year =
    if year `mod` 400 == 0 then True
    else if year `mod` 100 == 0 then False
    else if year `mod` 4 == 0 then True
    else False
-- Using guards with multiple conditions
season :: Int -> String
season month
    | month == 12 || month == 1 || month == 2  = "Winter"
    | month >= 3 && month <= 5                 = "Spring"
    | month >= 6 && month <= 8                 = "Summer"
    | month >= 9 && month <= 11                = "Autumn"
    | otherwise                                = "Invalid month"
-- HC3T8 - Advanced Task 8: Calculate BMI and return category using where
bmiCategory :: Float -> Float -> String
bmiCategory weight height
    | bmi < 18.5  = "Underweight"
    | bmi < 25    = "Normal"
    | bmi < 30    = "Overweight"
    | otherwise   = "Obese"
    where bmi = weight / (height * height)
-- HC3T9 - Advanced Task 9: Find maximum of three numbers using let
-- Using let for intermediate values
maxOfThree :: Int -> Int -> Int -> Int
maxOfThree x y z =
    let maxXY = max x y
    in max maxXY z
    
-- HC3T10 - Advanced Task 10: Check if string is palindrome using recursion and guards

isPalindrome :: String -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome str
    | head str == last str = isPalindrome (init (tail str))
    | otherwise = False
isPalindrome' :: String -> Bool
isPalindrome' str = str == reverse str


-- DEMONSTRATION AND TESTING

main :: IO ()
main = do
    putStrLn "HC3T1:"
    putStrLn $ "checkNumber 5 = " ++ checkNumber 5
    putStrLn $ "checkNumber (-3) = " ++ checkNumber (-3)
    putStrLn $ "checkNumber 0 = " ++ checkNumber 0
    putStrLn ""
    
    putStrLn "HC3T2:"
    putStrLn $ "grade 95 = " ++ grade 95
    putStrLn $ "grade 72 = " ++ grade 72
    putStrLn $ "grade 50 = " ++ grade 50
    putStrLn ""
    
    putStrLn "HC3T3:"
    putStrLn $ "rgbToHex (255, 0, 127) = " ++ rgbToHex (255, 0, 127)
    putStrLn $ "rgbToHex (0, 255, 64) = " ++ rgbToHex (0, 255, 64)
    putStrLn ""
    
    putStrLn "HC3T4:"
    putStrLn $ "triangleArea 3 4 5 = " ++ show (triangleArea 3 4 5)
    putStrLn $ "triangleArea 7 8 9 = " ++ show (triangleArea 7 8 9)
    putStrLn ""
    
    putStrLn "HC3T5:"
    putStrLn $ "triangleType 3 3 3 = " ++ triangleType 3 3 3
    putStrLn $ "triangleType 5 5 8 = " ++ triangleType 5 5 8
    putStrLn $ "triangleType 6 7 8 = " ++ triangleType 6 7 8
    putStrLn ""
    
    putStrLn "HC3T6:"
    putStrLn $ "isLeapYear 2000 = " ++ show (isLeapYear 2000)
    putStrLn $ "isLeapYear 1900 = " ++ show (isLeapYear 1900)
    putStrLn $ "isLeapYear 2024 = " ++ show (isLeapYear 2024)
    putStrLn $ "isLeapYear 2023 = " ++ show (isLeapYear 2023)
    putStrLn ""
    
    putStrLn "HC3T7:"
    putStrLn $ "season 3 = " ++ season 3
    putStrLn $ "season 7 = " ++ season 7
    putStrLn $ "season 11 = " ++ season 11
    putStrLn $ "season 13 = " ++ season 13
    putStrLn ""
    
    putStrLn "HC3T8:"
    putStrLn $ "bmiCategory 70 1.75 = " ++ bmiCategory 70 1.75
    putStrLn $ "bmiCategory 90 1.8 = " ++ bmiCategory 90 1.8
    putStrLn $ "bmiCategory 50 1.65 = " ++ bmiCategory 50 1.65
    putStrLn $ "bmiCategory 100 1.7 = " ++ bmiCategory 100 1.7
    putStrLn ""
    
    putStrLn "HC3T9:"
    putStrLn $ "maxOfThree 10 20 15 = " ++ show (maxOfThree 10 20 15)
    putStrLn $ "maxOfThree 5 25 10 = " ++ show (maxOfThree 5 25 10)
    putStrLn $ "maxOfThree 8 8 12 = " ++ show (maxOfThree 8 8 12)
    putStrLn ""
    
    putStrLn "HC3T10:"
    putStrLn $ "isPalindrome \"racecar\" = " ++ show (isPalindrome "racecar")
    putStrLn $ "isPalindrome \"haskell\" = " ++ show (isPalindrome "haskell")
    putStrLn $ "isPalindrome \"madam\" = " ++ show (isPalindrome "madam")
    putStrLn $ "isPalindrome' \"racecar\" = " ++ show (isPalindrome' "racecar")
    putStrLn ""

  
