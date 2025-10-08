
---By: Hulisani Muravha
--HC4T1 to HC4T8 Solutions

module Main where
-- HC4T1 - Task 1: Define a weatherReport Function

weatherReport :: String -> String
weatherReport "sunny"  = "It's a bright and beautiful day!"
weatherReport "rainy"  = "Don't forget your umbrella!"
weatherReport "cloudy" = "A bit gloomy, but no rain yet!"
weatherReport _        = "Weather unknown"

-- HC4T2 - Task 2: Define a dayType Function  
-- Description: Determines if a day is weekday or weekend using pattern matching

dayType :: String -> String
dayType "Saturday" = "It's a weekend!"
dayType "Sunday"   = "It's a weekend!"
dayType "Monday"   = "It's a weekday."
dayType "Tuesday"  = "It's a weekday."
dayType "Wednesday"= "It's a weekday."
dayType "Thursday" = "It's a weekday."
dayType "Friday"   = "It's a weekday."
dayType _          = "Invalid day"


-- HC4T3 - Task 3: Define a gradeComment Function
-- Description: Returns comment based on grade range using pattern matching
gradeComment :: Int -> String
gradeComment grade
  | grade >= 90 && grade <= 100 = "Excellent!"
  | grade >= 70 && grade <= 89  = "Good job!"
  | grade >= 50 && grade <= 69  = "You passed."
  | grade >= 0  && grade <= 49  = "Better luck next time."
  | otherwise                   = "Invalid grade"


-- HC4T4 - Task 4: Rewrite specialBirthday using Pattern Matching

specialBirthday :: Int -> String
specialBirthday 1  = "First birthday! How exciting!"
specialBirthday 16 = "Sweet sixteen!"
specialBirthday 18 = "You're an adult now!"
specialBirthday 21 = "Legal drinking age in some countries!"
specialBirthday 50 = "Half a century!"
specialBirthday _  = "Just another birthday."

-- HC4T5 - Task 5: Add a Catch-All Pattern with a Custom Message

specialBirthdayV2 :: Int -> String
specialBirthdayV2 1  = "First birthday! How exciting!"
specialBirthdayV2 16 = "Sweet sixteen!"
specialBirthdayV2 18 = "You're an adult now!"
specialBirthdayV2 21 = "Legal drinking age in some countries!"
specialBirthdayV2 50 = "Half a century!"
specialBirthdayV2 age = "Happy " ++ show age ++ "th birthday!"

-- HC4T6 - Task 6: Identify List Contents Using Pattern Matching

whatsInsideThisList :: [a] -> String
whatsInsideThisList []     = "This list is empty"
whatsInsideThisList [_]    = "This list has exactly one element"
whatsInsideThisList [_,_]  = "This list has exactly two elements" 
whatsInsideThisList [_,_,_]= "This list has exactly three elements"
whatsInsideThisList _      = "This list has more than three elements"


-- HC4T7 - Task 7: Ignore Elements in a List
-- Description: Returns only first and third elements, ignoring others

firstAndThirdSafe :: [a] -> Maybe (a, a)
firstAndThirdSafe (x:_:z:_) = Just (x, z)
firstAndThirdSafe _          = Nothing


-- HC4T8 - Task 8: Extract Values from Tuples

describeTuple :: (String, Int, Bool) -> String
describeTuple (name, age, isStudent) =
  "Name: " ++ name ++ ", Age: " ++ show age ++ ", Student: " ++ show isStudent

describePair :: (String, String) -> String
describePair (first, second) =
  "First: " ++ first ++ ", Second: " ++ second

-- DEMONSTRATION AND TESTING
main :: IO ()
main = do
  putStrLn "=== Haskell Pattern Matching Tasks ==="
  putStrLn "By: Hulisani Muravha"
  putStrLn ""
  
  putStrLn "HC4T1: Weather Report"
  putStrLn $ "  sunny:  " ++ weatherReport "sunny"
  putStrLn $ "  rainy:  " ++ weatherReport "rainy" 
  putStrLn $ "  cloudy: " ++ weatherReport "cloudy"
  putStrLn $ "  snowy:  " ++ weatherReport "snowy"
  putStrLn ""
  
  putStrLn "HC4T2: Day Type"
  putStrLn $ "  Saturday: " ++ dayType "Saturday"
  putStrLn $ "  Monday:   " ++ dayType "Monday"
  putStrLn $ "  Funday:   " ++ dayType "Funday"
  putStrLn ""
  
  putStrLn "HC4T3: Grade Comment"
  putStrLn $ "  95: " ++ gradeComment 95
  putStrLn $ "  75: " ++ gradeComment 75
  putStrLn $ "  55: " ++ gradeComment 55
  putStrLn $ "  35: " ++ gradeComment 35
  putStrLn $ "  -5: " ++ gradeComment (-5)
  putStrLn ""
  
  putStrLn "HC4T4: Special Birthday (Pattern Matching)"
  putStrLn $ "  1:  " ++ specialBirthday 1
  putStrLn $ "  16: " ++ specialBirthday 16
  putStrLn $ "  21: " ++ specialBirthday 21
  putStrLn $ "  25: " ++ specialBirthday 25
  putStrLn ""
  
  putStrLn "HC4T5: Special Birthday V2 (With Age in Message)"
  putStrLn $ "  1:  " ++ specialBirthdayV2 1
  putStrLn $ "  16: " ++ specialBirthdayV2 16
  putStrLn $ "  25: " ++ specialBirthdayV2 25
  putStrLn $ "  30: " ++ specialBirthdayV2 30
  putStrLn ""
  
  putStrLn "HC4T6: What's Inside This List?"
  putStrLn $ "  []:          " ++ whatsInsideThisList ([] :: [Int])
  putStrLn $ "  [1]:         " ++ whatsInsideThisList [1]
  putStrLn $ "  [1,2]:       " ++ whatsInsideThisList [1,2]
  putStrLn $ "  [1,2,3]:     " ++ whatsInsideThisList [1,2,3]
  putStrLn $ "  [1,2,3,4]:   " ++ whatsInsideThisList [1,2,3,4]
  putStrLn ""
  
  putStrLn "HC4T7: First and Third Elements"
  putStrLn $ "  [1,2,3,4,5]: " ++ show (firstAndThirdSafe [1,2,3,4,5])
  putStrLn $ "  [10,20,30]:  " ++ show (firstAndThirdSafe [10,20,30])
  putStrLn $ "  [1,2]:       " ++ show (firstAndThirdSafe [1,2])
  putStrLn ""
  
  putStrLn "HC4T8: Describe Tuple"
  putStrLn $ "  Tuple 1: " ++ describeTuple ("Alice", 20, True)
  putStrLn $ "  Tuple 2: " ++ describeTuple ("Bob", 25, False)
  putStrLn $ "  Pair:    " ++ describePair ("Hello", "World")
  putStrLn ""
  
  putStrLn "=== All Pattern Matching Tasks Completed! ==="
