--By Hulisani Muravha 
module Main where
-- HC7T1: Implement an Eq Instance for a Custom Data Type

data Color = Red | Green | Blue
    deriving (Show)  
instance Eq Color where
    Red   == Red   = True
    Green == Green = True
    Blue  == Blue  = True
    _     == _     = False
    
-- HC7T2: Implement an Ord Instance for a Custom Data Type
instance Ord Color where
    Red   `compare` Red   = EQ
    Red   `compare` Green = LT
    Red   `compare` Blue  = LT
    Green `compare` Red   = GT
    Green `compare` Green = EQ
    Green `compare` Blue  = LT
    Blue  `compare` Red   = GT
    Blue  `compare` Green = GT
    Blue  `compare` Blue  = EQ

-- HC7T3: Function Using Multiple Constraints
compareValues :: (Eq a, Ord a) => a -> a -> a
compareValues x y
    | x == y    = x  -- If equal, return first (arbitrary choice)
    | x > y     = x
    | otherwise = y

-- HC7T4: Custom Type with Show and Read
data Shape = Circle Double | Rectangle Double Double

-- Implement Show instance for Shape
instance Show Shape where
    show (Circle r) = "Circle " ++ show r
    show (Rectangle w h) = "Rectangle " ++ show w ++ " " ++ show h

-- Implement Read instance for Shape
instance Read Shape where
    readsPrec _ input =
        case words input of
            ["Circle", r] -> [(Circle (read r), "")]
            ["Rectangle", w, h] -> [(Rectangle (read w) (read h), "")]
            _ -> []
            
-- HC7T5: Function with Num Constraint
squareArea :: Num a => a -> a
squareArea side = side * side

-- HC7T6: Using Integral and Floating Type Classes
circleCircumference :: (Floating a) => a -> a
circleCircumference radius = 2 * pi * radius
circleCircumference' :: (Integral a, Floating b) => a -> b
circleCircumference' radius = 2 * pi * fromIntegral radius

-- HC7T7: Bounded and Enum
instance Bounded Color where
    minBound = Red
    maxBound = Blue

instance Enum Color where
    fromEnum Red   = 0
    fromEnum Green = 1
    fromEnum Blue  = 2
    
    toEnum 0 = Red
    toEnum 1 = Green
    toEnum 2 = Blue
    toEnum _ = error "Color value out of bounds"
nextColor :: Color -> Color
nextColor Blue = Red  -- Wrap around from last to first
nextColor color = succ color

-- HC7T8: Parse a Value from a String Using Read
parseShape :: String -> Maybe Shape
parseShape input = case reads input of
    [(shape, "")] -> Just shape
    _             -> Nothing
    
-- HC7T9: Type Class with Multiple Instances
class Describable a where
    describe :: a -> String
instance Describable Bool where
    describe True  = "This is True"
    describe False = "This is False"
instance Describable Shape where
    describe (Circle r) = "A circle with radius " ++ show r
    describe (Rectangle w h) = "A rectangle with width " ++ show w ++ " and height " ++ show h
-- HC7T10: 
describeAndCompare :: (Describable a, Ord a) => a -> a -> String
describeAndCompare x y
    | x > y     = "The larger value is: " ++ describe x
    | y > x     = "The larger value is: " ++ describe y
    | otherwise = "The values are equal: " ++ describe x
describeAndCompare' :: (Describable a, Eq a) => a -> a -> String
describeAndCompare' x y
    | x == y    = "The values are equal: " ++ describe x
    | otherwise = "First: " ++ describe x ++ ", Second: " ++ describe y

-- Make Color Describable for HC7T10 demonstration
instance Describable Color where
    describe Red   = "The color Red"
    describe Green = "The color Green" 
    describe Blue  = "The color Blue"
instance Describable Int where
    describe n = "The number " ++ show n
allColors :: [Color]
allColors = [minBound .. maxBound]


--DEMONSTRATION AND TESTING

main :: IO ()
main = do
    putStrLn "HC7T1:"
    putStrLn $ "Red == Red: " ++ show (Red == Red)
    putStrLn $ "Red == Green: " ++ show (Red == Green)
    putStrLn $ "Green == Blue: " ++ show (Green == Blue)
    putStrLn ""
    
    putStrLn "HC7T2:"
    putStrLn $ "Red < Green: " ++ show (Red < Green)
    putStrLn $ "Green < Blue: " ++ show (Green < Blue)
    putStrLn $ "Blue > Red: " ++ show (Blue > Red)
    putStrLn ""
    
    putStrLn "HC7T3:"
    putStrLn $ "compareValues 5 10: " ++ show (compareValues (5 :: Int) 10)
    putStrLn $ "compareValues 'a' 'z': " ++ show (compareValues 'a' 'z')
    putStrLn $ "compareValues Red Blue: " ++ show (compareValues Red Blue)
    putStrLn ""
    
    putStrLn "HC7T4:"
    let circle = Circle 5.0
        rect = Rectangle 4.0 6.0
    putStrLn $ "Circle 5.0: " ++ show circle
    putStrLn $ "Rectangle 4.0 6.0: " ++ show rect
    putStrLn $ "Read \"Circle 3.0\": " ++ show (read "Circle 3.0" :: Shape)
    putStrLn ""
    
    putStrLn "HC7T5:"
    putStrLn $ "squareArea 5 (Int): " ++ show (squareArea (5 :: Int))
    putStrLn $ "squareArea 2.5 (Double): " ++ show (squareArea (2.5 :: Double))
    putStrLn ""
    
    putStrLn "HC7T6:"
    putStrLn $ "circleCircumference 5.0: " ++ show (circleCircumference 5.0)
    putStrLn $ "circleCircumference' 5: " ++ show (circleCircumference' (5 :: Int))
    putStrLn ""
    
    putStrLn "HC7T7:"
    putStrLn $ "nextColor Red: " ++ show (nextColor Red)
    putStrLn $ "nextColor Green: " ++ show (nextColor Green)
    putStrLn $ "nextColor Blue: " ++ show (nextColor Blue)
    putStrLn $ "All colors: " ++ show allColors
    putStrLn ""
    
    putStrLn "HC7T8:"
    putStrLn $ "parseShape \"Circle 5.0\": " ++ show (parseShape "Circle 5.0")
    putStrLn $ "parseShape \"Rectangle 3.0 4.0\": " ++ show (parseShape "Rectangle 3.0 4.0")
    putStrLn $ "parseShape \"Invalid shape\": " ++ show (parseShape "Invalid shape")
    putStrLn ""
    
    putStrLn "HC7T9:"
    putStrLn $ "describe True: " ++ describe True
    putStrLn $ "describe False: " ++ describe False
    putStrLn $ "describe (Circle 2.5): " ++ describe (Circle 2.5)
    putStrLn $ "describe (Rectangle 3 4): " ++ describe (Rectangle 3 4)
    putStrLn $ "describe Red: " ++ describe Red
    putStrLn $ "describe (5 :: Int): " ++ describe (5 :: Int)
    putStrLn ""
    
    putStrLn "HC7T10:"
    putStrLn $ "describeAndCompare Red Green: " ++ describeAndCompare Red Green
    putStrLn $ "describeAndCompare 5 3: " ++ describeAndCompare (5 :: Int) 3
    putStrLn $ "describeAndCompare' True False: " ++ describeAndCompare' True False
    putStrLn ""
