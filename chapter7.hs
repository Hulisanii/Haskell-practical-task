-- HC7T1: Eq Instance for Color
data Color = Red | Green | Blue

instance Eq Color where
    Red == Red = True
    Green == Green = True
    Blue == Blue = True
    _ == _ = False

-- HC7T2: Ord Instance for Color
instance Ord Color where
    compare Red Red = EQ
    compare Green Green = EQ
    compare Blue Blue = EQ
    compare Red _ = LT
    compare Green Red = GT
    compare Green Blue = LT
    compare Blue _ = GT

-- HC7T3: Function with Eq and Ord Constraints
compareValues :: (Eq a, Ord a) => a -> a -> a
compareValues x y
    | x > y     = x
    | otherwise = y

-- HC7T4: Shape with Show and Read Instances
data Shape = Circle Double | Rectangle Double Double

instance Show Shape where
    show (Circle r) = "Circle " ++ show r
    show (Rectangle w h) = "Rectangle " ++ show w ++ " " ++ show h

instance Read Shape where
    readsPrec _ input =
        case words input of
            ["Circle", r] -> [(Circle (read r), "")]
            ["Rectangle", w, h] -> [(Rectangle (read w) (read h), "")]
            _ -> []

-- HC7T5: Function with Num Constraint
squareArea :: Num a => a -> a
squareArea side = side * side

-- HC7T6: Circle Circumference with Integral and Floating
circleCircumference :: Floating a => a -> a
circleCircumference radius = 2 * pi * radius

circleCircumferenceInt :: Integral a => a -> Double
circleCircumferenceInt radius = 2 * pi * fromIntegral radius

-- HC7T7: Bounded and Enum for Color
instance Bounded Color where
    minBound = Red
    maxBound = Blue

instance Enum Color where
    fromEnum Red = 0
    fromEnum Green = 1
    fromEnum Blue = 2
    
    toEnum 0 = Red
    toEnum 1 = Green
    toEnum 2 = Blue
    toEnum _ = error "Color.toEnum: invalid value"

nextColor :: Color -> Color
nextColor color
    | color == maxBound = minBound
    | otherwise = succ color

-- HC7T8: Parse Shape from String
parseShape :: String -> Maybe Shape
parseShape input = case reads input of
    [(shape, "")] -> Just shape
    _ -> Nothing

-- HC7T9: Describable Type Class
class Describable a where
    describe :: a -> String

instance Describable Bool where
    describe True = "This is true"
    describe False = "This is false"

instance Describable Shape where
    describe (Circle r) = "A circle with radius " ++ show r
    describe (Rectangle w h) = "A rectangle with width " ++ show w ++ " and height " ++ show h

-- HC7T10: Function with Multiple Type Class Constraints
describeAndCompare :: (Describable a, Ord a) => a -> a -> String
describeAndCompare x y
    | x > y     = describe x
    | otherwise = describe y

-- Main function to test everything
main :: IO ()
main = do
    putStrLn "=== Testing Color Type ==="
    print $ Red == Green  -- Should be False
    print $ Red < Green   -- Should be True
    print $ compareValues Red Blue  -- Should be Blue
    
    putStrLn "\n=== Testing nextColor ==="
    print $ nextColor Red    -- Should be Green
    print $ nextColor Green  -- Should be Blue
    print $ nextColor Blue   -- Should be Red (wrap around)
    
    putStrLn "\n=== Testing Shape Type ==="
    let circle = Circle 5.0
    let rect = Rectangle 4.0 6.0
    print circle
    print rect
    
    putStrLn "\n=== Testing parseShape ==="
    print $ parseShape "Circle 5.0"
    print $ parseShape "Rectangle 3.0 4.0"
    print $ parseShape "Invalid"
    
    putStrLn "\n=== Testing Describable ==="
    putStrLn $ describe True
    putStrLn $ describe False
    putStrLn $ describe circle
    putStrLn $ describe rect
    
    putStrLn "\n=== Testing Math Functions ==="
    print $ squareArea (5 :: Int)
    print $ squareArea (5.0 :: Double)
    print $ circleCircumference (5.0 :: Double)
    print $ circleCircumferenceInt (5 :: Int)
    
    putStrLn "\n=== Testing describeAndCompare ==="
    putStrLn $ describeAndCompare (Circle 3.0) (Circle 5.0)
    putStrLn $ describeAndCompare (Rectangle 2.0 3.0) (Rectangle 1.0 4.0)
    
    putStrLn "All tests completed!"
