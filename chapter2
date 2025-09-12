-- HC2T1 – Tuple with different types
sampleData :: (Int, Double, String, Char, Bool)
sampleData = (42, 3.14, "Haskell", 'X', True && False)

-- HC2T2 – Function Type Signatures
addNums :: Int -> Int -> Int
addNums a b = a + b

isEvenNum :: Int -> Bool
isEvenNum n = mod n 2 == 0

joinWords :: String -> String -> String
joinWords first second = first ++ second

-- HC2T3 – Immutable Variables (constants)
myBirthYear :: Int
myBirthYear = 2002

approxPi :: Double
approxPi = 3.14159

welcomeMsg :: String
welcomeMsg = "Hey there, welcome to Haskell :)"

likesCoding :: Bool
likesCoding = True

-- HC2T4 – Prefix & Infix examples
prefixOps :: (Int, Int, Bool)
prefixOps = ((+) 8 4, (*) 7 6, (||) True False)

infixOps :: (Int, Int, Bool)
infixOps = (9 + 1, 12 * 3, True && True)

-- HC2T5 – Functions
circleArea :: Float -> Float
circleArea r = pi * r ^ 2

biggestOf3 :: Int -> Int -> Int -> Int
biggestOf3 x y z = max x (max y z)

-- HC2T6 – Int vs Integer
smallVal :: Int
smallVal = 350

largeVal :: Integer
largeVal = 9876543210

-- HC2T7 – Boolean Expressions
b1, b2, b3, b4 :: Bool
b1 = True && False      -- False
b2 = False || True      -- True
b3 = not True           -- False
b4 = 15 > 3             -- True

-- MAIN (testing all tasks)
main :: IO ()
main = do
    putStrLn ">>> HC2T1 – Tuple:"
    print sampleData

    putStrLn "\n>>> HC2T2 – Functions:"
    print (addNums 4 9)
    print (isEvenNum 12)
    putStrLn (joinWords "Hello" " World")

    putStrLn "\n>>> HC2T3 – Constants:"
    print myBirthYear
    print approxPi
    putStrLn welcomeMsg
    print likesCoding

    putStrLn "\n>>> HC2T4 – Prefix & Infix:"
    print prefixOps
    print infixOps

    putStrLn "\n>>> HC2T5 – More Functions:"
    print (circleArea 7)
    print (biggestOf3 25 12 30)

    putStrLn "\n>>> HC2T6 – Int vs Integer:"
    print (smallVal, largeVal)

    putStrLn "\n>>> HC2T7 – Boolean Expressions:"
    print [b1, b2, b3, b4]

-- Hulisani Muravha
