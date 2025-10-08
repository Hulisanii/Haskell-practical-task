--By Hulisani Muravha
module Main where
-- HC8T1: Type Synonyms and Basic Function
type Address = String
type Value = Int

generateTx :: Address -> Address -> Value -> String
generateTx from to amount = from ++ " -> " ++ to ++ ": " ++ show amount

-- HC8T2: New Types and Data Constructors
data PaymentMethod = Cash | Card | Cryptocurrency
    deriving Show

type Person = (String, (String, Int), PaymentMethod)

bob :: Person
bob = ("Bob", ("123 Main St", 12345), Cash)

-- HC8T3: Algebraic Data Types and Functions
data Shape = Circle Float | Rectangle Float Float

area :: Shape -> Float
area (Circle r) = pi * r * r
area (Rectangle w h) = w * h

-- HC8T4: Record Syntax for Employee
data Employee = Employee
    { name :: String
    , experienceInYears :: Float
    } deriving Show

richard :: Employee
richard = Employee { name = "Richard", experienceInYears = 7.5 }

-- HC8T5: Record Syntax for Person
data PersonRec = PersonRec
    { personName :: String
    , age :: Int
    , isEmployed :: Bool
    } deriving Show

person1 :: PersonRec
person1 = PersonRec { personName = "Alice", age = 30, isEmployed = True }

person2 :: PersonRec
person2 = PersonRec { personName = "Bob", age = 25, isEmployed = False }

-- HC8T6: Record Syntax for Shape Variants
data ShapeRecord = CircleRecord
    { center :: (Float, Float)
    , color :: String
    , radius :: Float
    } | RectangleRecord
    { width :: Float
    , height :: Float
    , rectColor :: String
    } deriving Show

circleShape :: ShapeRecord
circleShape = CircleRecord { center = (0, 0), color = "Red", radius = 5.0 }

rectangleShape :: ShapeRecord
rectangleShape = RectangleRecord { width = 10.0, height = 5.0, rectColor = "Blue" }

-- HC8T7: Data Types and Describing Animals
data Animal = Dog String | Cat String
    deriving Show

describeAnimal :: Animal -> String
describeAnimal (Dog name) = "Dog named " ++ name
describeAnimal (Cat name) = "Cat named " ++ name

-- HC8T8: Type Synonyms and Greeting Function
type Name = String
type Age = Int

greet :: Name -> Age -> String
greet name age = "Hello " ++ name ++ ", you are " ++ show age ++ " years old!"

-- HC8T9: Record Type and Transaction Function
data Transaction = Transaction
    { fromAddr :: Address
    , toAddr :: Address
    , amountVal :: Value
    , transactionId :: String
    } deriving Show

createTransaction :: Address -> Address -> Value -> String -> Transaction
createTransaction from to amount tid = Transaction
    { fromAddr = from
    , toAddr = to
    , amountVal = amount
    , transactionId = tid
    }

-- HC8T10: Deriving Show for Book
data Book = Book
    { title :: String
    , author :: String
    , year :: Int
    } deriving Show

main :: IO ()
main = do
    -- HC8T1 Test
    putStrLn "HC8T1: Type Synonyms and Basic Function"
    putStrLn $ generateTx "Alice" "Bob" 100
    
    -- HC8T2 Test
    putStrLn "\nHC8T2: New Types and Data Constructors"
    print bob
    
    -- HC8T3 Test
    putStrLn "\nHC8T3: Algebraic Data Types and Functions"
    putStrLn $ "Circle area: " ++ show (area (Circle 5))
    putStrLn $ "Rectangle area: " ++ show (area (Rectangle 10 5))
    
    -- HC8T4 Test
    putStrLn "\nHC8T4: Record Syntax for Employee"
    print richard
    
    -- HC8T5 Test
    putStrLn "\nHC8T5: Record Syntax for Person"
    print person1
    print person2
    
    -- HC8T6 Test
    putStrLn "\nHC8T6: Record Syntax for Shape Variants"
    print circleShape
    print rectangleShape
    
    -- HC8T7 Test
    putStrLn "\nHC8T7: Data Types and Describing Animals"
    putStrLn $ describeAnimal (Dog "Rex")
    putStrLn $ describeAnimal (Cat "Whiskers")
    
    -- HC8T8 Test
    putStrLn "\nHC8T8: Type Synonyms and Greeting Function"
    putStrLn $ greet "Charlie" 35
    
    -- HC8T9 Test
    putStrLn "\nHC8T9: Record Type and Transaction Function"
    let tx = createTransaction "Alice" "Bob" 50 "tx123"
    print tx
    
    -- HC8T10 Test
    putStrLn "\nHC8T10: Deriving Show for Book"
    let book = Book { title = "Haskell Guide", author = "John Doe", year = 2023 }
    print book
