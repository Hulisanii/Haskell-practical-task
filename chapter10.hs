{-# LANGUAGE FlexibleInstances #-}

-- HC10T1: ShowSimple Type Class
class ShowSimple a where
    showSimple :: a -> String

data PaymentMethod = Cash | Card | Crypto

instance ShowSimple PaymentMethod where
    showSimple Cash = "Cash"
    showSimple Card = "Card"
    showSimple Crypto = "Crypto"

-- HC10T2: Summable Type Class
class Summable a where
    sumUp :: [a] -> a

instance Summable Int where
    sumUp = sum

-- HC10T3: Comparable Type Class
class Comparable a where
    compareWith :: a -> a -> Ordering

data Blockchain = Bitcoin | Ethereum | Solana

instance Comparable Blockchain where
    compareWith Bitcoin Bitcoin = EQ
    compareWith Ethereum Ethereum = EQ
    compareWith Solana Solana = EQ
    compareWith Bitcoin _ = GT
    compareWith Ethereum Bitcoin = LT
    compareWith Ethereum Solana = GT
    compareWith Solana _ = LT

-- HC10T4: Eq Instance for Box
data Box a = Box a

instance Eq a => Eq (Box a) where
    (Box x) == (Box y) = x == y

-- HC10T5: ShowDetailed Type Class
class ShowDetailed a where
    showDetailed :: a -> String

data User = User 
    { userName :: String
    , userAge :: Int
    , userEmail :: String
    }

instance ShowDetailed User where
    showDetailed user = "User: " ++ userName user ++ 
                       ", Age: " ++ show (userAge user) ++ 
                       ", Email: " ++ userEmail user

-- HC10T6: Mutual Recursion in Eq for Blockchain
instance Eq Blockchain where
    Bitcoin == Bitcoin = True
    Ethereum == Ethereum = True
    Solana == Solana = True
    _ == _ = False
    
    x /= y = not (x == y)

-- HC10T7: Convertible Type Class
class Convertible a b where
    convert :: a -> b

instance Convertible PaymentMethod String where
    convert Cash = "Cash Payment"
    convert Card = "Card Payment" 
    convert Crypto = "Crypto Payment"

-- HC10T8: AdvancedEq Subclass of Eq
class Eq a => AdvancedEq a where
    compareEquality :: a -> a -> Bool

instance AdvancedEq Blockchain where
    compareEquality x y = x == y

-- HC10T9: MinMax Type Class
class MinMax a where
    minValue :: a
    maxValue :: a

instance MinMax Int where
    minValue = minBound
    maxValue = maxBound

-- HC10T10: Concatenatable Type Class
class Concatenatable a where
    concatWith :: a -> a -> a

instance Concatenatable [Char] where
    concatWith = (++)

-- Show instances for printing
instance Show PaymentMethod where
    show = showSimple

instance Show Blockchain where
    show Bitcoin = "Bitcoin"
    show Ethereum = "Ethereum" 
    show Solana = "Solana"

instance Show User where
    show user = "User " ++ userName user

instance Show a => Show (Box a) where
    show (Box x) = "Box " ++ show x

-- Test functions to demonstrate usage
main :: IO ()
main = do
    putStrLn "HC10T1:"
    print $ showSimple Cash
    print $ showSimple Card
    print $ showSimple Crypto
    putStrLn ""
    
    putStrLn "HC10T2:"
    print $ sumUp ([1, 2, 3, 4] :: [Int])
    putStrLn ""
    
    putStrLn "HC10T3:"
    print $ compareWith Bitcoin Ethereum
    print $ compareWith Ethereum Solana
    print $ compareWith Solana Solana
    putStrLn ""  
    
    putStrLn "HC10T4:"
    print $ Box (5 :: Int) == Box (5 :: Int)
    print $ Box (5 :: Int) == Box (6 :: Int)
    putStrLn ""    
    
    putStrLn "HC10T5:"
    let testUser = User "John Doe" 30 "john@example.com"
    putStrLn $ showDetailed testUser
    putStrLn "" 
    
    putStrLn "HC10T6:"
    print $ Bitcoin == Bitcoin
    print $ Bitcoin /= Ethereum
    putStrLn "" 
    
    putStrLn "HC10T7:"
    putStrLn $ convert Cash
    putStrLn $ convert Crypto
    putStrLn "" 
    
    putStrLn "HC10T8:"
    print $ compareEquality Bitcoin Bitcoin
    print $ compareEquality Bitcoin Ethereum
    putStrLn "" 
    
    putStrLn "HC10T9: MinMax"
    print $ (minValue :: Int)
    print $ (maxValue :: Int)
    putStrLn "" 
    
    putStrLn "HC10T10:"
    putStrLn $ concatWith "Hello " "World!"
   
