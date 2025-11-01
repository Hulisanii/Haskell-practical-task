
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module SemigroupMonoidTasks where

import Data.Semigroup
import Data.Monoid

-- HC17T1: Severity Data Type and Semigroup Instance
data Severity = Low | Medium | High | Critical
    deriving (Show, Eq, Ord, Enum, Bounded)

instance Semigroup Severity where
    x <> y = max x y  -- Higher severity overrides lower one

-- HC17T2: Min and Max Newtypes with Semigroup
newtype Min a = Min { getMin :: a }
    deriving (Show, Eq, Ord)

instance Ord a => Semigroup (Min a) where
    Min x <> Min y = Min (min x y)

newtype Max a = Max { getMax :: a }
    deriving (Show, Eq, Ord)

instance Ord a => Semigroup (Max a) where
    Max x <> Max y = Max (max x y)

-- HC17T3: Monoid Instance for Severity
instance Monoid Severity where
    mempty = Low

-- HC17T4: Monoid Instance for Sum Newtype
newtype Sum a = Sum { getSum :: a }
    deriving (Show, Eq, Num)

instance Num a => Semigroup (Sum a) where
    Sum x <> Sum y = Sum (x + y)

instance Num a => Monoid (Sum a) where
    mempty = Sum 0

-- HC17T5: combineLists Function
combineLists :: [Int] -> [Int] -> [Int]
combineLists xs ys = xs <> ys  -- Using Semigroup instance for lists

-- HC17T6: maxSeverity Function
maxSeverity :: [Severity] -> Severity
maxSeverity = mconcat

-- HC17T7: multiplyProducts Function
newtype Product a = Product { getProduct :: a }
    deriving (Show, Eq, Num)

instance Num a => Semigroup (Product a) where
    Product x <> Product y = Product (x * y)

instance Num a => Monoid (Product a) where
    mempty = Product 1

multiplyProducts :: [Product Int] -> Product Int
multiplyProducts = mconcat

-- HC17T8: foldWithSemigroup Function
foldWithSemigroup :: Semigroup a => [a] -> a
foldWithSemigroup = foldr (<>) mempty

-- HC17T9: Config Data Type and Semigroup Instance
data LoggingLevel = Debug | Info | Warning | Error
    deriving (Show, Eq, Ord, Enum, Bounded)

data Config = Config
    { loggingLevel :: LoggingLevel
    , timeout :: Int
    , retries :: Int
    } deriving (Show, Eq)

instance Semigroup Config where
    Config l1 t1 r1 <> Config l2 t2 r2 = 
        Config (max l1 l2) (min t1 t2) (max r1 r2)

-- HC17T10: Monoid Instance for Config
instance Monoid Config where
    mempty = Config Debug maxBound 0  -- Lowest loggingLevel, highest timeout, lowest retries

-- Additional utility functions and demonstrations
main :: IO ()
main = do
    putStrLn "=== HC17T1: Severity Data Type and Semigroup Instance ==="
    putStrLn $ "Low <> Medium = " ++ show (Low <> Medium)
    putStrLn $ "High <> Medium = " ++ show (High <> Medium)
    putStrLn $ "Critical <> High = " ++ show (Critical <> High)
    putStrLn $ "Low <> Critical = " ++ show (Low <> Critical)
    
    putStrLn "\n=== HC17T2: Min and Max Newtypes with Semigroup ==="
    let min1 = Min (5 :: Int)
        min2 = Min (3 :: Int)
        max1 = Max (5 :: Int)
        max2 = Max (3 :: Int)
    putStrLn $ "Min 5 <> Min 3 = " ++ show (min1 <> min2)
    putStrLn $ "Max 5 <> Max 3 = " ++ show (max1 <> max2)
    
    putStrLn "\n=== HC17T3: Monoid Instance for Severity ==="
    putStrLn $ "mempty :: Severity = " ++ show (mempty :: Severity)
    putStrLn $ "Low <> mempty = " ++ show (Low <> (mempty :: Severity))
    putStrLn $ "mconcat [Low, Medium, High] = " ++ show (mconcat [Low, Medium, High])
    
    putStrLn "\n=== HC17T4: Monoid Instance for Sum Newtype ==="
    let sum1 = Sum (10 :: Int)
        sum2 = Sum (20 :: Int)
    putStrLn $ "Sum 10 <> Sum 20 = " ++ show (sum1 <> sum2)
    putStrLn $ "mempty :: Sum Int = " ++ show (mempty :: Sum Int)
    putStrLn $ "mconcat [Sum 1, Sum 2, Sum 3] = " ++ show (mconcat [Sum 1, Sum 2, Sum 3])
    
    putStrLn "\n=== HC17T5: combineLists Function ==="
    putStrLn $ "combineLists [1,2,3] [4,5,6] = " ++ show (combineLists [1,2,3] [4,5,6])
    putStrLn $ "combineLists [] [7,8,9] = " ++ show (combineLists [] [7,8,9])
    
    putStrLn "\n=== HC17T6: maxSeverity Function ==="
    putStrLn $ "maxSeverity [Low, Medium] = " ++ show (maxSeverity [Low, Medium])
    putStrLn $ "maxSeverity [Low, High, Medium] = " ++ show (maxSeverity [Low, High, Medium])
    putStrLn $ "maxSeverity [Critical, High, Low] = " ++ show (maxSeverity [Critical, High, Low])
    putStrLn $ "maxSeverity [] = " ++ show (maxSeverity [])
    
    putStrLn "\n=== HC17T7: multiplyProducts Function ==="
    let products = [Product 2, Product 3, Product 4]
    putStrLn $ "multiplyProducts [Product 2, Product 3, Product 4] = " ++ show (multiplyProducts products)
    putStrLn $ "multiplyProducts [] = " ++ show (multiplyProducts [])
    
    putStrLn "\n=== HC17T8: foldWithSemigroup Function ==="
    putStrLn $ "foldWithSemigroup [Sum 1, Sum 2, Sum 3] = " ++ show (foldWithSemigroup [Sum 1, Sum 2, Sum 3])
    putStrLn $ "foldWithSemigroup [Min 5, Min 3, Min 7] = " ++ show (foldWithSemigroup [Min 5, Min 3, Min 7])
    putStrLn $ "foldWithSemigroup [Max 1, Max 8, Max 3] = " ++ show (foldWithSemigroup [Max 1, Max 8, Max 3])
    putStrLn $ "foldWithSemigroup [\"Hello\", \" \", \"World\"] = " ++ show (foldWithSemigroup ["Hello", " ", "World"])
    
    putStrLn "\n=== HC17T9: Config Data Type and Semigroup Instance ==="
    let config1 = Config Debug 30 3
        config2 = Config Warning 10 5
        config3 = Config Error 20 2
    putStrLn $ "Config Debug 30 3 <> Config Warning 10 5 = " ++ show (config1 <> config2)
    putStrLn $ "Config Warning 10 5 <> Config Error 20 2 = " ++ show (config2 <> config3)
    
    putStrLn "\n=== HC17T10: Monoid Instance for Config ==="
    putStrLn $ "mempty :: Config = " ++ show (mempty :: Config)
    putStrLn $ "Config Debug 30 3 <> mempty = " ++ show (config1 <> mempty)
    putStrLn $ "mconcat [config1, config2, config3] = " ++ show (mconcat [config1, config2, config3])
    
    -- Additional demonstrations
    putStrLn "\n=== Additional Demonstrations ==="
    
    -- Using foldMap
    putStrLn "Using foldMap with Severity:"
    putStrLn $ "foldMap Max [1,5,3,7,2] = " ++ show (getMax (foldMap Max [1,5,3,7,2 :: Int]))
    putStrLn $ "foldMap Min [1,5,3,7,2] = " ++ show (getMin (foldMap Min [1,5,3,7,2 :: Int]))
    
    -- Testing associativity
    putStrLn "\nTesting associativity (a <> (b <> c) == (a <> b) <> c):"
    let a = Medium
        b = High
        c = Low
    putStrLn $ "Medium <> (High <> Low) = " ++ show (a <> (b <> c))
    putStrLn $ "(Medium <> High) <> Low = " ++ show ((a <> b) <> c)
    putStrLn $ "Associative: " ++ show (a <> (b <> c) == (a <> b) <> c)
    
    -- Testing identity laws
    putStrLn "\nTesting identity laws:"
    putStrLn $ "Low <> mempty = Low: " ++ show (Low <> mempty == Low)
    putStrLn $ "mempty <> Critical = Critical: " ++ show (mempty <> Critical == Critical)
    
    -- Complex Config example
    putStrLn "\nComplex Config combination:"
    let defaultConfig = mempty
        productionConfig = Config Error 60 5
        developmentConfig = Config Debug 120 10
        combined = defaultConfig <> productionConfig <> developmentConfig
    putStrLn $ "Default <> Production <> Development = " ++ show combined

-- Helper function to demonstrate practical usage
combineMultipleSeverities :: [Severity] -> Severity
combineMultipleSeverities = foldr (<>) mempty

-- Example of using these in a real-world scenario
data AlertSystem = AlertSystem
    { systemName :: String
    , currentSeverity :: Severity
    , configuration :: Config
    }

combineAlerts :: [AlertSystem] -> (Severity, Config)
combineAlerts systems = 
    ( mconcat (map currentSeverity systems)
    , mconcat (map configuration systems)
    )

-- Test the alert system combination
testAlertSystem :: IO ()
testAlertSystem = do
    putStrLn "\n=== Alert System Example ==="
    let system1 = AlertSystem "DB" High (Config Warning 30 3)
        system2 = AlertSystem "API" Medium (Config Error 20 5)
        system3 = AlertSystem "Cache" Critical (Config Info 40 2)
        systems = [system1, system2, system3]
    
    let (combinedSeverity, combinedConfig) = combineAlerts systems
    putStrLn $ "Combined severity: " ++ show combinedSeverity
    putStrLn $ "Combined config: " ++ show combinedConfig
