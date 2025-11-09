
{-# LANGUAGE InstanceSigs #-}

import Control.Applicative (liftA2, liftA3, (<*), (*>))
import Control.Monad (forever, when, replicateM)
import Data.Foldable (sequenceA_)

-- HC19T1:
data Pair a = Pair a a deriving (Show, Eq)
instance Functor Pair where
fmap f (Pair x y) = Pair (f x) (f y)
instance Applicative Pair where
    pure x = Pair x x
    (Pair f g) <*> (Pair x y) = Pair (f x) (g y)

-- HC19T2:
addThreeApplicative :: Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int
addThreeApplicative x y z = (+) <$> x <*> y <*> z

-- HC19T3: 
safeProduct :: [Maybe Int] -> Maybe Int
safeProduct = foldr (\x acc -> (*) <$> x <*> acc) (Just 1)

-- HC19T4:
liftAndMultiply :: (Int -> Int -> Int) -> Maybe Int -> Maybe Int -> Maybe Int
liftAndMultiply = liftA2

-- HC19T5:
applyEffects :: (IO Int, IO Int) -> IO Int
applyEffects (io1, io2) = do
    result1 <- io1
    result2 <- io2
    putStrLn $ "First value: " ++ show result1
    putStrLn $ "Second value: " ++ show result2
    return (result1 + result2)

-- HC19T6:
repeatEffect :: IO () -> IO ()
repeatEffect = forever

-- HC19T7: 
conditionalPrint :: Bool -> String -> IO ()
conditionalPrint = when

-- HC19T8: 
discardSecond :: IO a -> IO b -> IO a
discardSecond = (<*)

-- HC19T9: 
pureAndApply :: Applicative f => f (a -> b) -> a -> f b
pureAndApply f x = f <*> pure x

-- HC19T10: 
combineResults :: Either String Int -> Either String Int -> Either String Int
combineResults x y = (+) <$> x <*> y

-- HC19T11:
newtype Wrapper a = Wrapper { unwrap :: a } deriving (Show, Eq)

instance Functor Wrapper where
    fmap f (Wrapper x) = Wrapper (f x)

instance Applicative Wrapper where
    pure = Wrapper
    (Wrapper f) <*> (Wrapper x) = Wrapper (f x)

-- HC19T12:
sumThreeApplicative :: Either String Int -> Either String Int -> Either String Int -> Either String Int
sumThreeApplicative x y z = liftA3 (\a b c -> a + b + c) x y z

-- HC19T13: 
whenApplicative :: Applicative f => Bool -> f () -> f ()
whenApplicative condition action = if condition then action else pure ()

-- HC19T14: 
replicateEffect :: Int -> IO a -> IO [a]
replicateEffect = replicateM

-- HC19T15: 
sequenceEffects :: Applicative f => [f a] -> f [a]
sequenceEffects = sequenceA

-- HC19T16: 
applyWithEffects :: IO (Int -> Int) -> IO Int -> IO Int
applyWithEffects = (<*>)

-- HC19T17: 
simulateMaybeEffect :: (Int -> Int -> Int -> Int) -> Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int
simulateMaybeEffect f x y z = f <$> x <*> y <*> z

-- HC19T18:
combineEitherResults :: Either String Int -> Either String Int -> Either String Int -> Either String Int
combineEitherResults x y z = 
    case (x, y, z) of
        (Right a, Right b, Right c) -> Right (a + b + c)
        (Left e, _, _) -> Left e
        (_, Left e, _) -> Left e
        (_, _, Left e) -> Left e

-- HC19T19:
sequenceApplicative :: [Maybe a] -> Maybe [a]
sequenceApplicative = sequenceA

-- HC19T20: 
replicateForever :: IO () -> IO ()
replicateForever = forever

main :: IO ()
main = do
    putStrLn "HC19T1: Applicative Instance for Pair"
    let pair1 = Pair (+1) (*2) <*> Pair 3 4
    print pair1  
    
    putStrLn "\nHC19T2: addThreeApplicative"
    print $ addThreeApplicative (Just 1) (Just 2) (Just 3)  
    print $ addThreeApplicative (Just 1) Nothing (Just 3)    
    
    putStrLn "\nHC19T3: safeProduct"
    print $ safeProduct [Just 2, Just 3, Just 4]  
    print $ safeProduct [Just 2, Nothing, Just 4] 
    
    putStrLn "\nHC19T4: liftAndMultiply"
    print $ liftAndMultiply (+) (Just 5) (Just 3)  
    
    putStrLn "\nHC19T5: applyEffects"
    let effects = (return 10, return 20)
    result <- applyEffects effects
    putStrLn $ "Sum: " ++ show result
    
    putStrLn "\nHC19T6: repeatEffect (demonstrating with limited prints)"
    putStrLn "repeatEffect would print forever (commented out)"
    
    putStrLn "\nHC19T7: conditionalPrint"
    conditionalPrint True "This should print"
    conditionalPrint False "This should NOT print"
    
    putStrLn "\nHC19T8: discardSecond"
    result8 <- discardSecond (putStrLn "First" >> return 1) (putStrLn "Second" >> return 2)
    putStrLn $ "Result: " ++ show result8
    
    putStrLn "\nHC19T9: pureAndApply"
    print $ pureAndApply (Just (+1)) 5  
    print $ pureAndApply (Wrapper (*2)) 3  
    
    putStrLn "\nHC19T10: combineResults"
    print $ combineResults (Right 1) (Right 2)  
    print $ combineResults (Left "error") (Right 2) 
    
    putStrLn "\nHC19T11: Wrapper Applicative"
    let wrapperResult = Wrapper (+1) <*> Wrapper 5
    print wrapperResult  
    
    putStrLn "\nHC19T12: sumThreeApplicative"
    print $ sumThreeApplicative (Right 1) (Right 2) (Right 3)  
    print $ sumThreeApplicative (Right 1) (Left "error") (Right 3) 
    
    putStrLn "\nHC19T13: whenApplicative"
    whenApplicative True $ putStrLn "Condition is true"
    whenApplicative False $ putStrLn "This won't print"
    
    putStrLn "\nHC19T14: replicateEffect"
    results <- replicateEffect 3 (putStrLn "Replicated" >> return 42)
    print results
    
    putStrLn "\nHC19T15: sequenceEffects"
    print $ sequenceEffects [Just 1, Just 2, Just 3]  
    print $ sequenceEffects [Just 1, Nothing, Just 3] 
    
    putStrLn "\nHC19T16: applyWithEffects"
    let funcIO = return (+10)
    let valueIO = return 5
    result16 <- applyWithEffects funcIO valueIO
    print result16  
    
    putStrLn "\nHC19T17: simulateMaybeEffect"
    let tripleSum a b c = a + b + c
    print $ simulateMaybeEffect tripleSum (Just 1) (Just 2) (Just 3) 
    
    putStrLn "\nHC19T18: combineEitherResults"
    print $ combineEitherResults (Right 1) (Right 2) (Right 3) 
    print $ combineEitherResults (Left "err1") (Right 2) (Right 3)  
    
    putStrLn "\nHC19T19: sequenceApplicative"
    print $ sequenceApplicative [Just 1, Just 2]  
    print $ sequenceApplicative [Just 1, Nothing] 
    
    putStrLn "\nHC19T20: replicateForever"
    putStrLn "replicateForever would run forever (commented out)"
  
    
