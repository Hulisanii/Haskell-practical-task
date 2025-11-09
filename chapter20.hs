
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}

import Control.Monad (liftM, mapM, filterM, replicateM, when)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Control.Monad.Trans.State (StateT(..), get, put, modify, evalState, execState, runState)
import Control.Monad.Trans.Writer (WriterT(..), tell, runWriter)
import Control.Monad.Trans.Reader (ReaderT(..), ask, runReader)
import Control.Monad.State (MonadState)
import Control.Monad.Writer (MonadWriter)
import Control.Monad.Reader (MonadReader)
import System.IO (readFile)
import System.Random (randomRIO)
import Data.Char (isDigit, isLetter, isUpper, isLower)

-- HC20T1:
safeDivide :: Double -> Double -> Maybe Double
safeDivide _ 0 = Nothing
safeDivide x y = Just (x / y)
safeDivideMonadic :: Double -> Double -> Maybe Double
safeDivideMonadic x y = do
    guard $ y /= 0
    return (x / y)

-- HC20T2:
sequenceMaybe :: [Maybe a] -> Maybe [a]
sequenceMaybe [] = Just []
sequenceMaybe (Nothing:_) = Nothing
sequenceMaybe (Just x:xs) = do
    rest <- sequenceMaybe xs
    return (x:rest)
sequenceMaybe' :: [Maybe a] -> Maybe [a]
sequenceMaybe' = sequence

-- HC20T3: 
type Logging = WriterT [String] Maybe
loggingCalculator :: Double -> Double -> Logging Double
loggingCalculator x y = do
    tell ["Adding " ++ show x ++ " and " ++ show y]
    let sumResult = x + y
    tell ["Result: " ++ show sumResult]
    return sumResult
calculator :: String -> Double -> Double -> Writer [String] Double
calculator "add" x y = do
    tell ["Performing addition: " ++ show x ++ " + " ++ show y]
    return (x + y)
calculator "multiply" x y = do
    tell ["Performing multiplication: " ++ show x ++ " * " ++ show y]
    return (x * y)
calculator op _ _ = do
    tell ["Unknown operation: " ++ op]
    return 0

-- HC20T4:
countChars :: Char -> String -> State Int ()
countChars _ [] = return ()
countChars target (x:xs) = do
    when (x == target) $ modify (+1)
    countChars target xs
countCharsTotal :: Char -> String -> Int
countCharsTotal char str = execState (countChars char str) 0

-- HC20T5: 
type Config = String
type GreetingM = Reader Config
getGreeting :: GreetingM String
getGreeting = do
    config <- ask
    return $ "Hello, " ++ config ++ "!"
getFormalGreeting :: String -> GreetingM String
getFormalGreeting name = do
    config <- ask
    return $ config ++ " " ++ name

-- HC20T6: 
doubleMonad :: Maybe [Int] -> [Maybe Int]
doubleMonad Nothing = [Nothing]
doubleMonad (Just xs) = map Just xs

-- HC20T7:
findFirst :: (a -> Bool) -> [a] -> Either String a
findFirst _ [] = Left "Element not found"
findFirst pred (x:xs)
    | pred x = Right x
    | otherwise = findFirst pred xs

-- HC20T8:
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }
instance Functor Parser where
    fmap f (Parser p) = Parser $ \input -> do
        (result, rest) <- p input
        return (f result, rest)
instance Applicative Parser where
    pure x = Parser $ \input -> Just (x, input)
    (Parser pf) <*> (Parser px) = Parser $ \input -> do
        (f, rest1) <- pf input
        (x, rest2) <- px rest1
        return (f x, rest2)
instance Monad Parser where
    (Parser p) >>= f = Parser $ \input -> do
        (result, rest) <- p input
        runParser (f result) rest
parseChar :: Char -> Parser Char
parseChar expected = Parser $ \input ->
    case input of
        (x:xs) | x == expected -> Just (x, xs)
        _ -> Nothing
parseDigit :: Parser Int
parseDigit = Parser $ \input ->
    case input of
        (x:xs) | x `elem` "0123456789" -> Just (read [x], xs)
        _ -> Nothing

-- HC20T9: 
newtype Identity a = Identity { runIdentity :: a }
instance Functor Identity where
    fmap f (Identity x) = Identity (f x)
instance Applicative Identity where
    pure = Identity
    (Identity f) <*> (Identity x) = Identity (f x)
instance Monad Identity where
    (Identity x) >>= f = f x
replicateMonad :: Int -> a -> Identity [a]
replicateMonad n x = return (replicate n x)

-- HC20T10:
type StateMaybe s a = StateT s Maybe a
safeStateOperation :: StateMaybe Int Int
safeStateOperation = do
    current <- get
    when (current <= 0) $ fail "State must be positive"
    put (current * 2)
    return current

-- HC20T11:
type Position = (Int, Int)
randomWalk :: Int -> State Position [Position]
randomWalk 0 = do
    pos <- get
    return [pos]
randomWalk steps = do
    pos <- get
    direction <- liftIO $ randomRIO (0, 3)
    let newPos = case direction of
                    0 -> (fst pos + 1, snd pos)     
                    1 -> (fst pos - 1, snd pos)     
                    2 -> (fst pos, snd pos + 1)     
                    3 -> (fst pos, snd pos - 1)     
    put newPos
    rest <- randomWalk (steps - 1)
    return (pos:rest)
randomWalkDemo :: Int -> State Position [Position]
randomWalkDemo 0 = do
    pos <- get
    return [pos]
randomWalkDemo steps = do
    pos <- get
    let direction = steps `mod` 4  
    let newPos = case direction of
                    0 -> (fst pos + 1, snd pos)
                    1 -> (fst pos - 1, snd pos)
                    2 -> (fst pos, snd pos + 1)
                    3 -> (fst pos, snd pos - 1)
    put newPos
    rest <- randomWalkDemo (steps - 1)
    return (pos:rest)

-- HC20T12: 
readFileLines :: FilePath -> IO [String]
readFileLines filepath = do
    content <- readFile filepath
    return (lines content)

displayFileContents :: FilePath -> IO ()
displayFileContents filepath = do
    lines <- readFileLines filepath
    mapM_ putStrLn lines

-- HC20T13: 
fibonacciState :: Int -> State (Int, Int) Int
fibonacciState 0 = return 0
fibonacciState 1 = return 1
fibonacciState n = do
    (prev, current) <- get
    put (current, prev + current)
    fibNMinus1 <- fibonacciState (n-1)
    return fibNMinus1

-- HC20T14:
mapMFilter :: Monad m => (a -> m Bool) -> (a -> m b) -> [a] -> m [b]
mapMFilter pred f xs = do
    filtered <- filterM pred xs
    mapM f filtered

-- HC20T15:
data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Show)
instance Functor Tree where
    fmap f (Leaf x) = Leaf (f x)
    fmap f (Node left right) = Node (fmap f left) (fmap f right)
newtype TreeMonad a = TreeMonad { runTreeMonad :: Tree a }
instance Functor TreeMonad where
    fmap f (TreeMonad tree) = TreeMonad (fmap f tree)
instance Applicative TreeMonad where
    pure = TreeMonad . Leaf
    (TreeMonad fTree) <*> (TreeMonad xTree) = TreeMonad (applyTree fTree xTree)
      where
        applyTree (Leaf f) tree = fmap f tree
        applyTree (Node leftF rightF) tree = Node (applyTree leftF tree) (applyTree rightF tree)
instance Monad TreeMonad where
    return = pure
    (TreeMonad (Leaf x)) >>= f = f x
    (TreeMonad (Node left right)) >>= f = 
        TreeMonad $ Node (runTreeMonad (TreeMonad left >>= f)) (runTreeMonad (TreeMonad right >>= f))
treeSum :: Tree Int -> Int
treeSum (Leaf x) = x
treeSum (Node left right) = treeSum left + treeSum right
treeSumMonadic :: Tree Int -> TreeMonad Int
treeSumMonadic tree = do
    values <- TreeMonad tree
    return (sumTreeValues tree)
  where
    sumTreeValues (Leaf x) = x
    sumTreeValues (Node left right) = sumTreeValues left + sumTreeValues right

-- HC20T16:
retryIO :: Int -> IO a -> IO (Maybe a)
retryIO 0 _ = return Nothing
retryIO attempts action = do
    result <- action
    return (Just result)
    `catch` (\e -> do
        putStrLn $ "Attempt failed: " ++ show (e :: IOError)
        retryIO (attempts - 1) action)
retryIOSimple :: Int -> IO Bool -> IO Bool
retryIOSimple 0 _ = return False
retryIOSimple attempts action = do
    success <- action
    if success then return True
    else retryIOSimple (attempts - 1) action

-- HC20T17:
validatePassword :: String -> Either String String
validatePassword password = do
    when (length password < 8) $ Left "Password must be at least 8 characters long"
    when (not (any isUpper password)) $ Left "Password must contain at least one uppercase letter"
    when (not (any isLower password)) $ Left "Password must contain at least one lowercase letter"
    when (not (any isDigit password)) $ Left "Password must contain at least one digit"
    Right "Password is valid"

-- HC20T18:
getValidNumber :: MaybeT IO Int
getValidNumber = MaybeT $ do
    putStr "Enter a number: "
    input <- getLine
    case reads input of
        [(n, "")] -> return (Just n)
        _ -> do
            putStrLn "Invalid input. Please enter a valid number."
            return Nothing
getPositiveNumber :: MaybeT IO Int
getPositiveNumber = do
    n <- getValidNumber
    guard (n > 0)
    return n

-- HC20T19: 
type Logger = Writer [String]
logFunction :: String -> [String] -> Logger ()
logFunction funcName args = do
    tell [funcName ++ " called with arguments: " ++ show args]
addWithLog :: Int -> Int -> Logger Int
addWithLog x y = do
    logFunction "add" [show x, show y]
    tell ["Result: " ++ show (x + y)]
    return (x + y)
multiplyWithLog :: Int -> Int -> Logger Int
multiplyWithLog x y = do
    logFunction "multiply" [show x, show y]
    tell ["Result: " ++ show (x * y)]
    return (x * y)

-- HC20T20:
batchProcessing :: [IO a] -> IO [a]
batchProcessing actions = foldr processAction (return []) actions
  where
    processAction action acc = do
        result <- action
        results <- acc
        return (result:results)
batchProcessing' :: [IO a] -> IO [a]
batchProcessing' = sequence
liftIO :: IO a -> State Position a
liftIO io = StateT $ \s -> do
    result <- io
    return (result, s)


main :: IO ()
main = do
    putStrLn "HC20T1: safeDivide with Maybe Monad"
    print $ safeDivide 10 2   
    print $ safeDivide 10 0   
    
    putStrLn "\nHC20T2: sequenceMaybe"
    print $ sequenceMaybe [Just 1, Just 2, Just 3]  
    print $ sequenceMaybe [Just 1, Nothing, Just 3] 
    
    putStrLn "\nHC20T3: Writer Monad Logging Calculator"
    let (result3, log3) = runWriter (calculator "add" 5 3)
    print result3
    mapM_ putStrLn log3
    
    putStrLn "\nHC20T4: countChars with State Monad"
    print $ countCharsTotal 'a' "banana"  
    
    putStrLn "\nHC20T5: Reader Monad for Configurable Greeting"
    let greeting = runReader getGreeting "User"
    print greeting
    
    putStrLn "\nHC20T6: doubleMonad"
    print $ doubleMonad (Just [1,2,3])   
    print $ doubleMonad Nothing         
    
    putStrLn "\nHC20T7: findFirst with Either Monad"
    print $ findFirst (>5) [1,3,7,2]    
    print $ findFirst (>10) [1,3,7,2]   
    
    putStrLn "\nHC20T8: Parser Monad (simplified demonstration)"
    let parserTest = runParser (parseChar 'a') "abc"
    print parserTest  
    
    putStrLn "\nHC20T9: replicateMonad with Identity Monad"
    print $ runIdentity (replicateMonad 3 "hello")  
    
    putStrLn "\nHC20T11: randomWalk with State Monad"
    let walk = evalState (randomWalkDemo 3) (0,0)
    print walk
    
    putStrLn "\nHC20T13: fibonacciMemo (simplified)"
    let fib5 = evalState (fibonacciState 5) (0,1)
    print fib5
    
    putStrLn "\nHC20T14: mapMFilter"
    let testList = [1,2,3,4,5]
    let result14 = runIdentity $ mapMFilter 
          (\x -> return (x `mod` 2 == 0)) 
          (\x -> return (x * 2)) 
          testList
    print result14  
    
    putStrLn "\nHC20T15: treeSum"
    let testTree = Node (Leaf 1) (Node (Leaf 2) (Leaf 3))
    print $ treeSum testTree  
    
    putStrLn "\nHC20T17: validatePassword"
    print $ validatePassword "Weak"      
    print $ validatePassword "Strong123" 
    
    putStrLn "\nHC20T19: Writer Monad-based Logging System"
    let (result19, log19) = runWriter (addWithLog 3 4)
    print result19
    mapM_ putStrLn log19
    
