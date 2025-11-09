
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Monoid (Sum(..), Product(..))
import qualified Data.Map as Map
import Data.Map (Map)

-- HC21T1:
newtype Writer w a = Writer { runWriter :: (a, w) }
tell :: Monoid w => w -> Writer w ()
tell w = Writer ((), w)
addW :: Int -> Int -> Writer [String] Int
addW x y = do
    tell ["Adding " ++ show x ++ " and " ++ show y]
    return (x + y)
subW :: Int -> Int -> Writer [String] Int
subW x y = do
    tell ["Subtracting " ++ show y ++ " from " ++ show x]
    return (x - y)
mulW :: Int -> Int -> Writer [String] Int
mulW x y = do
    tell ["Multiplying " ++ show x ++ " and " ++ show y]
    return (x * y)
calcDemo :: Writer [String] Int
calcDemo = do
    a <- addW 5 3
    b <- mulW a 2
    subW b 4

-- HC21T2:
instance Functor (Writer w) where
    fmap :: (a -> b) -> Writer w a -> Writer w b
    fmap f (Writer (a, w)) = Writer (f a, w)
instance Monoid w => Applicative (Writer w) where
    pure :: a -> Writer w a
    pure a = Writer (a, mempty)
    (<*>) :: Writer w (a -> b) -> Writer w a -> Writer w b
    (Writer (f, w1)) <*> (Writer (a, w2)) = Writer (f a, w1 <> w2)
instance Monoid w => Monad (Writer w) where
    (>>=) :: Writer w a -> (a -> Writer w b) -> Writer w b
    (Writer (a, w1)) >>= k = 
        let (Writer (b, w2)) = k a
        in Writer (b, w1 <> w2)

-- HC21T3:
listen :: Monoid w => Writer w a -> Writer w (a, w)
listen (Writer (a, w)) = Writer ((a, w), w)
pass :: Monoid w => Writer w (a, w -> w) -> Writer w a
pass (Writer ((a, f), w)) = Writer (a, f w)
redactSecrets :: Writer [String] a -> Writer [String] a
redactSecrets w = pass $ do
    (result, log) <- listen w
    let filteredLog = map (\line -> if "secret" `elem` words line then "[REDACTED]" else line) log
    return (result, const filteredLog)
secretDemo :: Writer [String] Int
secretDemo = do
    tell ["Normal message"]
    tell ["This is a secret message"]
    tell ["Another normal one"]
    return 42

-- HC21T4:
addWCount :: Int -> Int -> Writer (Sum Int) Int
addWCount x y = do
    tell (Sum 1)
    return (x + y)
mulWCount :: Int -> Int -> Writer (Sum Int) Int
mulWCount x y = do
    tell (Sum 1)
    return (x * y)
calcDemoCount :: Writer (Sum Int) Int
calcDemoCount = do
    a <- addWCount 5 3
    b <- mulWCount a 2
    addWCount b 4

-- HC21T5:
newtype Reader r a = Reader { runReader :: r -> a }
ask :: Reader r r
ask = Reader id
local :: (r -> r) -> Reader r a -> Reader r a
local f (Reader g) = Reader (g . f)
data Config = Config 
    { greetPrefix :: String
    , shout :: Bool
    } deriving (Show)
greet :: String -> Reader Config String
greet name = do
    config <- ask
    let baseGreeting = greetPrefix config ++ " " ++ name
    if shout config
        then return (map toUpper baseGreeting)
        else return baseGreeting
greetLoudly :: String -> Reader Config String
greetLoudly name = local (\c -> c { shout = True }) (greet name)

-- HC21T6: 
instance Functor (Reader r) where
    fmap f (Reader g) = Reader (f . g)
instance Applicative (Reader r) where
    pure a = Reader (\_ -> a)
    (Reader f) <*> (Reader g) = Reader (\r -> f r (g r))
instance Monad (Reader r) where
    (Reader f) >>= k = Reader (\r -> runReader (k (f r)) r)
getGreetingStyle :: Reader Config String
getGreetingStyle = do
    config <- ask
    return $ if shout config then "LOUD" else "normal"
composedGreeting :: String -> Reader Config String
composedGreeting name = do
    style <- getGreetingStyle
    greeting <- greet name
    return $ "[" ++ style ++ "] " ++ greeting

-- HC21T7: Reader—Mini "Env" Refactor
type Env = Map String String
-- Before refactoring
getDBPortExplicit :: Env -> Maybe String
getDBPortExplicit env = Map.lookup "DB_PORT" env
getDBHostExplicit :: Env -> Maybe String  
getDBHostExplicit env = Map.lookup "DB_HOST" env

-- After refactoring
getDBPort :: Reader Env (Maybe String)
getDBPort = Reader (Map.lookup "DB_PORT")
getDBHost :: Reader Env (Maybe String)
getDBHost = Reader (Map.lookup "DB_HOST")
getDBConfig :: Reader Env String
getDBConfig = do
    port <- getDBPort
    host <- getDBHost
    return $ "Host: " ++ show host ++ ", Port: " ++ show port
getDBConfigOverride :: Reader Env String
getDBConfigOverride = local (Map.insert "DB_PORT" "5432") getDBConfig

-- HC21T8:
newtype State s a = MkState { runState :: s -> (a, s) }
instance Functor (State s) where
    fmap f (MkState g) = MkState $ \s ->
        let (a, s') = g s
        in (f a, s')
instance Applicative (State s) where
    pure a = MkState (\s -> (a, s))
    (MkState f) <*> (MkState g) = MkState $ \s ->
        let (h, s') = f s
            (a, s'') = g s'
        in (h a, s'')
instance Monad (State s) where
    (MkState f) >>= k = MkState $ \s ->
        let (a, s') = f s
            (MkState g) = k a
        in g s'
get :: State s s
get = MkState (\s -> (s, s))
put :: s -> State s ()
put s = MkState (\_ -> ((), s))
modify :: (s -> s) -> State s ()
modify f = MkState (\s -> ((), f s))

-- HC21T9: 
mapCount :: (a -> b) -> [a] -> State Int [b]
mapCount _ [] = return []
mapCount f (x:xs) = do
    count <- get
    put (count + 1)
    rest <- mapCount f xs
    return (f x : rest)

-- HC21T10:
data VendingState = MkVendingState
    { items :: Int
    , credit :: Int
    } deriving (Show)

insertCoin :: Int -> State VendingState ()
insertCoin amount = modify (\s -> s { credit = credit s + amount })

vend :: State VendingState String
vend = do
    state <- get
    if items state > 0 && credit state >= 50
        then do
            put $ state { items = items state - 1, credit = credit state - 50 }
            return "Item vended!"
        else return "Cannot vend - check items and credit"

getChange :: State VendingState Int
getChange = do
    state <- get
    put $ state { credit = 0 }
    return (credit state)

vendingSequence :: State VendingState String
vendingSequence = do
    insertCoin 25
    insertCoin 25
    insertCoin 10
    result <- vend
    change <- getChange
    return $ result ++ " Change: " ++ show change

-- HC21T11:
type UndoState = (Int, [Int])
setValue :: Int -> State UndoState ()
setValue newVal = do
    (current, history) <- get
    put (newVal, current : history)
undo :: State UndoState ()
undo = do
    (current, history) <- get
    case history of
        [] -> return ()
        (prev:rest) -> put (prev, rest)
undoDemo :: State UndoState (Int, [Int])
undoDemo = do
    setValue 10
    setValue 20
    setValue 30
    undo
    setValue 25
    undo
    get

-- HC21T12:
type WalkState = (Int, (Int, Int))

randomStep :: State WalkState ()
randomStep = do
    (seed, (x, y)) <- get
    let newSeed = (seed * 1664525 + 1013904223) `mod` (2^32)
    let direction = newSeed `mod` 4
    let newPos = case direction of
                    0 -> (x + 1, y)    
                    1 -> (x - 1, y)    
                    2 -> (x, y + 1)    
                    3 -> (x, y - 1)    
    put (newSeed, newPos)
randomWalk :: Int -> State WalkState [(Int, Int)]
randomWalk 0 = do
    (_, pos) <- get
    return [pos]
randomWalk n = do
    (_, pos) <- get
    randomStep
    rest <- randomWalk (n - 1)
    return (pos : rest)

-- HC21T13:
step :: String -> Reader Config (Writer [String] ())
step msg = do
    config <- ask
    let prefix = greetPrefix config
    return $ tell [prefix ++ ": " ++ msg]
loggingDemo :: Reader Config (Writer [String] ())
loggingDemo = do
    step1 <- step "Starting process"
    step2 <- step "Processing data"
    step3 <- step "Finished"
    return $ step1 >> step2 >> step3

-- HC21T14:
type InstrumentedState1 s a = State s (Writer [String] a)
inc1 :: Int -> InstrumentedState1 Int Int
inc1 amount = do
    current <- get
    put (current + amount)
    return $ Writer (current + amount, ["Incremented by " ++ show amount])

-- HC21T15:
tick :: Reader Config (State Int Bool)
tick = do
    config <- ask
    let threshold = case greetPrefix config of
                    "High" -> 5
                    "Low" -> 2
                    _ -> 3
    return $ do
        modify (+1)
        current <- get
        return (current >= threshold)
runTicks :: Config -> Int -> (Int, [Bool])
runTicks config n = runState (sequence (replicate n (runReader tick config))) 0

-- HC21T16:
testWriterAssociativity :: Bool
testWriterAssociativity =
    let m = Writer (1, ["start"])
        k x = Writer (x + 1, ["k:" ++ show x])
        h x = Writer (x * 2, ["h:" ++ show x])
        left = (m >>= k) >>= h
        right = m >>= (\x -> k x >>= h)
    in runWriter left == runWriter right

testReaderLocalId :: Bool
testReaderLocalId =
    let reader = Reader (\r -> r + 1)
        left = runReader (local id reader) 5
        right = runReader reader 5
    in left == right

-- HC21T17:
legacyFunction :: Env -> [String] -> Int -> (String, [String], Int)
legacyFunction env logs counter =
    let log1 = "Starting with counter: " ++ show counter
        logs' = log1 : logs
        counter' = counter + 1
        
        dbHost = case Map.lookup "DB_HOST" env of
            Just host -> host
            Nothing -> "localhost"
        log2 = "DB host: " ++ dbHost
        logs'' = log2 : logs'
        counter'' = counter' + 1
        
        result = "Final count: " ++ show counter''
        logs''' = ("Finished: " ++ result) : logs''
    in (result, reverse logs''', counter'')

refactoredFunction :: Reader Env (Writer [String] (State Int String))
refactoredFunction = do
    env <- ask
    return $ do
        tell ["Starting process"]
        counter <- get
        tell ["Initial counter: " ++ show counter]
        
        modify (+1)
        dbHost <- return $ Map.lookup "DB_HOST" env `orElse` "localhost"
        tell ["DB host: " ++ dbHost]
      
        modify (+1)
        finalCount <- get
        tell ["Finished with count: " ++ show finalCount]
        return ("Final count: " ++ show finalCount)
  where
    orElse (Just x) _ = x
    orElse Nothing y = y
runRefactored :: Env -> Int -> (String, [String], Int)
runRefactored env startCounter =
    let readerAction = refactoredFunction
        writerAction = runReader readerAction env
        stateAction = runWriter writerAction
        (finalStateAction, logs) = stateAction
        (result, finalCounter) = runState finalStateAction startCounter
    in (result, logs, finalCounter)


main :: IO ()
main = do
    putStrLn "HC21T1: Logging Calculator"
    print $ runWriter calcDemo
    
    putStrLn "\nHC21T3: Secret Redaction"
    print $ runWriter secretDemo
    print $ runWriter (redactSecrets secretDemo)
    
    putStrLn "\nHC21T4: Counting Operations"
    print $ runWriter calcDemoCount
    
    putStrLn "\nHC21T5: Configurable Greeting"
    let config = Config "Hello" False
    print $ runReader (greet "Alice") config
    print $ runReader (greetLoudly "Alice") config
    
    putStrLn "\nHC21T7: Env Refactor"
    let env = Map.fromList [("DB_HOST", "localhost"), ("DB_PORT", "5432")]
    print $ runReader getDBConfig env
    print $ runReader getDBConfigOverride env
    
    putStrLn "\nHC21T9: Counter Mapping"
    print $ runState (mapCount (*2) [1,2,3,4]) 0
    
    putStrLn "\nHC21T10: Vending Machine"
    let initialState = MkVendingState { items = 5, credit = 0 }
    print $ runState vendingSequence initialState
    
    putStrLn "\nHC21T11: Undo Stack"
    print $ runState undoDemo (0, [])
    
    putStrLn "\nHC21T12: Random Walk"
    let walkStart = (42, (0, 0))
    print $ runState (randomWalk 5) walkStart
    
    putStrLn "\nHC21T13: Reader + Writer Logging"
    let loggingConfig = Config "LOG" False
    let (writerAction, _) = runReader loggingDemo loggingConfig
    print $ runWriter writerAction
    
    putStrLn "\nHC21T15: Environment-Driven State"
    let highConfig = Config "High" False
    let lowConfig = Config "Low" False
    print $ runTicks highConfig 6
    print $ runTicks lowConfig 6
    
    putStrLn "\nHC21T16: Monad Laws"
    print $ testWriterAssociativity
    print $ testReaderLocalId
    
    putStrLn "\nHC21T17: Refactoring Demo"
    let testEnv = Map.fromList [("DB_HOST", "prod-db")]
    print $ legacyFunction testEnv [] 0
    print $ runRefactored testEnv 0
    
