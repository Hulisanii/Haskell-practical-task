
{-# LANGUAGE FlexibleInstances #-}
import System.IO
import System.IO.Error
import Text.Read (readMaybe)
import Control.Exception (Exception, throw, try, catch, SomeException(..))

-- HC15T3: Custom Exception for Traffic Light Errors
data TrafficLightError = InvalidLightColor String | SystemMalfunction String
    deriving (Show)

instance Exception TrafficLightError

-- HC15T1: Handle Exceptions for File Reading and Velocity Calculation
velocityCalculation :: IO ()
velocityCalculation = do
    putStrLn "=== HC15T1: Velocity Calculation with Exception Handling ==="
    putStr "Enter distance (meters): "
    distanceInput <- getLine
    putStr "Enter time (seconds): "
    timeInput <- getLine
    
    let maybeDistance = readMaybe distanceInput
    let maybeTime = readMaybe timeInput
    
    case (maybeDistance, maybeTime) of
        (Just distance, Just time) -> do
            if time <= 0
                then putStrLn "Error: Time must be positive"
                else do
                    let velocity = distance / time
                    putStrLn $ "Velocity: " ++ show velocity ++ " m/s"
                    
                    -- Write result to file with exception handling
                    result <- try (writeFile "velocity_result.txt" 
                                 ("Distance: " ++ show distance ++ 
                                  ", Time: " ++ show time ++ 
                                  ", Velocity: " ++ show velocity ++ " m/s")) 
                                 :: IO (Either IOError ())
                    case result of
                        Left err -> putStrLn $ "File write error: " ++ show err
                        Right _ -> putStrLn "Result saved to velocity_result.txt"
                        
        (Nothing, _) -> putStrLn "Error: Invalid distance input"
        (_, Nothing) -> putStrLn "Error: Invalid time input"

-- HC15T2: Self-Driving AI Car System
data TrafficLight = Red | Yellow | Green deriving (Show, Eq)

carReaction :: TrafficLight -> String
carReaction Red = "STOP: Applying brakes"
carReaction Yellow = "CAUTION: Preparing to stop"
carReaction Green = "GO: Maintaining speed"

selfDrivingCar :: String -> IO ()
selfDrivingCar color = do
    putStrLn $ "\nTraffic light: " ++ color
    case color of
        "red" -> putStrLn $ carReaction Red
        "yellow" -> putStrLn $ carReaction Yellow
        "green" -> putStrLn $ carReaction Green
        _ -> throw $ InvalidLightColor $ "Unknown traffic light color: " ++ color

-- HC15T4: Exception Handler for Traffic Light
trafficLightHandler :: TrafficLightError -> IO ()
trafficLightHandler (InvalidLightColor msg) = 
    putStrLn $ "Traffic System Alert: " ++ msg
trafficLightHandler (SystemMalfunction msg) = 
    putStrLn $ "Car System Error: " ++ msg

handleTrafficLight :: String -> IO ()
handleTrafficLight color = 
    catch (selfDrivingCar color) 
          (\(e :: TrafficLightError) -> trafficLightHandler e)

-- HC15T5: Safe Division Using Maybe
safeDivide :: Double -> Double -> Maybe Double
safeDivide _ 0 = Nothing
safeDivide x y = Just (x / y)

-- HC15T6: Safe Input Parsing with readMaybe
safeReadDouble :: String -> Maybe Double
safeReadDouble = readMaybe

getSafeInput :: String -> IO (Maybe Double)
getSafeInput prompt = do
    putStr prompt
    input <- getLine
    return $ safeReadDouble input

-- HC15T7: Velocity Calculation with Optionals and Parsing Handling
velocityWithOptionals :: IO ()
velocityWithOptionals = do
    putStrLn "\n=== HC15T7: Velocity with Optional Values ==="
    maybeDistance <- getSafeInput "Enter distance (meters): "
    maybeTime <- getSafeInput "Enter time (seconds): "
    
    case (maybeDistance, maybeTime) of
        (Just distance, Just time) -> 
            case safeDivide distance time of
                Just velocity -> putStrLn $ "Velocity: " ++ show velocity ++ " m/s"
                Nothing -> putStrLn "Error: Division by zero (time cannot be zero)"
        (Nothing, _) -> putStrLn "Error: Invalid distance input"
        (_, Nothing) -> putStrLn "Error: Invalid time input"

-- HC15T8: Division with Either for Detailed Errors
data DivisionError = DivideByZero | NegativeTime | InvalidInput String
    deriving (Show)

divideEither :: Double -> Double -> Either DivisionError Double
divideEither _ 0 = Left DivideByZero
divideEither _ time | time < 0 = Left NegativeTime
divideEither distance time = Right (distance / time)

safeDivideWithEither :: String -> String -> Either DivisionError Double
safeDivideWithEither distStr timeStr = do
    distance <- case readMaybe distStr of
        Just d -> Right d
        Nothing -> Left $ InvalidInput "Invalid distance format"
    
    time <- case readMaybe timeStr of
        Just t -> Right t
        Nothing -> Left $ InvalidInput "Invalid time format"
    
    divideEither distance time

-- HC15T9: Try Function for File IO Exceptions
readFileSafely :: FilePath -> IO (Either String String)
readFileSafely filename = do
    result <- try (readFile filename) :: IO (Either IOError String)
    case result of
        Left err -> return $ Left $ "File error: " ++ show err
        Right content -> return $ Right content

fileReadingDemo :: IO ()
fileReadingDemo = do
    putStrLn "\n=== HC15T9: Safe File Reading ==="
    putStr "Enter filename to read: "
    filename <- getLine
    
    fileResult <- readFileSafely filename
    case fileResult of
        Left errMsg -> putStrLn errMsg
        Right content -> do
            putStrLn "File content:"
            putStrLn content

-- HC15T10: Hybrid Error Handling with Either and IO
hybridVelocityProgram :: IO ()
hybridVelocityProgram = do
    putStrLn "\n=== HC15T10: Hybrid Error Handling ==="
    
    -- Get inputs with IO exception handling
    (distanceInput, timeInput) <- catch getInputs handleIOError
    
    -- Process with Either for domain logic
    case safeDivideWithEither distanceInput timeInput of
        Left DivideByZero -> putStrLn "Error: Time cannot be zero"
        Left NegativeTime -> putStrLn "Error: Time cannot be negative"
        Left (InvalidInput msg) -> putStrLn $ "Input error: " ++ msg
        Right velocity -> do
            putStrLn $ "Calculated velocity: " ++ show velocity ++ " m/s"
            
            -- Save with IO exception handling
            saveResult <- try (saveVelocityToFile distanceInput timeInput velocity) 
                           :: IO (Either IOError ())
            case saveResult of
                Left err -> putStrLn $ "Save failed: " ++ show err
                Right _ -> putStrLn "Result saved successfully"
  where
    getInputs :: IO (String, String)
    getInputs = do
        putStr "Enter distance: "
        dist <- getLine
        putStr "Enter time: "
        time <- getLine
        return (dist, time)
    
    handleIOError :: IOError -> IO (String, String)
    handleIOError err = do
        putStrLn $ "IO Error during input: " ++ show err
        return ("0", "1")  -- Default values
    
    saveVelocityToFile :: String -> String -> Double -> IO ()
    saveVelocityToFile dist time vel = do
        let content = "Distance: " ++ dist ++ ", Time: " ++ time ++ 
                     ", Velocity: " ++ show vel ++ " m/s\n"
        appendFile "velocity_log.txt" content

-- Main function to demonstrate all tasks
main :: IO ()
main = do
    -- HC15T1: File Reading and Velocity Calculation
    velocityCalculation
    
    -- HC15T2 & HC15T3 & HC15T4: Self-Driving Car with Exception Handling
    putStrLn "\n=== HC15T2-4: Self-Driving Car System ==="
    mapM_ handleTrafficLight ["red", "green", "yellow", "blue"]
    
    -- HC15T5: Safe Division Demo
    putStrLn "\n=== HC15T5: Safe Division with Maybe ==="
    putStrLn $ "10 / 2 = " ++ show (safeDivide 10 2)
    putStrLn $ "10 / 0 = " ++ show (safeDivide 10 0)
    
    -- HC15T6 & HC15T7: Safe Input and Velocity Calculation
    velocityWithOptionals
    
    -- HC15T8: Division with Either
    putStrLn "\n=== HC15T8: Division with Either ==="
    putStrLn $ "10 / 2: " ++ show (safeDivideWithEither "10" "2")
    putStrLn $ "10 / 0: " ++ show (safeDivideWithEither "10" "0")
    putStrLn $ "10 / -5: " ++ show (safeDivideWithEither "10" "-5")
    putStrLn $ "abc / 2: " ++ show (safeDivideWithEither "abc" "2")
    
    -- HC15T9: File Reading with try
    fileReadingDemo
    
    -- HC15T10: Hybrid Error Handling
    hybridVelocityProgram
    
    putStrLn "\nAll error handling demonstrations completed!"

-- Additional utility functions
demoSafeDivision :: IO ()
demoSafeDivision = do
    putStrLn "\n--- Safe Division Examples ---"
    let examples = [(10, 2), (15, 3), (8, 0), (20, 4)]
    mapM_ (\(x, y) -> 
        putStrLn $ show x ++ " / " ++ show y ++ " = " ++ 
                 case safeDivide x y of
                    Just result -> show result
                    Nothing -> "undefined (division by zero)"
        ) examples

demoTrafficLightSystem :: IO ()
demoTrafficLightSystem = do
    putStrLn "\n--- Traffic Light System Test ---"
    let testLights = [Red, Yellow, Green]
    mapM_ (\light -> putStrLn $ show light ++ " -> " ++ carReaction light) testLights
