---by Hulisani Muravha 
---Chapter11IO.hs

import Data.Char (toUpper)

-- HC11T1: Greet the User
main1 :: IO ()
main1 = do
  putStrLn "What is your name?"
  name <- getLine
  putStrLn ("Hello, " ++ name ++ "!")
  
-- HC11T2: Count Characters in a Line
main2 :: IO ()
main2 = do
  putStrLn "Enter a line:"
  line <- getLine
  putStrLn ("Number of characters: " ++ show (length line))
  
-- HC11T3: Double a Number
main3 :: IO ()
main3 = do
  putStrLn "Enter a number:"
  input <- getLine
  let number = read input :: Int
  print (number * 2)
  
-- HC11T4: Concatenate Two Lines

main4 :: IO ()
main4 = do
  putStrLn "Enter the first line:"
  line1 <- getLine
  putStrLn "Enter the second line:"
  line2 <- getLine
  putStrLn ("Combined: " ++ line1 ++ line2)
  
-- HC11T5: Repeat Until "quit"
main5 :: IO ()
main5 = do
  putStrLn "Type something (or 'quit' to stop):"
  loop
  where
    loop = do
      input <- getLine
      if input == "quit"
        then putStrLn "Goodbye!"
        else do
          putStrLn ("You typed: " ++ input)
          loop


-- HC11T6: Uppercase Converter
main6 :: IO ()
main6 = do
  putStrLn "Enter text:"
  line <- getLine
  putStrLn (map toUpper line)
  
-- HC11T7: User Options
main7 :: IO ()
main7 = do
  putStrLn "Choose an option:"
  putStrLn "1. Greet"
  putStrLn "2. Say Goodbye"
  putStrLn "3. Exit"
  choice <- getLine
  case choice of
    "1" -> putStrLn "Hello there!"
    "2" -> putStrLn "Goodbye!"
    "3" -> putStrLn "Exiting..."
    _   -> putStrLn "Invalid option."


-- HC11T8: Even or Odd Checker
main8 :: IO ()
main8 = do
  putStrLn "Enter a number:"
  input <- getLine
  let n = read input :: Int
  if even n
    then putStrLn "That number is even."
    else putStrLn "That number is odd."

    
-- HC11T9: Sum Two Numbers
main9 :: IO ()
main9 = do
  putStrLn "Enter first number:"
  a <- getLine
  putStrLn "Enter second number:"
  b <- getLine
  let sumResult = (read a :: Int) + (read b :: Int)
  putStrLn ("Sum = " ++ show sumResult)


-- HC11T10: Reverse User Input
main10 :: IO ()
main10 = do
  putStrLn "Enter text to reverse:"
  line <- getLine
  putStrLn ("Reversed: " ++ reverse line)


