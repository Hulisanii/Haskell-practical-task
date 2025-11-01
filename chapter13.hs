
-- By Hulisani Muravha
module Main where

-- Chapter 13: Working with Modules and Directories
-- Tasks: HC13T1 – HC13T10

import System.Directory
import Data.List
import qualified Data.Map as Map
import qualified SumNonEmpty as SN
import qualified Data.List as L  -- for renaming and qualified import demo
import qualified Data.Char as C  -- another qualified import for conflict demo

-- HC13T1: List Files in Directory
listFilesInDirectory :: IO [FilePath]
listFilesInDirectory = listDirectory "."

-- HC13T2: Filter Files by Substring
filterFilesBySubstring :: String -> IO [FilePath]
filterFilesBySubstring substring = do
    files <- listFilesInDirectory
    return (filter (isInfixOf substring) files)

-- HC13T3: Sort and Return Filtered Files
sortFilteredFiles :: String -> IO [FilePath]
sortFilteredFiles substring = do
    filtered <- filterFilesBySubstring substring
    return (sort filtered)

-- HC13T6: File Names to Map
filesToMap :: [FilePath] -> Map.Map Int FilePath
filesToMap files = Map.fromList (zip [1..] files)

-- HC13T8: Qualified Imports for Name Conflicts
-- Example showing that we can use L.sort from Data.List and C.toUpper from Data.Char
qualifiedImportDemo :: IO ()
qualifiedImportDemo = do
    let wordsList = ["banana", "apple", "cherry"]
    putStrLn $ "Sorted list (using Data.List.sort): " ++ show (L.sort wordsList)
    putStrLn $ "Uppercased 'haskell' (using Data.Char.toUpper): " ++ map C.toUpper "haskell"

-- HC13T9: Renaming Module Namespace
-- Demonstrating module renaming for Data.List as DL and Data.Char as DC
import qualified Data.List as DL
import qualified Data.Char as DC

renamingDemo :: IO ()
renamingDemo = do
    let text = "guardianpro"
    putStrLn $ "DL.sort example: " ++ show (DL.sort ["zebra","lion","elephant"])
    putStrLn $ "DC.toUpper example: " ++ map DC.toUpper text

-- HC13T10: Multi-Module Main Function
-- Combines directory listing and sorting (System.Directory + Data.List)
multiModuleSearchAndSort :: String -> IO ()
multiModuleSearchAndSort substring = do
    putStrLn $ "Searching for files containing \"" ++ substring ++ "\"..."
    filtered <- sortFilteredFiles substring
    if null filtered
      then putStrLn "No files found."
      else do
          putStrLn "Sorted filtered files:"
          mapM_ putStrLn filtered

-- MAIN DEMONSTRATION (calls tasks HC13T1–HC13T10)
main :: IO ()
main = do
    putStrLn "----- HC13T1: List Files in Current Directory -----"
    files <- listFilesInDirectory
    print files

    putStrLn "\n----- HC13T2: Filter Files by Substring 'hs' -----"
    filtered <- filterFilesBySubstring "hs"
    print filtered

    putStrLn "\n----- HC13T3: Sort and Return Filtered Files -----"
    sorted <- sortFilteredFiles "hs"
    print sorted

    putStrLn "\n----- HC13T4 & HC13T5: SumNonEmpty Module Test -----"
    putStrLn $ "Sum of [5,10,15] = " ++ show (SN.sumNonEmpty [5,10,15])
    putStrLn $ "Sum of [] (expecting error message):"
    -- This will raise an error if uncommented, handled for demonstration
    -- print (SN.sumNonEmpty [])

    putStrLn "\n----- HC13T6: File Names to Map -----"
    let fileMap = filesToMap sorted
    print fileMap

    putStrLn "\n----- HC13T7: Use Custom Module in Main -----"
    putStrLn $ "SumNonEmpty [1,2,3,4,5] = " ++ show (SN.sumNonEmpty [1,2,3,4,5])

    putStrLn "\n----- HC13T8: Qualified Imports for Name Conflicts -----"
    qualifiedImportDemo

    putStrLn "\n----- HC13T9: Renaming Module Namespace -----"
    renamingDemo

    putStrLn "\n----- HC13T10: Multi-Module Main Function -----"
    multiModuleSearchAndSort "hs"

    putStrLn "\nProgram execution completed."



SumNonEmpty.hs
-- By Hulisani Muravha
-- HC13T4 & HC13T5: SumNonEmpty Module

module SumNonEmpty (sumNonEmpty) where

-- Exports only sumNonEmpty (helper functions are private)

sumNonEmpty :: (Num a, Show a) => [a] -> a
sumNonEmpty []  = error "Error: sumNonEmpty called on an empty list!"
sumNonEmpty xs  = sum xs
