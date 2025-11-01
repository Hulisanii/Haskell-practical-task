-- HC18.hs
-- Chapter 18: Functors and fmap
-- By Hulisani Muravha

module Main where

import Data.Char (toLower)

-----------------------------------
-- HC18T1: mapToLower Function with fmap
-----------------------------------
mapToLower :: String -> String
mapToLower = fmap toLower

-----------------------------------
-- HC18T2: Functor Instance for Tree
-----------------------------------
data Tree a = Empty | Node a (Tree a) (Tree a)
    deriving (Show)

instance Functor Tree where
    fmap _ Empty = Empty
    fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)

-----------------------------------
-- HC18T3: incrementTreeValues Function
-----------------------------------
incrementTreeValues :: Num a => Tree a -> Tree a
incrementTreeValues = fmap (+1)

-----------------------------------
-- HC18T4: mapToBits Function
-----------------------------------
mapToBits :: [Bool] -> [Char]
mapToBits = fmap (\b -> if b then '1' else '0')

-----------------------------------
-- HC18T5: Functor Instance for Either
-----------------------------------
instance Functor (Either e) where
    fmap _ (Left e) = Left e
    fmap f (Right x) = Right (f x)

-----------------------------------
-- HC18T6: applyToMaybe Function
-----------------------------------
applyToMaybe :: (a -> b) -> Maybe a -> Maybe b
applyToMaybe = fmap

-----------------------------------
-- HC18T7: fmapTuple Function
-----------------------------------
fmapTuple :: (b -> c) -> (a, b) -> (a, c)
fmapTuple = fmap

-----------------------------------
-- HC18T8: identityLawCheck Function
-----------------------------------
identityLawCheck :: (Eq (f a), Functor f) => f a -> Bool
identityLawCheck x = fmap id x == x

-----------------------------------
-- HC18T9: compositionLawCheck Function
-----------------------------------
compositionLawCheck :: (Eq (f c), Functor f) => (b -> c) -> (a -> b) -> f a -> Bool
compositionLawCheck f g x = fmap (f . g) x == (fmap f . fmap g) x

-----------------------------------
-- HC18T10: nestedFmap Function
-----------------------------------
nestedFmap :: (a -> b) -> [[a]] -> [[b]]
nestedFmap = fmap . fmap

-----------------------------------
-- main Function: Demonstration
-----------------------------------
main :: IO ()
main = do
    putStrLn "--- HC18T1: mapToLower ---"
    print $ mapToLower "HELLO HASKELL"

    putStrLn "\n--- HC18T2 & HC18T3: Functor Tree + Increment ---"
    let tree = Node 5 (Node 3 Empty Empty) (Node 7 Empty Empty)
    print tree
    print $ incrementTreeValues tree

    putStrLn "\n--- HC18T4: mapToBits ---"
    print $ mapToBits [True, False, True, True]

    putStrLn "\n--- HC18T5: Functor Either ---"
    print (fmap (+3) (Right 7 :: Either String Int))
    print (fmap (+3) (Left "Error" :: Either String Int))

    putStrLn "\n--- HC18T6: applyToMaybe ---"
    print $ applyToMaybe (*2) (Just 10)
    print $ applyToMaybe (*2) Nothing

    putStrLn "\n--- HC18T7: fmapTuple ---"
    print $ fmapTuple length ("Haskell", "Fun")

    putStrLn "\n--- HC18T8: identityLawCheck ---"
    print $ identityLawCheck [1, 2, 3]
    print $ identityLawCheck (Just 5)

    putStrLn "\n--- HC18T9: compositionLawCheck ---"
    print $ compositionLawCheck (*2) (+3) [1, 2, 3]

    putStrLn "\n--- HC18T10: nestedFmap ---"
    print $ nestedFmap (+1) [[1,2,3], [4,5,6]]

    putStrLn "\nAll Chapter 18 Functor tasks completed successfully!"
