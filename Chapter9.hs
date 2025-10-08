--By Hulisani Muravha
module Main where
-- HC9T1: Define a Parametric Type Synonym
type Entity a = (a, String)

-- HC9T2: Implement a Parametric Data Type
data Box a = Empty | Has a
    deriving Show

-- HC9T3: Function to Add Values in a Box
addN :: Num a => a -> Box a -> Box a
addN _ Empty = Empty
addN n (Has x) = Has (x + n)

-- HC9T4: Extract a Value from a Box
extract :: a -> Box a -> a
extract def Empty = def
extract _ (Has x) = x

-- HC9T5: Parametric Data Type with Record Syntax
data Shape a = Circle { color :: a, radius :: Double }
             | Rectangle { color :: a, width :: Double, height :: Double }
    deriving Show

-- HC9T6: Recursive Data Type for Tweets
data Tweet = Tweet 
    { content :: String
    , likes :: Int
    , comments :: [Tweet]
    }
    deriving Show

-- HC9T7: Engagement Function for Tweets
engagement :: Tweet -> Int
engagement tweet = likes tweet + sum (map engagement (comments tweet))

-- HC9T8: Recursive Sequence Data Type
data Sequence a = End | Node a (Sequence a)
    deriving Show

-- HC9T9: Check for Element in a Sequence
elemSeq :: Eq a => a -> Sequence a -> Bool
elemSeq _ End = False
elemSeq x (Node y rest) = x == y || elemSeq x rest

-- HC9T10: Binary Search Tree Data Type
data BST a = EmptyBST | NodeBST a (BST a) (BST a)
    deriving Show

main :: IO ()
main = do
    -- HC9T1 Test
    putStrLn "HC9T1: Parametric Type Synonym"
    let entity1 :: Entity String = ("Person", "123 Main St")
    let entity2 :: Entity Int = (42, "456 Oak Ave")
    print entity1
    print entity2
    
    -- HC9T2 Test
    putStrLn "\nHC9T2: Parametric Data Type"
    let box1 = Empty :: Box Int
    let box2 = Has 42
    print box1
    print box2
    
    -- HC9T3 Test
    putStrLn "\nHC9T3: Add Values in a Box"
    let result1 = addN 10 Empty
    let result2 = addN 10 (Has 5)
    print result1
    print result2
    
    -- HC9T4 Test
    putStrLn "\nHC9T4: Extract Value from Box"
    let val1 = extract 0 Empty
    let val2 = extract 0 (Has 42)
    putStrLn $ "Extracted: " ++ show val1
    putStrLn $ "Extracted: " ++ show val2
    
    -- HC9T5 Test
    putStrLn "\nHC9T5: Parametric Shape with Record Syntax"
    let circle = Circle "red" 5.0
    let rectangle = Rectangle "blue" 10.0 6.0
    print circle
    print rectangle
    
    -- HC9T6 & HC9T7 Test
    putStrLn "\nHC9T6 & HC9T7: Recursive Tweets and Engagement"
    let tweet1 = Tweet "Hello World!" 10 []
    let tweet2 = Tweet "Nice day!" 25 [tweet1]
    let tweet3 = Tweet "Main tweet" 50 [tweet1, tweet2]
    print tweet3
    putStrLn $ "Engagement: " ++ show (engagement tweet3)
    
    -- HC9T8 & HC9T9 Test
    putStrLn "\nHC9T8 & HC9T9: Sequence and Element Check"
    let seq1 = Node 1 (Node 2 (Node 3 End))
    print seq1
    putStrLn $ "Contains 2: " ++ show (elemSeq 2 seq1)
    putStrLn $ "Contains 5: " ++ show (elemSeq 5 seq1)
    
    -- HC9T10 Test
    putStrLn "\nHC9T10: Binary Search Tree"
    let bst = NodeBST 5 
                (NodeBST 3 
                    (NodeBST 1 EmptyBST EmptyBST)
                    (NodeBST 4 EmptyBST EmptyBST))
                (NodeBST 8 
                    (NodeBST 7 EmptyBST EmptyBST)
                    (NodeBST 9 EmptyBST EmptyBST))
    print bst
