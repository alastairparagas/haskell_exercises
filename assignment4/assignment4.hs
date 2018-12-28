module Assignment4 where
import Data.List (foldl', foldr)
import Test.QuickCheck (quickCheck, forAll, choose, sublistOf)

{-
  Exercise 1
  Reimplement each of the following functions in a more idiomatic
  Haskell style. Use wholemeal programming practices, breaking each function 
  into a pipeline of incremental transformations to an entire data structure. 
  Name your functions fun1’ and fun2’ respectively.
-}

-- Unidiomatic Function 1 implementation
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs) 
  | even x    = (x - 2) * fun1 xs 
  | otherwise = fun1 xs

-- Idiomatic Function 1 solution
fun1' :: [Integer] -> Integer
fun1' = (foldl' (*) 1) . map (\x -> x - 2) . filter even

-- Unidiomatic Function 2 implementation
fun2 :: Integer -> Integer 
fun2 1 = 0
fun2 n
 | even n = n + fun2 (n `div` 2) 
 | otherwise = fun2 (3 * n + 1)

-- idiomatic Function 2 solution
fun2' :: Integer -> Integer
fun2' = (foldl' (+) 0) . 
  filter even .
  takeWhile (\x -> x /= 1) . 
  iterate (\x -> 
    if even x 
    then x `div` 2 
    else 3 * x + 1
  )
  
  
{- 
  Exercise 2: Folding with trees
  Recall the definition of a binary tree data structure. The height of
  a binary tree is the length of a path from the root to the deepest node. 
  For example, the height of a tree with a single node is 0; the height of a tree with 
  three nodes, whose root has two children, is 1; and so on. A binary tree is balanced 
  if the height of its left and right subtrees differ by no more than 1, and its left and 
  right subtrees are also balanced. You should use the following data structure to represent 
  binary trees. Note that each node stores an extra Integer representing the height at that node.
-}

data Tree a = 
  Leaf | Node Integer (Tree a) a (Tree a) 
  deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = 
  let 
    balancedInsert :: a -> Tree a -> Tree a
    balancedInsert newElement tree = case tree of
      Leaf -> 
        Node 0 Leaf newElement Leaf
      Node 0 Leaf currentVal rightSubtree -> 
        Node 1 (Node 0 Leaf newElement Leaf) currentVal rightSubtree
      Node 1 leftSubtree currentVal Leaf -> 
        Node 1 leftSubtree currentVal (Node 0 Leaf newElement Leaf)
      Node 
        someHeight 
        leftSubtree@(Node leftSubtreeHeight _ _ _) 
        currentVal
        rightSubtree@(Node rightSubtreeHeight _ _ _) -> 
          if leftSubtreeHeight <= rightSubtreeHeight
          then case balancedInsert newElement leftSubtree of 
            newLeftSubtree@(Node leftSubtreeHeight _ _ _) -> 
              Node (leftSubtreeHeight + 1) newLeftSubtree currentVal rightSubtree
            _ -> 
              Node someHeight Leaf currentVal rightSubtree
          else case balancedInsert newElement rightSubtree of 
            newRightSubtree@(Node rightSubtreeHeight _ _ _) -> 
              Node (rightSubtreeHeight + 1) leftSubtree currentVal newRightSubtree
            _ ->
              Node someHeight leftSubtree currentVal Leaf
  in foldr balancedInsert Leaf 


{-
  Exercise 3: More folds!
  Implement a function
     xor :: [Bool] -> Bool
  which returns True if and only if there are an odd number of True values 
  contained in the input list. It does not matter how many False values the 
  input list contains. For example,
    xor [False, True, False] == True
    xor [False, True, False, False, True] == False 
  Your solution must be implemented using a fold.
-}

xor' :: [Bool] -> Bool
xor' = odd . length . filter (\x -> x == True)

xor :: [Bool] -> Bool
xor = snd . foldl' (\(amountOfTrues, _) currentBool -> 
                    let 
                      newAmountOfTrues = 
                        if currentBool == True 
                        then amountOfTrues + 1
                        else amountOfTrues
                    in (newAmountOfTrues, odd newAmountOfTrues)) (0, False)
                    
                    
{-
  Exercise 3: More folds!
  Implement map as a fold
-}
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\currentElement aggregate -> f currentElement : aggregate) []


{-
  Exercise 3: More folds!
  Implement foldl using foldr. That is, complete the definition
    myFoldl :: (a -> b -> a) -> a -> [b] -> a
    myFoldl f base xs = foldr ...
  in such a way that myFoldl behaves identically to the standard 
  foldl function.
-}
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (\x y -> f y x) base xs


{-
  Exercise 4: Finding primes
  Read about the Sieve of Sundaram. Implement the algorithm 
  using function composition. Given an integer n, your function 
  should generate all the odd prime numbers up to 2n + 2.
    sieveSundaram :: Integer -> [Integer]
    sieveSundaram = ...
-}
sieveSundaram :: Integer -> [Integer]
sieveSundaram = filter (\n -> 
                        let
                          factorsOfN = [x | x <- [1..n], mod n x == 0]
                        in factorsOfN == [1, n]
                        ) . filter odd . \n -> [0..2*n+2]


-- Unit tests
main = 
  let 
    testExercise1Fun1 :: [Integer] -> Bool
    testExercise1Fun1 xs = fun1 xs == fun1' xs
    
    testExercise1Fun2 :: Integer -> Bool
    testExercise1Fun2 someNum = fun2 someNum == fun2' someNum
    
    testExercise2 :: String -> Bool
    testExercise2 aList = case foldTree aList of 
      Node rootHeight leftSubtree _ rightSubtree -> let
        checkIfBalanced tree1 tree2 = case (tree1, tree2) of 
          (Leaf, Leaf) -> True
          (Node 0 _ _ _, Leaf) -> True
          (Leaf, Node 0 _ _ _) -> True
          (Node leftSubtreeHeight leftleftSubtree _ leftrightSubtree, 
           Node rightSubtreeHeight rightleftSubtree _ rightrightSubtree) -> 
            let 
              heightDifference = leftSubtreeHeight - rightSubtreeHeight
            in (heightDifference >= -1) && 
              (heightDifference <= 1) && 
              checkIfBalanced leftleftSubtree leftrightSubtree && 
              checkIfBalanced rightleftSubtree rightrightSubtree
        in checkIfBalanced leftSubtree rightSubtree
      Leaf -> length aList == 0
      
    testExercise3Xor :: [Bool] -> Bool
    testExercise3Xor bool = 
      xor' bool == xor bool
    
    testExercise3Map' :: [Integer] -> Bool
    testExercise3Map' listOfInteger = 
      map' (\x -> x + 1) listOfInteger == map (\x -> x + 1) listOfInteger
      
    testExercise3MyFoldl :: [Integer] -> Bool 
    testExercise3MyFoldl xs = 
      let 
        someFunc = (\agg currElem -> currElem + 1 : agg)
      in foldl' someFunc [] xs == myFoldl someFunc [] xs
  in do {
    quickCheck testExercise1Fun1;
    quickCheck (forAll (choose (0, 1000)) testExercise1Fun2);
    quickCheck (forAll (sublistOf (take 10 $ repeat '0')) testExercise2);
    quickCheck testExercise3Xor;
    quickCheck testExercise3Map';
    quickCheck testExercise3MyFoldl
  }
