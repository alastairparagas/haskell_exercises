module Golf where

{-
  Exercise 1
  Your first task is to write a function
    skips :: [a] -> [[a]]
  The output of skips is a list of lists. The first list 
  in the output should be the same as the input list. 
  The second list in the output should contain every second 
  element from the input list. . . and the nth list in the 
  output should contain every nth element from the input list.
-}
skips :: [a] -> [[a]]
skips list = 
  let 
    -- Provided a list, constructs a list that skips over n elements
    listSkipper :: [a] -> Int -> Int -> [a]
    listSkipper list currentn n = case (list, currentn) of 
      ([], _) -> []
      (x:xs, 0) -> x : listSkipper xs n n
      (x:xs, _) -> listSkipper xs (currentn-1) n
    
    -- Provided a list, constructs a list of k-skipped-elements lists, 
    -- going from 0 skippings to n skippings (where n is the size of list)
    listOfSkippedLists :: [a] -> Int -> [[a]]
    listOfSkippedLists list n = case (length list > n) of 
      True -> listSkipper list n n : listOfSkippedLists list (n+1)
      False -> []
    
    in listOfSkippedLists list 0

{-
  Exercise 2
  A local maximum of a list is an element of the list which 
  is strictly greater than both the elements immediately before 
  and after it. For example, in the list [2,3,4,1,5], the only 
  local maximum is 4, since it is greater than the elements 
  immediately before and after it (3 and 1). 5 is not a local maximum 
  since there is no element that comes after it.
  Write a function
    localMaxima :: [Integer] -> [Integer]
  which finds all the local maxima in the input list and returns them 
  in order.
-}
localMaxima :: [Int] -> [Int]
localMaxima list = case list of
  x : y : z : xs -> 
    if x < y && y > z
    then y : localMaxima (z:xs)
    else localMaxima (y:z:xs)
  _ -> []

{-
  Exercise 3
  For this task, write a function
    histogram :: [Integer] -> String
  which takes as input a list of Integers between 0 and 9 
  (inclusive), and outputs a vertical histogram showing how 
  many of each number were in the input list. You may assume 
  that the input list does not contain any numbers less than 
  zero or greater than 9 (that is, it does not matter what your 
  function does if the input does contain such numbers).
-}
histogram :: [Int] -> String
histogram list = 
  let 
    histolistAdd :: [[Int]] -> Int -> [[Int]]
    histolistAdd histolist item = case histolist of 
      [] -> [[item]]
      x:xs -> case (elem item x) of 
        True -> x : histolistAdd xs item
        False -> (item : x) : xs
    
    histogramer :: [Int] -> [[Int]] -> [[Int]]
    histogramer list histolist = case list of 
      [] -> histolist
      x:xs -> histogramer xs $ histolistAdd histolist x
        
    visualization :: [[Int]] -> String
    visualization histogram = case histogram of 
      [] -> "==========\n0123456789\n"
      row:rows ->
        let 
          rowVisualizer :: [Int] -> String -> String 
          rowVisualizer row visual = case row of 
            [] -> visual
            x:xs -> 
              let 
                (part1, part2) = splitAt x visual
                in rowVisualizer xs (part1 ++ "*" ++ part2)
                
          defaultVisual :: String
          defaultVisual = take 10 (repeat ' ') ++ "\n"
          
          in rowVisualizer row defaultVisual ++ visualization rows
    
    in visualization (reverse (histogramer list []))
