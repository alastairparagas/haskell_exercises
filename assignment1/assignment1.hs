{- 
  Exercise 1
  Define the functions
    toDigits    :: Integer -> [Integer]
    toDigitsRev :: Integer -> [Integer]
  toDigits should convert positive Integers to a list of digits. (For 0 
  or negative inputs, toDigits should return the empty list.) 
  toDigitsRev should do the same, but with the digits reversed.
-}

toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0    = []
  | otherwise = toDigits (n `div` 10) ++ [(n `mod` 10)]
  
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0    = []
  | otherwise = [(n `mod` 10)] ++ toDigitsRev (n `div` 10)
  
toDigitsRevAlternate :: Integer -> [Integer]
toDigitsRevAlternate n = reverse (toDigits n)


{- 
  Exercise 2
  Define a function
    doubleEveryOther :: [Integer] -> [Integer]
  Remember that doubleEveryOther should double every other number 
  beginning from the right, that is, the second-to-last, 
  fourth-to-last, . . . numbers are doubled.
-}
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther ([])   = []
doubleEveryOther (x:[]) = [x]
doubleEveryOther (x:y:xs) = 
  if length xs `mod` 2 == 0 
  then [x*2,y] ++ doubleEveryOther xs 
  else [x,y*2] ++ doubleEveryOther xs


{-
  Exercise 3
  Define the function
    sumDigits :: [Integer] -> Integer
  to calculate the sum of all digits.
-}

sumDigits :: [Integer] -> Integer
sumDigits []      = 0
sumDigits (x:xs)  = sum (toDigits x) + sumDigits xs


{- 
  Exercise 4
  Define the function 
    validate :: Integer -> Bool 
  that indicates whether an Integer could be a valid credit card 
  number. This will use all functions defined in the previous 
  exercises.
-}
validate :: Integer -> Bool
validate ccNumber = 
  sumDigits (doubleEveryOther (toDigits ccNumber)) `mod` 10 == 0


{-
  Exercise 5
  Tower of Hanoi
    type Peg = String
    type Move = (Peg, Peg)
    hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
-}
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi discsCount sourcePeg destPeg sparePeg 
  | discsCount == 0   = []
  | discsCount == 1   = [(sourcePeg, destPeg)]
  | discsCount >= 2   = 
    hanoi (discsCount-1) sourcePeg sparePeg destPeg ++ 
    [(sourcePeg, destPeg)] ++ 
    hanoi (discsCount-1) sparePeg destPeg sourcePeg


{-
  Exercise 6
  Tower of Hanoi with 4 Pegs
    type Peg = String
    type Move = (Peg, Peg)
    hanoi :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
-}
hanoi4Pegs :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4Pegs discsCount sourcePeg destPeg sparePeg1 sparePeg2
  | discsCount == 0   = []
  | discsCount == 1   = [(sourcePeg, destPeg)]
  | discsCount == 2   = 
    [(sourcePeg, sparePeg1), (sourcePeg, destPeg), (sparePeg1, destPeg)]
  | discsCount >= 3   = 
    hanoi4Pegs (discsCount-2) sourcePeg sparePeg1 destPeg sparePeg2 ++ 
    hanoi4Pegs (discsCount-2) sourcePeg sparePeg2 destPeg sparePeg1 ++ 
    [(sourcePeg, destPeg)] ++ 
    hanoi4Pegs (discsCount-2) sparePeg2 destPeg sparePeg1 sourcePeg ++ 
    hanoi4Pegs (discsCount-2) sparePeg1 destPeg sparePeg2 sourcePeg
