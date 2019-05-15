{-# LANGUAGE FlexibleInstances #-}
import Data.List (foldl')
import Test.QuickCheck (quickCheck, forAll, choose)


{-
  Exercise 1
  A recursive function definition of type
    fib :: Integer -> Integer
  so that fib n computes the nth Fibonacci number Fn.
  Now use fib to define the infinite list of all Fibonacci numbers 
    fibs1 :: [Integer]
-}
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

{-
  Exercise 2
  Define the infinite list
    fibs2 :: [Integer]
  so that it has the same elements as fibs1, but computing the 
  first n elements of fibs2 requires only O(n) addition operations.
-}
fibs2 :: [Integer]
fibs2 = map (\(x, y) -> x) $ iterate (\(x, y) -> (y, x + y)) (0, 1)

{-
  Exercise 3
  • Define a data type of polymorphic streams, Stream.
  • Write a function to convert a Stream to an infinite list,
     streamToList :: Stream a -> [a]
  • instance Show a => Show (Stream a) where
       show ...
    which works by showing only some prefix of a stream 
    (say, the first 20 elements).
-}
data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons a leftOverStream) = a : streamToList leftOverStream

instance Show a => Show (Stream a) where
  show a = show $ take 10 $ streamToList a
  
{-
  Exercise 4
  • streamRepeat :: a -> Stream a
  which generates a stream containing infinitely many copies of the given element.
  • streamMap :: (a -> b) -> Stream a -> Stream b
  which applies a function to every element of a Stream.
  • streamFromSeed :: (a -> a) -> a -> Stream a
  which generates a Stream from a “seed” of type a, which is the first element 
  of the stream, and an “unfolding rule” of type a -> a which specifies how to 
  transform the seed into a new seed, to be used for generating the rest of the stream.
-}
streamRepeat :: a -> Stream a
streamRepeat a = Cons a (streamRepeat a)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap mapFunc (Cons a leftOverStream) = 
  Cons (mapFunc a) (streamMap mapFunc leftOverStream)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed unfoldFunc seed = 
  Cons seed (streamFromSeed unfoldFunc (unfoldFunc seed))

{-
  Exercise 5
  • nats :: Stream Integer
  which contains the infinite list of natural numbers 0, 1, 2, . . .
  • ruler :: Stream Integer
  which corresponds to the ruler function 0,1,0,2,0,1,0,3,...
-}
nats :: Stream Integer
nats = streamFromSeed (+ 1) 0

ruler :: Stream Integer
ruler = 
  let 
    interleaveStreams :: Stream Integer -> Stream Integer -> Stream Integer
    interleaveStreams (Cons a leftOverStreamA) (Cons b leftOverStreamB) = 
      Cons a (Cons b $ interleaveStreams leftOverStreamA leftOverStreamB)
    
    largestPower2 :: Integer -> Integer
    largestPower2 n 
      | even n = 1 + largestPower2 (div n 2)
      | otherwise = 0
  in interleaveStreams (streamRepeat 0) (streamMap largestPower2 (streamFromSeed (+2) 2))

{- 
  Exercise 6 (optional)
  I implemented Fibonacci using a generating function that generates a 
  power series - represented as an infinite sequence using an infinite sream, 
  where each element represents a coefficient of a term in a power series
-}
x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

instance Num (Stream Integer) where 
  fromInteger n = Cons n (streamRepeat 0)
  negate (Cons a leftOverStream) = Cons (-1 * a) (negate leftOverStream)
  (+) (Cons a leftOverStream1) (Cons b leftOverStream2) =
    Cons (a + b) ((+) leftOverStream1 leftOverStream2)
  (*) (Cons a leftOverStream1) stream2@(Cons b leftOverStream2) = 
    Cons (a * b) (
      (+) (streamMap (\x -> x * a) leftOverStream2) ((*) leftOverStream1 stream2)
    )

instance Fractional (Stream Integer) where
  (/) stream1@(Cons a leftOverStream1) stream2@(Cons b leftOverStream2) = 
    Cons (div a b) (
      streamMap 
      (\x -> div x b) $ 
      leftOverStream1 - ((*) (stream1 / stream2) leftOverStream2)
    )
    
fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^2)

{- 
  Exercise 7 (optional)
  I implemented Fibonacci, but in O(logN) runtime using matrices. The crux of  
  this is that Haskell already uses binary exponentiation when raising an 
  instance of a Num type (using the ^ operator) using the (*) defined method 
  on the Num type. Binary exponentiation goes like so:
    x^n = (x^(n/2))^2 when n is even
    x^n = x * (x^((n-1)/2))^2 when n is even
-}
data Matrix = Matrix Integer Integer Integer Integer
instance Num (Matrix) where
  (*) (Matrix aa1 ab1 ba1 bb1) (Matrix aa2 ab2 ba2 bb2) = 
    Matrix 
      ((*) aa1 aa2 + (*) ab1 ba2) 
      ((*) aa1 ab2 + (*) ab1 bb2)
      ((*) ba1 aa2 + (*) bb1 ba2) 
      ((*) ba1 ab2 + (*) bb1 bb2)
      
fib4 :: Integer -> Integer
fib4 0 = 0 
fib4 n = case ((Matrix 1 1 1 0) ^ n) of Matrix _ a _ _ -> a


main = 
  let 
    fib4answer :: Integer -> Integer
    fib4answer nthTerm = fib4 nthTerm
    
    fib3answer :: Integer -> Integer
    fib3answer nthTerm = (streamToList fibs3) !! fromIntegral nthTerm
    
    fib2answer :: Integer -> Integer
    fib2answer nthTerm = fibs2 !! fromIntegral nthTerm
    
    fib1answer :: Integer -> Integer
    fib1answer nthTerm = fibs1 !! fromIntegral nthTerm
    
    testFibs :: Integer -> Bool
    testFibs n = fib4answer n == fib3answer n
        && fib3answer n == fib2answer n 
        && fib2answer n == fib1answer n
        
    testFib4AndFib3 :: Integer -> Bool
    testFib4AndFib3 n = fib4answer (abs n) == fib3answer (abs n)
    
  in do {
    quickCheck (forAll (choose (0, 10)) testFibs);
    quickCheck testFib4AndFib3
  }
