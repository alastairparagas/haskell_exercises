{-# LANGUAGE FlexibleInstances #-}
import Data.List (foldl')

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
  Exercise 6
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