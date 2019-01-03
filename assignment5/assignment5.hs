{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Assignment5 where
import ExprT (ExprT(Lit, Add, Mul))
import Parser (parseExp)
import StackVM (Program, StackExp(..))


{-
  Exercise 1
  Write Version 1 of the calculator: an evaluator for ExprT, 
  with the signature
    eval :: ExprT -> Integer
  For example, eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) == 20.
-}
eval :: ExprT -> Integer
eval (ExprT.Lit someInt) = someInt
eval (ExprT.Add exprT1 exprT2) = eval exprT1 + eval exprT2
eval (ExprT.Mul exprT1 exprT2) = eval exprT1 * eval exprT2


{- 
  Exercise 2
  Leverage the assets of the UI team to implement the value-added 
  function
   evalStr :: String -> Maybe Integer
  which evaluates arithmetic expressions given as a String, producing 
  Nothing for inputs which are not well-formed expressions, and Just n 
  for well-formed inputs that evaluate to n.
-}
evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp ExprT.Lit ExprT.Add ExprT.Mul


{-
  Exercise 3
  Create a type class called Expr with three methods called 
  lit, add, and mul which parallel the constructors of ExprT. 
  Make an instance of Expr for the ExprT type, in such a way that
    mul (add (lit 2) (lit 3)) (lit 4) :: ExprT
    == Mul (Add (Lit 2) (Lit 3)) (Lit 4)
-}
class Expr a where
  add :: a -> a -> a
  mul :: a -> a -> a
  lit :: Integer -> a
  
instance Expr ExprT where 
  add x y = ExprT.Add x y
  mul x y = ExprT.Mul x y
  lit x = ExprT.Lit x

{- 
  Exercise 4
  Make instances of Expr for each of the following types:
    • Integer — works like the original calculator
    • Bool — every literal value less than or equal to 0 is interpreted as False, 
    and all positive Integers are interpreted as True; “addition” is logical or, 
    “multiplication” is logical and 
    • MinMax — “addition” is taken to be the max function, while “multiplication” 
    is the min function
    • Mod7— all values should be in the range 0 . . . 6, and all arithmetic is done 
    modulo 7; for example, 5 + 3 = 1.
-}

newtype MinMax  = MinMax Integer deriving (Eq, Show)
newtype Mod7    = Mod7 Integer deriving (Eq, Show)
  
instance Expr Integer where
  add x y = x + y
  mul x y = x * y
  lit x = x
  
instance Expr Bool where
  add x y = x || y
  mul x y = x && y
  lit x = x > 0
  
instance Expr MinMax where
  add (MinMax x) (MinMax y) = MinMax (max x y)
  mul (MinMax x) (MinMax y) = MinMax (min x y)
  lit x = MinMax x

instance Expr Mod7 where 
  add (Mod7 x) (Mod7 y) = Mod7 (mod (x + y) 7)
  mul (Mod7 x) (Mod7 y) = Mod7 (mod (x * y) 7)
  lit x = Mod7 (mod x 7)
  
{-
  Exercise 5
  Put together the pieces you have to create a function 
    compile :: String -> Maybe Program 
  which takes Strings representing arithmetic expressions and 
  compiles them into programs that can be run on the custom CPU.
-}

instance Expr Program where
  add x y = x ++ y ++ [StackVM.Add]
  mul x y = x ++ y ++ [StackVM.Mul]
  lit x = [StackVM.PushI x]
  
  
compile :: String -> Maybe Program
compile = parseExp lit add mul
