module Submission where

--Assignment Name: HW2
--Author Name : Tadius Frank
--Date : September 16 2021


--In the process of finishing this homework:
--(a) I had conversations about the contents and solutions of this assignment with the following people: Melat Feseha
--(b) I consulted the following resources, such as books, articles, webpages:
-- • Learn You a Haskell
-- • http://cheatsheet.codeslower.com/CheatSheet.pdf
-- (c) I did not look at the answers of any other students.
-- (d) I did not provide my answers to other students.


-- import Text.Regex.Posix
-- type of binary operators: +,-,*,/,
data BOp = ADD | MINUS | MUL | DIV | EXP deriving Eq

--  define show function 
instance Show BOp where
   show ADD = "+"
   show MINUS = "-"
   show MUL = "*"
   show DIV = "/"
   show EXP = "^"

-- type of unary operators: -
data UOp = NEG deriving Eq

--  define show function 
instance Show UOp where
   show NEG = "-"

-- type of arithmetic expressions
data ArithExp = AST_NUM Int | 
         AST_UNARY UOp ArithExp |
         AST_BINARY ArithExp BOp ArithExp 

-- define show function so arithmetic expressions can be printed
instance Show ArithExp where
   show (AST_NUM n) = show n
   show (AST_BINARY e1 bop e2) = ('(':(show e1)) ++ (show bop)++ (show e2)++")"
   show (AST_UNARY uop e) = '(':(show uop) ++ (show e)++")"

-- evaluate an arithmetic expression
eval:: ArithExp -> Int
eval (AST_NUM n) = n
eval (AST_UNARY NEG exp) = - (eval exp)
eval (AST_BINARY e1 ADD e2) = eval e1 + eval e2
eval (AST_BINARY e1 MINUS e2) = eval e1 - eval e2
eval (AST_BINARY e1 MUL e2) = eval e1 * eval e2
eval (AST_BINARY e1 EXP e2) = eval e1 ^ eval e2
eval (AST_BINARY e1 DIV e2) = let
           denom = eval e2
        in
           if (denom /= 0) then eval e1 `div` denom
                 else (error "divide by zero")

-- sample expressions
v = AST_BINARY (AST_BINARY (AST_NUM 3) MUL (AST_NUM 4)) ADD
               (AST_BINARY (AST_NUM 2) MINUS (AST_NUM 1))
w = AST_UNARY NEG v

-- A function evaluate that takes a list of operations and a stack returns a stack after peforming all the operations in the list
-- on that stack

evaluate::([OpCode], Stack) -> Float

-- Intializers 
data OpCode = Push Float | Add | Mult | Sub | Div | Swap deriving Show
type Stack = [Float]

--Bases Cases
-- if empty list and empty stack are given return 0.0
evaluate ([], []) = 0.0
-- if empty list and non empty stack is given return stack
evaluate ([], (f:b)) = f
-- if Push operation is present call evaulate recurislevy after appending the r element infront of the stack
evaluate ((Push r :t), l ) = evaluate (t, (r:l))
-- if an empty stack is given with a list of operation return zero
evaluate (_, []) = 0.0
evaluate (_, (f:[])) = 0.0

-- if a specific operation is given then peform that operation on the the first two elements of the stack then continue by calling
-- evaluate recursively 
evaluate ((Add:t), (f1:f2:b)) = evaluate (t, ((f1+f2):b))
evaluate ((Mult:t), (f1:f2:b)) = evaluate (t, ((f1*f2):b))
evaluate ((Sub:t), (f1:f2:b)) = evaluate (t, ((f2-f1):b))
evaluate ((Div:t), (f1:f2:b)) = evaluate (t, ((f2/f1):b))
evaluate ((Swap:t), (f1:f2:b)) = evaluate (t, ([f2]++[f1]++ b))



