--module PCFEnvInterpLetMonadStarter where
module Submission where

import ParsePCFLetMonad
import qualified Data.Map as Map
import Data.Map (Map)

-- Possible values of term of PCF
data Value = NUM Int | BOOL Bool | SUCC | PRED | 
                       ISZERO | CLOSURE (String, Atype, Term, RTEnv) | 
                       THUNK (Term, RTEnv)
                       deriving (Eq)

instance Show Value where
    show (NUM n) = show n
    show (BOOL b) = show b
    show (SUCC) = "succ "
    show (PRED) = "pred "
    show ISZERO = "isZero "
    show (CLOSURE(param, tp, body,env)) = "<fn "++param++": "++(show tp)++
                              " => "++(show body)++",env>"
    show (THUNK(e,env)) = "Thunk for "++(show e)                    

type RTEnv = Map String Value


-- Compute the value of a term given an environment providing values
-- for free variables 
interpret :: Term -> RTEnv -> Maybe Value
interpret (AST_NUM n) ev = return $ NUM n

-- normally interpret (AST_ID id) <- Map.lookup id ev
-- It's more complicated here because of recursion.
interpret (AST_ID id) ev =
    do
       idval <- Map.lookup id ev
       case idval of
         (THUNK(tm,oldev)) -> interpret tm oldev
         _                 -> return idval
 
interpret (AST_BOOL bval) ev = return $ BOOL(bval)
interpret (AST_FUN (param,tp,body)) ev = return $ CLOSURE(param,tp,body,ev)
interpret AST_SUCC ev = return SUCC
interpret AST_PRED ev = return PRED
interpret AST_ISZERO ev = return ISZERO

-- conditional statement
interpret (AST_IF (test,yesval,noval)) ev =
     let getBool (BOOL b) = return b
         getBool (other) = Nothing
     in
        do -- watch out for case where test evaluates to non-boolean
           bval <- interpret test ev
           testval <- getBool (bval)
           if (testval) then (interpret yesval ev)
                        else (interpret noval ev)
 
-- Function application
interpret (AST_APP (func,arg)) ev = 
     let
        eval (SUCC, NUM n) = return $ NUM (n+1)
        eval (PRED, NUM n) = if n > 0 then return $ NUM (n-1)
                                     else return $ NUM 0
        eval (ISZERO, NUM n) = if n == 0 then return $ BOOL True
                                         else return $ BOOL False
        eval (CLOSURE (param, tp, body, fev), arg) = 
              let  -- update environment for interpreting body
                 nuev = Map.insert param arg fev
              in 
                 interpret body nuev
        eval (_,_) = Nothing
     in
        do
           -- evaluate func and arg in ev before pattern matching
           evalfunc <- interpret func ev
           evalarg <- interpret arg ev
           eval (evalfunc,evalarg)

-- recursion - updates environment with THUNK for recursion.
interpret (AST_REC (name, tp, body)) ev = 
            let   -- create new environment so name associated with 
                  -- thunk with term and environment
               nuev = Map.insert name (THUNK(AST_REC(name,tp, body),ev)) ev 
            in 
               interpret body nuev


interpret (AST_LET (vble,tp,term,body)) ev = 
            do
               termiana <- interpret term ev
               let ev1 = Map.insert vble termiana ev
               interpret body ev1

        
-- runAST exp returns value of exp given as an abstract syntax tree.
-- evaluate exp in an empty environment
runAST :: Term -> Maybe Value
runAST exp = interpret exp Map.empty

-- runNParse exp returns value of exp given as a string.
runNParse :: String -> Maybe Value
runNParse term = do
    ast <- parsestr term
    runAST ast 

funExp = AST_FUN("x", TYPE_INT, AST_APP(AST_SUCC, AST_ID "x"))
funApplied = AST_APP(funExp, AST_NUM 46)

fun1 = AST_FUN ("g", TYPE_FUN(TYPE_INT, TYPE_FUN(TYPE_INT, TYPE_INT)), AST_APP(AST_APP (AST_ID "g", AST_NUM 3), AST_NUM 2))

-- defining the rec of sum
sumRec = AST_REC("sum", TYPE_FUN(TYPE_INT,TYPE_FUN(TYPE_INT, TYPE_INT)), AST_FUN("x", TYPE_INT, AST_FUN("y", TYPE_INT, AST_IF (AST_APP (AST_ISZERO, AST_ID "x"), AST_ID "y", AST_APP(AST_APP(AST_ID "sum", AST_APP ( AST_PRED, AST_ID "x")), AST_APP (AST_SUCC, AST_ID "y"))))))

sumapp = (AST_APP(AST_APP(sumRec,AST_NUM 3),AST_NUM 5))

-- testing some recursive examples
--testSum = typeCheck sumRec emptyEnv
--testRecSimple = typeCheck (AST_REC("x", TYPE_INT, (AST_NUM 0))) emptyEnv

-- defining an even larger example which is why it's super! 
-- it's evaluating the application of the 
-- recursive function sum with two inputs -- 3 and 2.
superCase = AST_APP(fun1, sumRec)

{-
main3 :: IO()
main3 = do
          putStrLn "Type a file name holding a PCF expression"
          fileName <- getLine
          infileHandle <- openFile fileName ReadMode
          pcfExp <- hGetLine infileHandle
          putStr("The value of "++(show pcfExp)++" is ")
--          valexp <- interpret(parsestr pcfExp) Map.empty
          putStrLn (show (run (parsestr pcfExp)))
          hClose infileHandle
-}
