module Submission where

module TypeCheckerMonad where

import qualified Data.Map as Map
import Data.Map (Map)
import ParsePCFLetMonad

type Type_env = Map String Atype

-- typeCheck tm env returns the type of term tm in environment env
typeCheck :: Term -> Type_env -> Maybe Atype
typeCheck (AST_ID s) env = Map.lookup s env
typeCheck (AST_NUM n) env = return TYPE_INT
typeCheck (AST_BOOL b) env = return TYPE_BOOL
typeCheck AST_SUCC env = return $ TYPE_FUN(TYPE_INT, TYPE_INT)
typeCheck AST_PRED env = return $ TYPE_FUN(TYPE_INT, TYPE_INT)
typeCheck AST_ISZERO env = return $ TYPE_FUN(TYPE_INT, TYPE_BOOL)
typeCheck (AST_FUN (s, tp, body)) env =
    do
       bodytype <- typeCheck body (Map.insert s tp env)
       return $ TYPE_FUN (tp,bodytype)
typeCheck (AST_APP (f, arg)) env = 
    do
        ftype <- typeCheck f env
        argtype <- typeCheck arg env
        case ftype of
             (TYPE_FUN(t1, t2)) ->
                  if t1 == argtype then return t2 else Nothing
             _ -> Nothing

typeCheck (AST_REC (r, tp, body)) env =
    do
      bodytype <- typeCheck body (Map.insert r tp env)
      if (bodytype == tp) then return tp
          else Nothing
          
typeCheck (AST_IF (cond, thentm, elsetm)) env =
    do
        condtp <- typeCheck cond env
        thentp <- typeCheck thentm env
        elsetp <- typeCheck elsetm env
        if ((condtp /= TYPE_BOOL) || (thentp /= elsetp))
            then Nothing
            else return thentp



----------------Type Check for ASF_IF addition-----------

typeCheck (ASF_IF (vble, tp, term, body)) env = 
    do
        bodytp <- typeCheck body (Map.insert vble tp env) -- adding the type
        termtp <- typeCheck term env -- Check term tyoe
        if (termtp /= tp)
            then Nothing
            else return bodytp
---------------------------------------------------------


emptyEnv = Map.empty

-- type check AST for term in empty environment
runit :: Term -> String
runit term = show(typeCheck term emptyEnv)

-- type check string representation of term in empty environment
-- I.e., first parse term and then run type checker
checkNParse :: String -> Maybe Atype
checkNParse term = do
    ast <- parsestr term
    typeCheck ast emptyEnv


main2 = do 
          putStrLn(show(testIf4))
          putStrLn(show(testFun))
          putStrLn(show(testSum))
          putStrLn(show(testRecSimple))
          putStrLn(show(testSuper))
          putStrLn(show(testErr1))
          putStrLn(show(testErr2))
          putStrLn(show(testErr3))

