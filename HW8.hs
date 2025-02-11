module Submission where

--Assignment Name: HW8
--Author Name : Tadius Frank
--Date : November 4, 2021


--In the process of finishing this homework:
--(a) I had conversations about the contents and solutions of this assignment with the following people: Melat Feseha
--(b) I consulted the following resources, such as books, articles, webpages:
-- • Learn You a Haskell
-- • http://cheatsheet.codeslower.com/CheatSheet.pdf
-- (c) I did not look at the answers of any other students.
-- (d) I did not provide my answers to other students.

-- PROBLEM B:

-- intiate the datatype Tokens
data Tokens = AToken | LParen | RParen | Comma | Error String |
                         EOF deriving (Eq,Show)

-- Make the pathway of the function getToken which takes a string of characters and rerturns a list of their lexical value based on tokens.
getTokens :: [Char] -> [Tokens]

getTokens [] = [EOF]
getTokens ('a':rest) = AToken : getTokens rest
getTokens ('(':rest) = LParen : getTokens rest
getTokens (')':rest) = RParen : getTokens rest
getTokens (',':rest) = Comma : getTokens rest
getTokens ( char :rest) = Error ( "illegal character "++[char]++" present in string."): getTokens rest


-- PROBLEM D:


recognizeExp :: [Tokens] -> Maybe [Tokens]

recognizeExp tokens@(fst:rst)
-- Check the first possible element "(" of Exp
    |(fst == LParen) = 
        do
            r <- recognizeTuple rst
            if (head r) == RParen then return $ tail r else Nothing
-- Check the first possible element "a" of Exp
    |(fst == AToken) = return rst
    | otherwise = Nothing

recognizeTuple :: [Tokens] -> Maybe [Tokens] 

recognizeTuple tokens@(fst:rst)
-- Check the first possible element "(" or "a" of Exp which leads to a exp followed by an expTail
    |(fst == LParen || fst == AToken) = 
        do
            r <- recognizeExp tokens
            recognizeExpTail r
    | otherwise = Nothing

recognizeExpTail :: [Tokens] -> Maybe [Tokens]

recognizeExpTail tokens@(fst:rst)
-- Check first possible element "," for exptails  
    |(fst == Comma) = 
        do
-- Leads us to an exp followed by an exptail
            r <- recognizeExp rst
            recognizeExpTail r
-- Check possibility of an ")" or "EOF (episilon)
    | (fst == EOF || fst == RParen) = return tokens
    | otherwise = Nothing 

recognize ::  [Tokens] -> Bool
recognize tokens = 
    case (recognizeExp tokens) of 
        Just lst -> lst == [EOF] || lst == []
        Nothing -> False

recognizeStr ::  [Char] -> Bool
recognizeStr str = recognize (getTokens str)





