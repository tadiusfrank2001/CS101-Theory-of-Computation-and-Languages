module Submission where
import NDFAAcceptor
import Data.Set (Set, fromList, singleton, toList, union, map, disjoint)
import Data.Char
import Debug.Trace


--Assignment Name: HW1
--Author Name : Tadius Frank
--Date : September 8 2021


--In the process of finishing this homework:
--(a) I had conversations about the contents and solutions of this assignment with the following people: Melat Feseha
--(b) I consulted the following resources, such as books, articles, webpages:
-- • Learn You a Haskell
-- • http://cheatsheet.codeslower.com/CheatSheet.pdf
-- (c) I did not look at the answers of any other students.
-- (d) I did not provide my answers to other students.

-- Create the encoding of an NDFSM that accepts all strings that contain an even number of a’s or an odd number of b’s. 



eAoB = NFA (fromList [0..4])
        (fromList [Emove 0 1,
                   Move 1 'a' 2,
                   Move 1 'b' 1,
                   Move 2 'a' 1,
                   Move 2 'b' 2,
                   Emove 0 3,
                   Move 3 'b' 4,
                   Move 4 'b' 3,
                   Move 3 'a' 3,
                   Move 4 'a' 4])
        0
        (fromList [1,4])


-- Regular expression recognizer using backtracking to recognize concatenation matches.

--{-# LANGUAGE UnicodeSyntax #-}
-- module RegularExpRecognizer(RegExp(Letter, Dot, Empty,Epsilon, Union, Concat,
--      Star), matches)
--                                   where


-- Internal representation of regular expressions.  Leave out Empty?

data RegExp =  Empty |                -- empty set, accepts nothing
               Epsilon |              -- empty string
               Letter Char |          -- fixed character
               Dot |                  -- any character
               Union RegExp RegExp |  -- union of two expressions
               Concat RegExp RegExp | -- concatenation of two exps
               Star RegExp |          -- 0 or more copies of expression
               Plus RegExp |           -- 1 or more copies of expression
                Option RegExp          -- 0 or 1 copies of expression

instance Show RegExp where
   show Empty = "∅"
   show Epsilon = "ε"
   show (Letter c) = [c]
   show Dot = "."
   show (Union r1 r2) = "("++(show r1) ++ " ⋃ "++ (show r2) ++ ")"
   show (Concat r1 r2) = "("++(show r1) ++ " "++ (show r2) ++ ")"
   show (Star r) = "("++(show r)++")*"
   show (Plus r) = "("++(show r)++")+"
   show (Option r) = "("++(show r)++")?"


-- helper function to give all possible ways of dividing strings in 2 pieces.
splits :: [a] -> [([a],[a])]
splits st = [splitAt n st | n <- [0 .. length st]]

-- like splits but first half must be non-empty
frontSplits :: [a] -> [([a],[a])]
frontSplits st = [splitAt n st | n <- [1 .. length st]]

-- matches regExp str returns true iff str is matched by regExp
matches :: RegExp -> String -> Bool
matches Empty str = False
matches Epsilon str = (str == "")
matches (Letter c) str = (str == [c]) -- [ ] converts Char to String
matches Dot str  = (length str == 1)
matches (Union r1 r2) str  = matches r1 str || matches r2 str
matches (Concat r1 r2) str  =
        or [matches r1 s1 && matches r2 s2 | (s1,s2) <- splits str]
matches (Star r) str  = matches Epsilon str ||
              or [matches r s1 && matches (Star r) s2 |
                          (s1,s2) <- frontSplits str]
-- Need to use frontSplits in last ine so don't recursively call the same
-- match.  I.e. if s1 = empty then s2 = str and get infinite recursion.
matches (Plus r) str = or [matches r s1 && matches (Star r) s2 |
                          (s1, s2) <- splits str]
matches (Option r) str = matches r str || matches Epsilon str
-- test data
rplus = Plus (Union (Letter 'a') (Letter 'b'))
rstar = Star (Union (Letter 'a') (Letter 'b'))
rconcat = Concat (Star (Letter 'a')) (Letter 'b')
runion = Union rstar Dot

{- Exercise: Add Option exp  -- like exp?, Plus Exp
-}


opt = Option (Concat (Star (Letter 'a')) (Letter 'b'))


