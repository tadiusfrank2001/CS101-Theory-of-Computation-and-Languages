module Submission where

--Assignment Name: HW2
--Author Name : Tadius Frank
--Date : September 23 2021


--In the process of finishing this homework:
--(a) I had conversations about the contents and solutions of this assignment with the following people: Melat Feseha
--(b) I consulted the following resources, such as books, articles, webpages:
-- • Learn You a Haskell
-- • http://cheatsheet.codeslower.com/CheatSheet.pdf
-- (c) I did not look at the answers of any other students.
-- (d) I did not provide my answers to other students.


-- listComp takes three inputs: the first is a function, the second a list and the third a predicate, 
-- and constructs a list by filtering the input list with the predicate then applying the function to the elements in the resultant list.

listComp::(a -> b) -> [a] -> (a -> Bool) -> [b]

listComp fun [] foo = []
listComp fun (h:t) foo = if foo h 
                     then (fun h) : listComp fun t foo
                     else listComp fun t foo

--getEmpl, which takes a list of Employee records, each of which has the following fields:
-- name which is a string, age which is an integer, and status which is of type Job and 
-- has a value of Managerial, Clerical, or Manual, as input, 
-- and returns the names of managerial employees over the age of 60.


data Job = Managerial | Clerical | Manual deriving Eq
data Employee = Employee{name::String, age::Integer, status::Job}

getEmpl :: [Employee] -> [String]

getEmpl [] = []
getEmpl (Employee{name = n, age = a, status = j}:t) = if (a > 60) && (j == Managerial)
                                                      then n : getEmpl t
                                                      else getEmpl t

