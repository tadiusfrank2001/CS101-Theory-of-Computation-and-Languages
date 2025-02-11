module Submission where

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



-- Function sumSquares that, given a nonnegative integer n, and return the sum of squares in range (n)
sumSquares :: (Eq p, Num p) => p -> p

sumSquares 0 = 0
sumSquares p = p*p + sumSquares (p-1)

-- Function listDup that takes a pair, which consists of an element, e, of any type, and a non-negative number n, as input and returns a list with n copies of e.

listDup :: (Eq b, Num b) => (a, b) -> [a]

listDup (e,0) = []
listDup (e,n) = e : (listDup (e,n-1))

-- Function getEvens takes a list of integers as input and returns a list by keeping only the elements that are even, in their original order.

getEvens :: Integral a => [a] -> [a]

getEvens [] = []
getEvens (h:t) = if (mod h 2 == 0)
                then h : getEvens t
                else getEvens t

-- Function incBy takes a list of integers l and an integer n as input and returns a list by increasing every element in l by n.

incBy :: Num a => a -> [a] -> [a]

incBy a [] = []
--incBy 0 (h:t) = (h:t)
incBy a (h:t) = (h + a) : incBy a t

-- Function zip1 takes two lists as input and zips them together to produce a list of pairs. If the two list are not of the same length, the resultant list is of the length of the shorter one.

zip1 :: [t] -> [t1] -> [(t, t1)]

zip1 [] [] = []
zip1 [] (y:yl) = []
zip1 (x:xl) [] = []
zip1 (x:xl) (y:yl) = (x,y): zip1 xl yl

-- Function zip2 takes two lists as input and zip them together to produce a list of pairs.

zip2 :: [t] -> [t] -> [(t, t)]

zip2 [] [] = []
zip2 [] (y:yl) = (y,y) : zip2 [] yl
zip2 (x:xl) [] = (x,x) : zip2 xl []
zip2 (x:xl) (y:yl) = (x,y) : zip2 xl yl

--Function find takes an element and a list as input, 
--and returns the index occurrence of the first element in the list 
--(the index of the first element is 0), and returns -1 if the element is not in the list.

indexfinder n [] i = -1
indexfinder n (h:t) i = if (n == h)
                        then i
                        else indexfinder n t i+1
                        

find :: (Eq a, Eq a1, Num a1) => a -> [a] -> a1

find a [] = -1
find a (h:t) = indexfinder a (h:t) 0

--Function compose takes two relations, 
--represented as a list of pairs, as input, 
--and computes the composition of the two relations. 
--The resulting relation should NOT contain any dupli- cates.

-- Function remove takes an element in the list and a list, then deletes the given element from the list.
remove :: (Eq a, Eq b) => (a, b) -> [(a, b)] -> [(a, b)]

remove e [] = []
remove e ((p1,p2):t) = if e == (p1,p2)
                then remove e t
                else (p1,p2) : remove e t

-- Function deleteDuppy takes a list, and checks for duplicates, returning a list of unique elements.
deleteDuppy :: (Eq a, Eq b) => [(a, b)] -> [(a, b)]

deleteDuppy [] = []
deleteDuppy ((p1,p2):t) = (p1,p2) : deleteDuppy (remove (p1,p2) t)


compose :: (Ord a, Ord b1, Eq b2) => [(a, b2)] -> [(b2, b1)] -> [(a, b1)]

compose [] _ = []
compose _ []= []
compose ((p1,p2):t1) ((q1,q2):t2) = if p2 == q1
                                    then deleteDuppy ((p1,q2) : (compose ((p1,p2):t1)(t2)) ++
                                    compose t1 ((q1,q2):t2))
                                    else deleteDuppy (compose ((p1,p2):t1) (t2) ++
                                    compose t1 ((q1,q2):t2))


