-- Partners:
-- Iman Elsayed
-- Payam Dowlatyari

import Data.List
import System.IO

--------------------------------------------------------------------------------------------------------------
-- 1) Write a Haskell function called insert that takes two parameters (a list and a number). 
--------------------------------------------------------------------------------------------------------------


insert2 :: Int -> [Int] -> [Int]
insert2 x [] = [x]
insert2 x (y:ys) = if x < y 
                 then x:y:ys 
         else y : insert2 x ys


--------------------------------------------------------------------------------------------------------------
-- 2) Write a polymorphic Haskell function called insertSort that takes one parameter (a list of elements). 
-- The function should return the list that would result from sorting the list using insertion sort.
--------------------------------------------------------------------------------------------------------------


insertSort :: [Int] -> [Int]
insertSort [] = []
insertSort [x] = [x]
insertSort (x:xs) = insert (insertSort xs)
  where insert [] = [x]
        insert (y:ys)
          | x < y = x : y : ys
          | otherwise = y : insert ys


--------------------------------------------------------------------------------------------------------------
-- 3) Write a function merge :: (Ord a) => [[a]] -> [a] that takes a finite list of sorted finite lists 
-- and merge them into a single sorted list.
--------------------------------------------------------------------------------------------------------------


merge :: (Ord a) => [[a]] -> [a]
merge [] = []
merge xs = merge2 (filter (\x -> not (null x)) xs)
   where 
    merge2 xs = sort (map head xs ++ merge (map tail xs))


--------------------------------------------------------------------------------------------------------------
-- 4) Write and test the definition of a (polymorphic) Haskell function 'center' that takes
-- three arguments, a list arg1 of type [a], a width arg2 of type Int, and a fill item arg3 of type a
--------------------------------------------------------------------------------------------------------------

center :: [a] -> Int -> a -> [a]
center s w f = lfill ++ s ++ rfill where
    n = w 
    nl = div w 2 
    nr = n - nl 
    lfill = replicate nl f 
    rfill = replicate nr f 


--------------------------------------------------------------------------------------------------------------
-- 5) Write and test the definition of a Haskell function 'largest', which finds the largest element of a list
--------------------------------------------------------------------------------------------------------------

largest :: [Int] -> Int
largest [] = 0
largest (head : tail) = max head (largest tail)