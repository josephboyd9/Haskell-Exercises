{-# LANGUAGE TemplateHaskell, Rank2Types, CPP #-}

import Test.QuickCheck
import Test.QuickCheck.All
import Control.Arrow --for copy/pasted  quickCheck of p6

--99 questions begins

--problem 1
myLast :: [a] -> a
myLast [] = error "empty list"
myLast [x] = x
myLast (_:xs) = myLast xs

--problem 2
myButLast :: [a] -> a
myButLast [] = error "empty"
myButLast [x,_] = x
myButLast (x:xs) = myButLast xs

--problem 3
elementAt :: [a] -> Int -> a
elementAt [] _ = error "The list is empty :/"
elementAt (x:_) 1 = x
elementAt (_:xs) x 
  | x < 1          = error "index out of bounds"
  | otherwise      = elementAt xs (x-1)
--using the infix operator (!!)
elementAt' :: [a] -> Int -> a
elementAt' list i = list !! (i-1)

--problem 4
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs
--multiple complex answers exist for this

--problem 5
myReverse''' :: [a] -> [a]
myReverse''' [] = []
myReverse''' [x] = [x]
myReverse''' (x:xs) = myReverse xs ++ [x]
--hoped this would be faster, it's about the same
myReverse' :: [a] -> [a]
myReverse' [] = [] -- error "empty list"
myReverse' [x] = [x]
myReverse' list = head list : (myReverse (tail list))
--efficiency? efficiency!!! :D ... but I can't understand it
myReverse'' :: [a] -> [a]
myReverse'' = foldl (flip (:)) []
--okay, this one is readable AND efficient :)
myReverse :: [a] -> [a]
myReverse [] = [] -- error "empty list"
myReverse [x] = [x]
myReverse list = myReverse'''' list []
   where 
      myReverse'''' [] reversed = reversed
      myReverse'''' (x:xs) reversed  = myReverse'''' xs (x : reversed)

--problem 6
isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome (x:xs) = isPalindrome' (x:xs)
   where
   isPalindrome' (x:xs)
    | last xs == x = isPalindrome (init xs)
    | otherwise = False

--problem 7
data NestedList a = Elem a | List [NestedList a]

--tests
prop_myLast x xs = x `notElem` xs ==> myLast (xs ++ [x]) == x --p1
prop_myButLast x y  xs = x `notElem` xs && x /= y ==> myButLast (xs++[x]++[y]) == x --p2
prop_elementAt xs x = length xs >= x && x > 0 && length xs > 0 ==> elementAt xs x == xs !! (x-1) --p3
prop_myLength xs = myLength xs == length xs --p4
prop_myReverse xs = myReverse xs == reverse xs --p5
--I'll be honest, I don't know what this means, I just copied an answer
prop_isPalindrome xs = isPalindrome xs == (uncurry (==) . (id &&& reverse)) xs--p6

return []
runTests = $quickCheckAll











