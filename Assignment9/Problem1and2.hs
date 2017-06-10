--Problem 1
--Problem 1-a
ones :: [Int]
ones = 1 : ones

--Problem 1-b
intList n = n : map (+1) (intList n) 

--Problem 1-c
takeN n _ | n <= 0 = []
takeN n [] = []
takeN n (x:list) = x : takeN (n-1) list

--Problem 2
--Problem 2-a
evens :: [Int]
evens = [2,4..]

odds :: [Int]
odds = [1,3..]

--Problem 2-b
merge :: [Int]->[Int]->[Int]
merge (t1:l1) (t2:l2) = t1 : t2 : merge l1 l2
merge _ (t2:l2) = (t2:l2)
merge (t1:l1) _ = (t1:l1)
merge _ _ = []
--The call to 'merge evens odds' will print numbers and will not terminate, 
--it evaluates to an infinite list since evens and odds are infinite lists
--The call to length(merge evens odds) will not response because it is keep counting elements
--from a infinite list, no output will be given.

--Problem 2-c
--(i)
cubes = [n^3 | n <- [0..]]
--(ii)
triple = [3^n | n <- [0..]]
--(iii)
squares = [n^2 | n <- [0..]]
output = merge [0,1..] squares
--(iv)
negativeNum = [-n | n <- [1,2..]]
