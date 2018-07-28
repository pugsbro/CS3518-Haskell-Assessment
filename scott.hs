--CS3518 Assessment
--Name: Bradley Scott
--Student ID: 51661169
--Email: b.scott.16@aberdeen.ac.uk


--PLEASE NOTE: compiled and run using "The Glorious Glasgow Haskell Compilation System, version 8.0.2"

-- Question 1.

--Test cases:

-- Main> inlist [2,3,2,4,7,9] 7 
-- True

-- Main> inlist [2..10] 101
-- False

-- Main> inlist [2..] 101
-- True

-- Main> inlist [] 0
-- False

--Code: 

-- inlist type signature shows that this function takes a list of integers and a single integer and returns a boolean value

inlist :: [Int] -> Int -> Bool

--base case shows that an empty list and any integer will return false as a number cannot occur in an empty list

inlist [] n = False

--function for inlist is below , recursively checks the head of the list until the head is equal to the number that is searched for

inlist (x:xs) n
  | x == n = True
  | otherwise = inlist xs n
  
  


-- Question 2.

-- Test cases:

-- Main> exactlyonce [2,3,2,4,3] 3
-- False

-- Main> exactlyonce [3..30] 15
-- True

-- Main> exactlyonce [] 100
-- False

-- Main> exactlyonce [1..] 15

-- This hangs as haskell cannot work with the infinite list because of lazy evaluation [1....] 
-- checking that the head is not equal to 1 but according the haskell the tail could contain a 1 so it is in an infinite loop as the tail is never empty

-- Code:

--exactly once type signature shows that this function takes a list of integers and a single integer and returns a boolean value

exactlyonce :: [Int] -> Int -> Bool

-- base case shows that an empty list and and integer will return false as a number cannot appear in an empty list
exactlyonce [] n = False

--function for exactly once is recursive it checks to see if the head of the list is equal to the number, if so then it uses the inlist function defined previously to check
-- if the number exists anywhere else in the list, if it does not exist anywhere else in the list true is returned as it would exist only once, otherwise false is returned as it exists elsewhere in the list


exactlyonce (x:xs) n
  | (x == n) = not(inlist xs n)
  | otherwise = exactlyonce xs n



-- Question 3.

--Test cases:

-- Main> equalones [1,2,0] [3,5,1,1]
-- False

-- Main> equalones [1,0] [0]
-- False

-- Main> equalones [1,0,0,1] [0,1,1,0]
-- True

-- Main> equalones [] []
-- True

--Code:

--equal ones type signature shows that this function takes two lists on integers and returns a boolean
equalones :: [Int] -> [Int] -> Bool

--equal ones function uses the inbuilt filter function to filter/create 2 new lists only containing the 1s of the provided lists
--if these newly filtered lists are equal to each other then True is returned
--otherwise False

equalones (x) (y)
  |(filter (==1) x == filter (==1) y) = True
  |otherwise = False


-- Question 4.

--Test cases:

-- Main> replacenew 2 [3,6,9]
-- [1,16,49]

-- Main> replacenew 3 [1..10]
-- [4,1,0,1,4,9,16,25,36,49]

-- Main> replacenew 5 []
-- []

--Code:

--replace new signature shows that this function takes an integer and a list of integers and returns a list of integers
replacenew :: Int -> [Int] -> [Int]

--base case shows that any integer and an empty list will return an empty list
replacenew x [] = []

--replacenew recursively creates a new list (with the prepend operator:) by doing the calculation on the head then calling itself again with the tail of the list to do the calculation on the head on the tail and so on

replacenew x (y : ys) = ((x-y) * (x-y)) : replacenew x ys


-- Question 5.

--Test cases:

--Main> addthemup [[1,3],[3,7]]
-- 40

-- Main> addthemup [[1,2,3],[9]]
-- 54

-- Main> addthemup [[1,2],[]]
-- 0

-- Main> addthemup [[1,2],[1,3],[4,5,7],[2]]
-- 384

-- Main> addthemup [[],[]]
-- 0

-- Code:

-- addthemup type signature shows that this function takes a list of lists are returns an integer
addthemup :: [[Int]] -> Int

--foldr does 1 * 2 * 3 * 4 etc every element in list returned by map sum
--map sum applies the function sum to every element in the list

addthemup [] = 0
addthemup (x:xs) = foldr (*) 1 (map sum (x:xs))


-- Question 6.

--Test cases:

-- Main> repeatnew square 1 2 
-- 4

-- Main> repeatnew square 10 0.999
-- 0.3589714781897133

-- Main> repeatnew (+ 5) 6 6
-- 36

--Code:

-- defined a square function (as shown in assessment example) to use with repeat new function
-- square function works with fractions as the Num typeclass is used, making for a more genetic solution

square :: Num a => a -> a

square n = n * n

-- repeat new type signature shows that the function takes a function, an integer and an input of the type the function accepts, and returns a value of the type that the function accepts (a is a type variable which makes the function polymorphic) 

repeatnew :: (a -> a) -> Int -> a -> a

-- base cases
repeatnew _ 0 argument   = argument
repeatnew f 1 argument   = f argument

-- recursively repeats the application of a function to an argument a given number of times

repeatnew f times argument = repeatnew f (times - 1) (f argument)


-- Question 7.

-- Test cases:

-- Main> antepenultimate1 []
-- False

-- Main> antepenultimate1 [1,0,1,1,0]
-- True

-- Main> antepenultimate1 [1,0,0,1,1]
-- False

-- Main> antepenultimate1 [1..100]
-- False

-- Main> antepenultimate1 [1,1,1,1,1,1,1,1,1,1,1,1]
-- True

--Code:

--antepenultimate1 type signature shows that the function takes a list of integers and returns a boolean 

antepenultimate1 :: [Int] -> Bool

--return true if the list size is greater than or equal to 3 and the third last member of the list of integers is 1

antepenultimate1 list = not((length list ) < 3) && list!!((length list) - 3) == 1


-- Question 8.

--Test cases:

-- Main> sequenceones []
-- False

-- Main> sequenceones [1,0,1,1,0]
-- True

-- Main> sequenceones [1,1]
-- True

-- Main> sequenceones [1..100]
-- False

-- Main> sequenceones [1,0,0,1,0,1]
-- False

--Code:

-- sequenceones type class constraint shows that type variable a must be members of the equivelance class and Num class this is due to the use of the equality function == requiring the compared values to
-- be of the same type , signature also shows that the function accepts a list of type a and returns a Boolean

sequenceones :: (Eq a, Num a) => [a] -> Bool

--base case showing empty list would return false 

sequenceones [] = False

-- sequenceones recursively checks if the head of the list and first element in the tail are equal to 1, if so return true otherwise return false 

sequenceones (x:xs)
   |(length xs) == 0 = False
   |(x == 1) && ((xs!! 0) == 1) = True
   |otherwise = sequenceones(xs)
