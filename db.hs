-------------------------------------------------------
-- Extended Haskell programming exercise
-- Topic: functions over lists
-- Author: Martin Sulzmann
-------------------------------------------------------

-- A student is represented by her name, student id and 
-- a list of the courses the student is taking

type Student = (String, Int, [Int])
type DB = [Student]

-- TASK 0
{-
Databases must be consistent.
We say the database is consistent:
  - if there're no multiple entries of students, 
  - and no multiple entries of courses per students
 For example, the below databases are *inconsistent*
-}

incons1 :: DB
incons1 = [("Jack", 111, [141, 252, 141])]
incons2 :: DB
incons2 = [("Jane", 112, [141, 252]), ("Jane", 112, [141, 252])]
incons22 :: DB
incons22 = [("Jane", 112, [141, 252]), ("Jane", 112, [141, 252]), ("Tim", 112, [141, 252])]
cons1 :: DB
cons1 = [("Jane", 122, [141, 252]), ("Tim", 212, [141, 252]), ("Paul", 112, [141, 252])]

{-
Your task is to implement the following function
which returns True if the database is valid (consistent),
otherwise the function returns False.

stackoverflow:
http://stackoverflow.com/questions/6121256/efficiently-checking-that-all-the-elements-of-a-big-list-are-the-same

dot(.) vs dollar($)
http://stackoverflow.com/questions/940382/haskell-difference-between-dot-and-dollar-sign 
-}

-- Gets the first elemement
get1 :: Student -> [Char]
get1 (s,_,_) = s
-- Gets the second elemement
get2 :: Student -> Int
get2 (_,m,_) = m
-- Gets the last elemement
get3 :: Student -> [Int]
get3 (_,_,cs) = cs

-- Gets all elems from a particular position from a list
allFirst :: DB -> [String]
allFirst l = map (\x -> get1 x) l 
allSecond :: DB -> [Int]
allSecond l = map (\x -> get2 x) l
allLast :: DB -> [[Int]]
allLast l = map (\x -> get3 x) l

-- Returns a list of Bools by comparing all elements in a list agains each other.
difElem (x:xs) = (map (/= x) (xs))

-- EXTENSION TO TASK 0
{-
Extension: We strenghten the notion of consistency.
In addition, we require that there shall by no duplicate student id's.
For example, 
-}
incons3 :: DB                
incons3 = [("Jane", 111, [141]), ("Jack", 111, [141, 252])]

cons3 :: DB                
cons3 = [("Jane", 111, [141]), ("Jack", 112, [141, 252])]
  
valid :: DB -> Bool
valid [] = True
valid db = or (myFlatt [difElem x | x <- allLast db]) 
           && and (difElem (allFirst (db))) 
           && or (difElem (allSecond (db)))

-- FROM HERE ON, WE WILL ASSUME THAT ALL DATABASES ARE CONSISTENT !!!

-- TASK 1
{-
Given a database and a student id, we're looking for the list of 
courses of this particular student.
-}
query1 :: DB -> Int -> [Int]
query1 db id = myFlatt ([get3 x | x <- db, get2 x == id])

-- TASK 2
{-
Given a database and a course, find all students 
taking this course.
-}
query2 :: DB -> Int -> [String]
query2 db c = [get1 x | x <- db, cs <- get3 x, cs == c]

-- TASK 3
{-
Given a database, sort the database (in non-decreasing order)
according to the name of students.

Examples:
http://learnyouahaskell.com/higher-order-functions
Tutorial:
http://learnyouahaskell.com/chapters
-}

sortDB :: DB -> DB
sortDB [] = []
sortDB (x:xs) = sortDB lesser ++ [x] ++ sortDB greater
  where lesser = filter (< x) (xs)
        greater = filter (> x) (xs) 

-- Testing purpose
cons2 :: DB
cons2 = [("a", 200, [341, 456]), ("c", 100, [100]), ("b", 50, [160, 456, 123])]

x :: Student
x = ("a", 200, [341, 456, 879])
xs :: DB
xs = [("c", 100, [100]), ("b", 50, [160, 456])]

{-
Extension1:
Provide a function sortDB' which sorts the database according to the number of courses a student is taking
-}
sortDB2 :: DB -> DB
sortDB2 [] = []
sortDB2 (x:xs) = sortDB greater ++ [x] ++ sortDB lesser 
  where greater = filter (\(_,_,cs) -> length cs < (length . get3) x) (xs)
        lesser = filter (\(_,_,cs) -> length cs > (length . get3) x) (xs) 

{-
Extension2:
Provide a function sortDB'' which is parameterized in the actual comparing relation which determines the sorting order
For example:
 Given

  cmpName :: Student -> Student -> Bool
  cmpName (n1, _, _) (n2, _, _) = n1 <= n2

Then you can define

 sortDB = sortDB'' cmpName
-}

-- TASK 4
{-
Given two databases, merge them to obtain one (consistent) database
 Example:

 merge [("Jane", 112, [141, 353])] [("Jane", 112, [141, 252])]
    => Just [("Jane", 112, [141, 252, 353])]
 merge [("Jane", 112, [141, 353])] [("Jane", 113, [141, 252])]
    => Nothing 

The last merge fails because Jane has been given two distinct
student IDs.
-}

-- Testing purpose]
m1 :: DB
m1 =  [("Jane", 112, [141, 353])] 
m11 :: DB
m11 = [("Jane", 112, [141, 252])]

m2 :: DB
m2 =  [("Jane", 112, [141, 353])] 
m22 :: DB
m22 = [("Jane", 113, [141, 252])]

merge :: DB -> DB -> DB
merge db1 db2 = [x | x <- db1, y <- db2, get2 x == get2 y]

-- Utils
myFlatt :: [[a]] -> [a]
myFlatt [] = []
myFlatt (xs:xss) = xs ++ (myFlatt xss)