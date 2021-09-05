import Data.List
import Data.Char

-- -- -- AULA 2021-08-04 BEGIN
isEven :: Int -> Bool
isEven n = mod n 2 == 0

soma :: Int -> Int -> Int
soma x y = x + y

member :: [Int] -> Int -> Bool
member [] _ = False
member (x:xs) n
    | n == x = True
    | otherwise = member xs n


-- digits :: String -> String
-- digits st = [ch | ch <- st, isDigit st]

-- firstDigit :: String -> [Char]
-- firstDigit st = case (digits st) of
--                     [] -> '\0'
--                     (st:sts) -> st

my_length :: [t] -> Int
my_length [] = 0
my_length (a:as) = 1 + my_length as 

-- Concat
(+++) :: [t] -> [t] -> [t]
[] +++ y = y
(x:xs) +++ y = x : (xs +++ y)

-- zip two lists of diff types
my_zip :: [t] -> [u] -> [(t, u)]
my_zip (a:as) (b:bs) = (a,b):zip as bs
my_zip [] [] = []

-- DATABASE BEGIN
type Person = String
type Book = String
type Database = [(Person, Book)]

exampleBase = [("Alice","Postman Pat"),
               ("Ana","All Alone"),
               ("Alice","Spot"),
               ("Rory","Postman Pat")]

books :: Database -> Person -> [Book]
books db p
 | db == [] = []
 | fst (head db) == p = snd (head db) : books (tail db) p
 | otherwise = books (tail db) p

books_other :: Database -> Person -> [Book]
books_other [] _ = []
books_other ((x, y):as) p
 | x == p = y : books_other as p
 | otherwise = books_other as p

borrowers :: Database -> Book -> [Person]
borrowers db b
 | db == [] = []
 | snd (head db) == b = fst (head db) : borrowers (tail db) b
 | otherwise = borrowers (tail db) b

-- isBorrowed :: Database -> Book -> Bool
-- isBorrowed db b = (my_length (borrowers db b)) > 0
isBorrowed :: Database -> Book -> Bool
isBorrowed db b = (borrowers db b) /= []

numBorrowed :: Database -> Person -> Int
numBorrowed db p = length(books db p)

makeLoan :: Database -> Person -> Book -> Database
makeLoan [] p b = [(p,b)]
makeLoan db@((p,l):dbs) person book
 | (p,l) == (person,book) = db
 | otherwise = (p,l) : (makeLoan dbs person book)

returnLoan :: Database -> Person -> Book -> Database
returnLoan [] _ _ = []
returnLoan db@((p,l):dbs) person book
 | (p,l) == (person,book) = dbs
 | otherwise = (p,l) : (returnLoan dbs person book)
-- DATABASE END


-- LIST COMPREHENSION BEGIN
doubleList xs = [2*a|a <- xs]
doubleIfEven xs = [2*a|a <- xs, isEven a]

sumPairs :: [(Int, Int)] -> [Int]
sumPairs lp = [a+b|(a,b) <- lp]

books_with_comp :: Database -> Person -> [Book]
books_with_comp db p = [book | (person,book) <- db, person == p]

borrowers_with_comp :: Database -> Book -> [Person]
borrowers_with_comp db b = [person | (person, book) <- db, book == b]

return_loan_with_comp :: Database -> Person -> Book -> Database
return_loan_with_comp db p b = [e | e <- db, e /= (p, b)]

-- quicksort
qs :: [Int] -> [Int]
qs [] = []
qs (x:xs) = qs [e | e <- xs, e <= x] ++ [x] ++ qs [e | e <- xs, e > x]
-- -- -- AULA 2021-08-04 END
-- -- -- AULA 2021-08-06 BEGIN

-- matriz1 :: [[Int]]
-- -- matriz1 (2x3)      1 2 3
-- --                    4 5 6
-- -- matriz2 (2x3)      5 6 7
-- --                    8 9 10
-- -- matriz1 + matriz2  5 6 7
-- --                    8 9 10

-- matriz1 = [ [[1, 2, 3] [4, 5, 6] ]
-- matriz1 = [ [[5, 6, 7] [8, 9, 10] ]
-- LIST COMPREHENSION END

-- MAP BEGIN
times2 :: Int -> Int
times2 n = 2 * n

sqr :: Int -> Int
sqr n = n * n

my_map :: (t -> u) -> [t] -> [u]
my_map f [] = []
my_map f (x:xs) = f x : map f xs

doubleList_map xs = map times2 xs
sqrList xs = map sqr xs

map_comp f l = [f a | a <- l]

-- MAP END

-- FOLDING BEGIN (SOMATORIO)
sumList :: [Int] -> Int
sumList [] = 0
sumList (a:as) = a + sumList as

fold :: (t -> t -> t) -> [t] -> t
fold f [a] = a
fold f (a:as) = f a (fold f as)

sumList_fold l = fold (+) l

and :: [Bool] -> Bool
and xs = fold (&&) xs

concat :: [[t]] -> [t]
concat xs = fold (++) xs

-- 6!
-- fold (*) [1..6]

my_foldr :: (t -> u -> u) -> u -> [t] -> u
my_foldr f s [] = s
my_foldr f s (a:as) = f a (my_foldr f s as)

concat_foldr :: [[t]] -> [t]
concat_foldr xs = foldr (++) [] xs

and_foldr :: [Bool] -> Bool
and_foldr bs = foldr (&&) True bs
-- FOLDING END
-- -- -- AULA 2021-08-06 END