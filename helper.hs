import Data.List

soma :: Int -> Int -> Int
soma x y = x + y

member :: [Int] -> Int -> Bool
member [] _ = False
member (x:xs) n
    | n == x = True
    | otherwise = member xs n


-- firstDigit :: String -> Char
-- firstDigit st = case (digits st) of
--                     [] -> '\0'
--                     (a:as) -> a

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

-- example Library
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