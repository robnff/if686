double :: [Int]->[Int]
double (head:tail)
 | tail==[] = [head*2]
 | otherwise = (head*2):(double tail)  

membership :: [Int] -> Int -> Bool
membership (head:tail) qlqr
 |tail==[] = False
 |otherwise = (head==qlqr) || membership tail qlqr

membershipChar :: [Char] -> Char -> Bool
membershipChar (head:tail) qlqr
 |tail==[] = False
 |otherwise = (head==qlqr) || membershipChar tail qlqr

digits :: String -> String
digits (a:as)
 |as==[] && (membershipChar ['1', '2', '3', '4', '5', '6', '7', '8', '9', '0'] a) = [a]
 |as==[] && ((membershipChar ['1', '2', '3', '4', '5', '6', '7', '8', '9', '0'] a)==False) = []
 |(membershipChar ['1', '2', '3', '4', '5', '6', '7', '8', '9', '0'] a) = a:digits as

sumpairs :: [Int]->[Int]->[Int]
sumpairs (a:as) (b:bs)
 |(as==[] && bs==[]) || as==[] || bs==[] = []
 |otherwise = (a+b):(sumpairs as bs)

quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort (a:as) = (sort [x | x <- as, x < a]) ++ [a] ++ (sort [x | x <- as, x >=a])

fiboN :: Int-> Int
fiboN 1 = 1
fiboN 2 = 1
fiboN a = fiboN (a-2) + fiboN (a-1)

listaFib :: Int->[Int]
listaFib a = (fiboN a):(fiboN a+1) 

fiboPar :: Int->[Int]
fiboPar a
 |
 | 