menorMaior :: Int-> Int-> Int-> (Int, Int)
menorMaior a b c = (g, h)
 where g = min (min a b) (min b c)
       h = max (max a b) (max b c)

ordenaTripla :: (Int, Int, Int) -> (Int, Int, Int)
ordenaTripla (a, b, c) = (d, e, f)
 where d = min (min a b) (min b c)
       f = max (max a b) (max b c)
       e = min (max a b) (max b c)

--mergesort

meulength :: [Int] -> Int
meulength [] = 0
meulength (a:as)= 1+ length as

splitBf :: [Int]-> Int -> [Int]
splitBf (a:as) 1 = [a]
splitBf (a:as) b = a:(splitBf as (b-1))

splitAf :: [Int] -> Int -> [Int]
splitAf (a:as) 1 = as
splitAf (a:as) n = splitAf as (n-1)

mergesubida :: [Int] -> [Int] -> [Int]
mergesubida [] b = b
mergesubida a [] = a
mergesubida (a:as) (b:bs)
 |a <= b = a:mergesubida as (b:bs)
 |otherwise = b:mergesubida (a:as) (b:bs)

--esse é o método final, o real mergesort, botei subida e descida pra ajudar a me orientar em quem faz oq
