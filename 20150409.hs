data Shape = Circle Float | Rectangle Float Float

area :: Shape -> Float
area (Circle raio) = 2*pi*raio
area (Rectangle base altura) = base*altura


data Dias = Segunda Int [String]| Terca Int [String]| Quarta Int [String]| Quinta Int [String]| Sexta Int [String]| Sabado | Domingo deriving (Show)

ehFds :: Dias -> Bool
ehFds Sabado = True
ehFds Domingo = True
ehFds _ = False

membership :: [String] -> String -> Bool
membership (head:tail) qlqr
 |head == qlqr = True
 |tail==[] = False
 |otherwise = (head==qlqr) || membership tail qlqr

temPLC :: Dias ->Bool
temPLC (Segunda _ aulasdodia) = membership aulasdodia "PLC"
temPLC (Terca _ aulasdodia) = membership aulasdodia "PLC"
temPLC (Quarta _ aulasdodia) = membership aulasdodia "PLC"
temPLC (Quinta _ aulasdodia) = membership aulasdodia "PLC"
temPLC (Sexta _ aulasdodia) = membership aulasdodia "PLC"
temPLC _ = False

data Tree t = NilT | Node t (Tree t) (Tree t)

igualTree :: (Eq t) => Tree t -> Tree t -> Bool
igualTree NilT NilT = True
igualTree _ NilT = False
igualTree NilT _ = False
igualTree (Node a esquerdaA direitaA) (Node b esquerdaB direitaB) = (a==b) && (igualTree esquerdaA esquerdaB) && igualTree direitaA direitaB

showTree :: (Show t) => Tree t -> String
showTree NilT = ""
showTree (Node a esquerda direita) = (show a) ++ showTree esquerda ++ showTree direita

--aqui começa o trabalho

data Graph t = Graph [t] [(t,t,Int)] deriving (Show, Eq, Ord)

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (a:as) = (quicksort [x | x <- as, x < a]) ++ [a] ++ (quicksort [x | x <- as, x >=a])

ehtuplaMenor :: (Ord a)=> (a,a,Int) -> (a,a,Int) -> Bool
ehtuplaMenor (a,af,intA) (b,bf,intB)
 | a>b = False
 | af>bf = False
 | intA > intB = False
 | otherwise = True

quicksortuplas :: (Ord a) => [(a,a,Int)] -> [(a,a,Int)]
quicksortuplas [] = []
quicksortuplas (a:as) = (quicksortuplas [x | x <- as, (ehtuplaMenor x  a)]) ++ [a] ++ (quicksortuplas [x | x <- as, not(ehtuplaMenor x a)])

listaigual :: (Ord a) => [a] -> [a] -> Bool
listaigual [] [] = True
listaigual (a:as) (b:bs)
 | not ((length (a:as)) == (length (b:bs))) = False 
 | a==b = listaigual as bs
 |otherwise = False



grafosIguais :: (Ord a) => (Graph a) -> (Graph a) -> Bool
grafosIguais (Graph listanosA listaarestasA) (Graph listanosB listaarestasB)
 | ((listaigual (quicksort listanosA) (quicksort listanosB)) && (listaigual (quicksortuplas listaarestasA) (quicksortuplas listaarestasB))) = True
 | otherwise = False