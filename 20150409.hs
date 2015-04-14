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
 
 
 
 -- trabalho seguinte
 
 
-- questão 1
junta :: (Int->Int) -> (Int->Int) -> (Int->Int)
junta (f) (g) = \x -> f (g x)

compose :: (Int->Int) -> [(Int-> Int)] -> [(Int->Int)]
compose (f) listaf = map (junta f) listaf

--questão 2

data Graph t = Graph [t] [(t,t)] deriving (Show, Eq, Ord)
g = Graph [1,2,3] [(1,2), (2,3), (3,1)]

t = Node 5 (Node 7 (Node 15 NilT (Node 6 NilT NilT)) (Node 2 NilT NilT)) (Node 10 NilT NilT)

--map de grafo

maplistarestas :: (a->b) -> [(a,a)] -> [(b,b)]
maplistarestas _ [] = []
maplistarestas (f) ((a,a2):as) = ((f a), (f a2)):(maplistarestas (f) as)

mapgraph :: (a ->b) -> Graph a -> Graph b
mapgraph (f) (Graph (listanos) listarestas) = Graph (map (f) listanos) (maplistarestas (f) listarestas)

--fold de grafo

foldgraph :: (a->b->b) -> b -> Graph a ->b
foldgraph f b (Graph listanos listarestas) = foldr f b listanos

-- questão 3

percorre :: Tree t -> (t->Bool)-> Tree t
percorre NilT _ = NilT
percorre (Node a arvoreEsq arvoreDir) f
 | (f a) = Node a (percorre arvoreEsq f) (percorre arvoreDir f)
 | otherwise = NilT
