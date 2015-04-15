import Data.Char (ord)

mapEleva :: [Float] -> [Float]
mapEleva a = map sqrt a

posicaoAlfabeto :: String -> [Int]
posicaoAlfabeto a = map abs (map ((-) 96) (map ord  a))

mapComp :: (a->b) -> [a] -> [b]
mapComp f a = [f x | x<-a]



member :: (Eq a) => [a] -> a -> Bool
member lista a = foldr ((||)) False (map ((==) a ) lista)

aux :: (Eq a) => [a] -> [a] -> [a]
aux a b = [x | x<-b, not (member a x)]

union :: (Eq a) => [a] -> [a] -> [a]
union a b = foldr (++) [] ([a]++[(aux a b)])


aux2 :: String -> Int
aux2 palavra = foldr (+) 0 (posicaoAlfabeto palavra)

valorListaString :: [String] -> [Int]
valorListaString a = map aux2 a

--valorString :: [String] -> [Int]
--valorString a = map (foldr (+) 0 (posicaoAlfabeto a)) a

data Tree t = NilT | Node t (Tree t) (Tree t) deriving (Show)

insert :: Tree t -> t -> Tree t
insert NilT  novo = Node novo (NilT) (NilT)
insert (Node a treeEsq treeRig) novo = Node a (insert treeEsq novo) (treeRig)

{-
criarArvore :: [t] -> (Tree t -> t -> Tree t) -> Tree t
criarArvore listaNos insert = foldr (insert) NilT listaNos 
-}
{-
createTree ::[Int] -> Tree Int

createTree [] = NiT

createTree (a:as) = 

 insert a (createTree as) -}

-- aqui começa o trabalho

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

--corta :: Tree t -> (t->Bool) -> [Tree t]
--corta NilT _ = []
--corta (Node a (Node aesq (arvoreEsq1) (arvoreDir1)) (Node adir (arvoreEsq2) (arvoreDir2))) f
-- | not (f aesq) = [percorre (Node a (Node aesq (arvoreEsq1) (arvoreDir1)) (Node adir (arvoreEsq2) (arvoreDir2))) f] ++ (percorre arvoreEsq f) ++ (percorre arvoreDir f)

--(Node 5 (Node 7 (Node 15 NilT (Node 6 NilT NilT)) (Node 2 NilT NilT)) (Node 10 NilT NilT))






{-
filtro :: Eq t => (t -> Bool) -> Tree t -> (Tree t, [Tree t])
filtro _ NilT = (NilT, [])
filtro f (Node v e d)
    | f v = ((Node v esquerda direita), (listaEsq ++ listaDir))
    | not (f v) && (e == NilT) && (d == NilT) = (NilT, [])
    | otherwise = (NilT, filterTree f e ++ filterTree f d )
    where (esquerda, listaEsq) = filtro f e
          (direita, listaDir) = filtro f d

filtrandoLista :: Eq t => (t -> Bool) -> (Tree t, [Tree t]) -> [Tree t]
filtrandoLista f (x, []) = []
filtrandoLista f (x, a:as) 
 | arv == NilT = filtrandoLista f (x, as)
 | otherwise = arv : filtrandoLista f (x, as)
 where (arv, filtraHead) = filtro f a

filterTree :: Eq t => (t -> Bool) -> Tree t -> [Tree t]
filterTree _ NilT = []
filterTree f a = arv : (filtrandoLista f (arv, floresta))
 where (arv, floresta) = filtro f a
 -} 