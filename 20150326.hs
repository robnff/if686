type Chave = String
type Elem = String
type Hash = (Chave, Elem)
type HashTable = [Hash]

hashExemplo :: HashTable
hashExemplo = [("testando", "funcionou")]

get :: HashTable -> Chave -> Elem
get [] _ = "Não tem"
get ((c,e):as) cc
 | c==cc = e
 | otherwise = get as cc

put :: HashTable -> Chave -> Elem -> HashTable
put as c e
 | ((get as c) == "Não tem") =  (c, e):as
 | otherwise = as 

remove :: HashTable -> Chave -> HashTable
remove []  _ = []
remove ((c,e):as) cc
 | c == cc = remove as cc
 | otherwise = (c, e):remove as cc

hasKey :: HashTable -> Chave -> Bool
hasKey as c
 | (get as c) == "Não tem" = False
 | otherwise = True

-- vou precisar do sort pra transformar uma lista numa representação mais confiável de um conjunto (ordenarei e retirarei os iguais para tal)
quicksort :: (Ord t) => [t] -> [t]
quicksort [] = []
quicksort (a:as) = (quicksort [x | x <- as, x < a]) ++ [a] ++ (quicksort [x | x <- as, x >=a])

removerep :: (Ord t) => [t] -> [t]
removerep [] = []
removerep [a] = [a]
removerep (a:a2:as)
 | a==a2 = removerep (a2:as)
 | otherwise = a:removerep (a2:as)

-- aqui acabam os métodos que não são de conjuntos e começam os relativos à conjuntos

iguais :: (Ord t) => [t] -> [t] -> Bool
iguais [] [] = True
iguais (a:as) (b:bs)
 | a==b = (True && (iguais as bs))
 | otherwise = False 

existe :: (Ord t) => [t] -> t -> Bool
existe [] _ = False
existe (a:as) b
 | a==b = True
 | otherwise = existe as b

intersec :: (Ord t) => [t] -> [t] -> Bool
intersec [] _ = False --será alterado dependendo de castor contar ou não com o vazio como interseção
intersec _ [] = True
intersec (a:as) bs
 | existe bs a = True
 | otherwise = intersec as bs

disjunto :: (Ord t) => [t] -> [t] -> Bool
disjunto as bs = not(intersec as bs)

contem :: (Ord t) => [t] ->[t] -> Bool -- o contém tbm tá dependendo de como o vazio vai ser considerado, por enquanto, fora de questão
contem [] _ = False
contem _ [] = False
contem (a:as) bs
 | existe bs a = True || (contem bs as)
 | otherwise = False

comparaConjuntos :: (Ord t)=> [t]->[t]->String
comparaConjuntos a b
 |iguais (removerep (quicksort a)) (removerep (quicksort b)) = "A igual a B"
 |contem (removerep (quicksort a)) (removerep (quicksort b)) = "A contem B"
 |contem (removerep (quicksort b)) (removerep (quicksort a)) = "B contem A"
 |intersec (removerep (quicksort a)) (removerep (quicksort b)) = "A interseciona B"
 |otherwise = "Conjuntos disjuntos"

 --tirar duvidas com vitor, universidades
 --formalizar followup de etapa-etapa