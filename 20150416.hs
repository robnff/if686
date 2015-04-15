quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (a:as) = (quicksort [x | x <- as, x < a]) ++ [a] ++ (quicksort [x | x <- as, x >=a])

corta :: Ord a => [a] -> a -> ([a],[a])
corta [] _ = ([],[])
corta lista pontcort = ([x | x<-lista, x<=pontcort], [x | x<-lista, x>pontcort])

particiona :: Ord a => [a] -> [a] -> [[a]]
particiona [] listaEntrada = [listaEntrada]
particiona (corte1:cortes) (a:as) = cortado1:(particiona cortes cortado2)
 where (cortado1, cortado2) = corta (quicksort (a:as)) corte1

listPartitioner :: Ord a => [a] -> ([a]->[[a]])
listPartitioner listacortes = particiona (quicksort listacortes)