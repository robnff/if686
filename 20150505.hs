import Data.Char (ord, chr)
import Data.List 

data Failable a = Failable a | Error String
instance Monad Failable where
 (>>=) (Failable a) f = f a
 (>>=) (Error a) _ = Error a
 return a = Failable a

data Fila t = Head t (Fila t) Int | Nil deriving (Eq)

criarFila :: Int -> t -> Failable(t, Fila t)
criarFila limite elemento = Failable (elemento, Head elemento (Nil) limite)

meulength :: Fila t -> Int
meulength Nil = 0
meulength (Head a (resto) limite) = 1 + meulength resto

push :: t -> Fila t -> Failable (t,Fila t)
push elemento (Head antiga (resto) limite)
 | (meulength (Head antiga (resto) limite))==limite = Error "nao da pra inserir em uma lista cheia"
 | otherwise = Failable (elemento, Head elemento (Head antiga (resto) limite) limite)

pop :: Eq t => Fila t -> Failable (t, Fila t)
pop (Head elemento calda limite)
 | (meulength (Head elemento calda limite)) == 0 = Error "nao da pra remover de uma lista vazia"
 | not(calda == Nil) = pop calda
 | otherwise = Failable(elemento, calda)

peek :: Fila t -> Failable (t,Fila t)
peek (Head elemento calda limite) = Failable(elemento, (Head elemento calda limite))

--começando a parte de IO

--questão 1 do trabalho

type Chave = String
type Elem = String
type Hash = (Chave, Elem)
type HashTable = [Hash]

hashExemplo :: HashTable
hashExemplo = [("testando", "funcionou")]

get :: HashTable -> Chave -> Maybe Elem
get [] _ = No
get ((c,e):as) cc
 | c==cc = Maybe e
 | otherwise = get as cc

put :: HashTable -> Chave -> Elem -> HashTable
put as c e
 | ((get as c) == "Não tem") =  (c, e):as
 | otherwise = as 

remove :: HashTable -> Chave -> Maybe HashTable
remove []  _ = Nothing
remove ((c,e):as) cc
 | c == cc = remove as cc
 | otherwise = (c, e):remove as cc

hasKey :: HashTable -> Chave -> Bool
hasKey as c
 | (get as c) == "Não tem" = False
 | otherwise = True

--questão 2 do trabalho

valida :: String -> Bool
valida [] = True
valida (a:as)
 | not ((elem a ['a'..'z']) || (a==' ') || (elem a ['A'..'Z'])) = False
 | otherwise = True && valida as

maiusculas :: String -> String
maiusculas [] = []
maiusculas (a:as) = (chr(ord(a)-32)):(maiusculas as)

meugetLine :: IO String
meugetLine = do { ch <- getChar;
 if (ch== '\n') then return []
 else do { ch2 <- meugetLine;
  return (ch:ch2)
}}

leString :: IO()
leString = do { putStrLn "Digita ae alguma coisa";
 resposta <- meugetLine;
 if (valida resposta) then putStrLn (maiusculas resposta)
 else putStrLn "Nothing"
} 



