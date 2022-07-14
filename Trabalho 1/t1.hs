{-- Maria Eduarda Machado de Holanda
    19/0043725 --}

-- questao 1
-- defina uma funcao que retorne o maior entre quatro inteiros

maior4 :: Int -> Int -> Int -> Int -> Int
maior4 a b c d 
    | a >= b && a >= c && a >= d = a
    | b >= a && b >= c && b >= d = b
    | c >= a && c >= b && c >= d = c
    | otherwise = d

-- questao 2
-- defina uma funcao que receba uma nota e retorne a mencao do aluno. 

converterNotaParaMencao :: Float -> String
converterNotaParaMencao nota 
    | nota >= 9.0 = "SS"
    | nota >= 7.0 = "MS"
    | nota >= 5.0 = "MM"
    | nota >= 3.0 = "MI"
    | nota >= 0.1 = "II"
    | otherwise   = "SR"

-- questao 3
-- defina uma função que retorna um booleano indicando se
-- uma lista de inteiros é decrescente ou não

isDecrescente :: [Int] -> Bool
isDecrescente [] = True
isDecrescente [x] = True
isDecrescente (f:s:xs) = (f > s) && isDecrescente (s:xs)

-- questao 4
-- defina uma funcao que recebe uma lista de strings
-- c e computa uma lista de pares de (String,Int) 
-- representando o histograma de seus elementos

occurs :: String -> [String] -> Int
occurs s [] = 0
occurs s (x:as)
    | s == x = 1 + occurs s as
    | otherwise = occurs s as

excludeOccurence :: String -> [String] -> [String]
excludeOccurence s [] = []
excludeOccurence s (x:as)
    | s == x = excludeOccurence s as
    | otherwise = x:excludeOccurence s as

histograma :: [String] -> [(String, Int)]
histograma [] = []
histograma (a:ls) 
    | q == 1 = (a,q):histograma ls
    | otherwise = (a,q):histograma xs
        where q  = occurs a (a:ls)
              xs = excludeOccurence a ls

-- questao 5
-- defina uma funcao que tem como parâmetros uma função binária (que tem dois 
-- parâmetros) e duas listas, retornando uma lista de valores resultantes da 
-- aplicação dessa função nos elementos correspondentes dessas listas

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f [] _ = []
myZipWith f _ [] = []
myZipWith f (a:as) (b:bs) = f a b : myZipWith f as bs

-- questao 6
-- a partir de duas notas das provas de cada aluno, determinar a lista 
-- dos alunos aprovados, com suas respectivas médias. O resultado deve estar
-- ordenado crescentemente pela média aritmética das notas. A aprovação ocorre se,
-- e somente se, tal média é maior ou igual a cinco. 
-- obs: pode importar a funcao sort ou sortBy

quickSort :: Ord a1 => [(a2, a1)] -> [(a2, a1)]
quickSort [] = []
quickSort ((x,y):xs) = quickSort [(f,s) | (f,s) <- xs, s <= y] 
                    ++ [(x,y)] 
                    ++ quickSort [(f,s) | (f,s) <- xs, s > y]

aprovadosOrdemDeMedia :: [(String,Float,Float)] -> [(String,Float)]
aprovadosOrdemDeMedia [] = []
aprovadosOrdemDeMedia ((s,n1,n2):ls)
    | m >= 5 = quickSort((s,m):aprovadosOrdemDeMedia ls)
    | otherwise = aprovadosOrdemDeMedia ls
    where m = (n1+n2)/2

-- questao 7
-- considere a representacao de matrizes como lista de listas
-- em que cada elemento da lista é uma lista que representa uma linha da matriz.
-- com base nisso, determine as seguintes funcoes:

-- a) some duas matrizes:

add :: Num t => [t] -> [t] -> [t]
add [] _ = []
add _ [] = []
add (a:as) (b:bs) = (a+b):add as bs

somaMatricial :: Num u => [[u]] -> [[u]] -> [[u]]
somaMatricial [] m = m
somaMatricial m [] = m
somaMatricial (l:ls) (h:hs) = add l h : somaMatricial ls hs

-- b) compute a transposta de duas matrizes:

matrizTransposta :: Num u => [[u]] -> [[u]]
matrizTransposta [] = []
matrizTransposta ([]:_) = []
matrizTransposta m = (map head m) : matrizTransposta (map tail m)

-- c) compute a multiplicação de duas matrizes:

mult :: Num a => [a] -> [a] -> [a]
mult [] _ = []
mult _ [] = []
mult (a:as) (b:bs) = (a*b):mult as bs

multiplicacaoMatricial :: Num u => [[u]] -> [[u]] -> [[u]]
multiplicacaoMatricial [] [] = []
multiplicacaoMatricial a b = [ [prodInt ri rj | rj <- bt] | ri <- a ]
    where bt = matrizTransposta b
          prodInt x1 x2 = sum (mult x1 x2)