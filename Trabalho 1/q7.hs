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
matrizTransposta m = map head m : matrizTransposta (map tail m)

-- c) compute a multiplicação de duas matrizes:

mult :: Num a => [a] -> [a] -> [a]
mult [] _ = []
mult _ [] = []
mult (a:as) (b:bs) = (a*b):mult as bs

multiplicacaoMatricial :: Num u => [[u]] -> [[u]] -> [[u]]
multiplicacaoMatricial a b = [ [prodInt ri rj | rj <- bt] | ri <- a ]
    where bt = matrizTransposta b
          prodInt x1 x2 = sum (mult x1 x2)
