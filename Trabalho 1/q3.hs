-- questao 3
-- defina uma função que retorna um booleano indicando se
-- uma lista de inteiros é decrescente ou não


isDecrescente :: [Int] -> Bool
isDecrescente [] = True
isDecrescente [x] = True
isDecrescente (f:s:xs) = (f > s) && isDecrescente (s:xs)
