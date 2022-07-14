-- questao 5
-- defina uma funcao que tem como parâmetros uma função binária (que tem dois 
-- parâmetros) e duas listas, retornando uma lista de valores resultantes da 
-- aplicação dessa função nos elementos correspondentes dessas listas

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f [] _ = []
myZipWith f _ [] = []
myZipWith f (a:as) (b:bs) = f a b : myZipWith f as bs