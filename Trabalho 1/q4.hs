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