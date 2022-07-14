-- questao 1
-- defina uma funcao que retorne o maior entre quatro inteiros

maior4 :: Int -> Int -> Int -> Int -> Int
maior4 a b c d 
    | a >= b && a >= c && a >= d = a
    | b >= a && b >= c && b >= d = b
    | c >= a && c >= b && c >= d = c
    | otherwise = d