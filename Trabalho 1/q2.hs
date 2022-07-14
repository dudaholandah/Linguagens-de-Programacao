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


