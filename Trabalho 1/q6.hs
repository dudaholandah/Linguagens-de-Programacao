-- questao 6
-- a partir de duas notas das provas de cada aluno, determinar a lista 
-- dos alunos aprovados, com suas respectivas médias. O resultado deve estar
-- ordenado crescentemente pela média aritmética das notas. A aprovação ocorre se,
-- e somente se, tal média é maior ou igual a cinco. 
-- obs: pode importar a funcao sort ou sortBy

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

