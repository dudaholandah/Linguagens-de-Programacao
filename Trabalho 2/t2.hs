module Root.Exercicios.UnBCare where

import Root.Modelo.ModeloDados
import Data.Maybe
import Data.List

{-
 *** Aluno: Maria Eduarda Machado de Holanda
 *** Matricula: 190043725
 

██╗░░░██╗███╗░░██╗██████╗░  ░█████╗░░█████╗░██████╗░██████╗
██║░░░██║████╗░██║██╔══██╗  ██╔══██╗██╔══██╗██╔══██╗██╔════╝
██║░░░██║██╔██╗██║██████╦╝  ██║░░╚═╝███████║██████╔╝█████╗░░
██║░░░██║██║╚████║██╔══██╗  ██║░░██╗██╔══██║██╔══██╗██╔══╝░░
╚██████╔╝██║░╚███║██████╦╝  ╚█████╔╝██║░░██║██║░░██║███████╗
░╚═════╝░╚═╝░░╚══╝╚═════╝░  ░╚════╝░╚═╝░░╚═╝╚═╝░░╚═╝╚══════╝

O objetivo desse trabalho é fornecer apoio ao gerenciamento de cuidados a serem prestados a um paciente.
O paciente tem um receituario médico, que indica os medicamentos a serem tomados com seus respectivos horários durante um dia.
Esse receituário é organizado em um plano de medicamentos que estabelece, por horário, quais são os remédios a serem
tomados. Cada medicamento tem um nome e uma quantidade de comprimidos que deve ser ministrada.
Um cuidador de plantão é responsável por ministrar os cuidados ao paciente, seja ministrar medicamento, seja comprar medicamento.
Eventualmente, o cuidador precisará comprar medicamentos para cumprir o plano.
O modelo de dados do problema (definições de tipo) está disponível no arquivo Modelo/ModeloDados.hs
Defina funções que simulem o comportamento descrito acima e que estejam de acordo com o referido
modelo de dados.

-}

{-

   QUESTÃO 1, VALOR: 1,0 ponto

Defina a função "comprarMedicamento", cujo tipo é dado abaixo e que, a partir de um medicamento, uma quantidade e um
estoque inicial de medicamentos, retorne um novo estoque de medicamentos contendo o medicamento adicionado da referida
quantidade. Se o medicamento já existir na lista de medicamentos, então a sua quantidade deve ser atualizada no novo estoque.
Caso o remédio ainda não exista no estoque, o novo estoque a ser retornado deve ter o remédio e sua quantidade como cabeça.

-}

existeMedicamento :: Medicamento -> EstoqueMedicamentos -> Bool
existeMedicamento _ [] = False
existeMedicamento med ((m,q):es)
   | m == med = True
   | otherwise = existeMedicamento med es

comprarMedicamento :: Medicamento -> Quantidade -> EstoqueMedicamentos -> EstoqueMedicamentos
comprarMedicamento med qtd [] = [(med,qtd)]
comprarMedicamento med qtd ((m,q):es)
   | not (existeMedicamento med ((m,q):es)) = (med,qtd):((m,q):es)
   | med == m = (m,q+qtd) : es
   | otherwise = (m,q) : comprarMedicamento med qtd es

{-
   QUESTÃO 2, VALOR: 1,0 ponto

Defina a função "tomarMedicamento", cujo tipo é dado abaixo e que, a partir de um medicamento e de um estoque de medicamentos,
retorna um novo estoque de medicamentos, resultante de 1 comprimido do medicamento ser ministrado ao paciente.
Se o medicamento não existir no estoque, Nothing deve ser retornado. Caso contrário, deve se retornar Just v,
onde v é o novo estoque.

-}

tomarMedicamento :: Medicamento -> EstoqueMedicamentos -> Maybe EstoqueMedicamentos
tomarMedicamento med [] = Nothing
tomarMedicamento med ((m,q):es)
   | not (existeMedicamento med ((m,q):es)) = Nothing
   | m == med = Just ((m,q-1) : es)
   | otherwise = Just ((m,q) : fromJust(tomarMedicamento med es))

{-
   QUESTÃO 3  VALOR: 1,0 ponto

Defina a função "consultarMedicamento", cujo tipo é dado abaixo e que, a partir de um medicamento e de um estoque de
medicamentos, retorne a quantidade desse medicamento no estoque.
Se o medicamento não existir, retorne 0.

-}

consultarMedicamento :: Medicamento -> EstoqueMedicamentos -> Quantidade
consultarMedicamento med [] = 0
consultarMedicamento med ((m,q):es)
   | m == med = q
   | otherwise = consultarMedicamento med es

{-
   QUESTÃO 4  VALOR: 1,0 ponto

  Defina a função "demandaMedicamentos", cujo tipo é dado abaixo e que computa a demanda de todos os medicamentos
  por um dia a partir do receituario. O retorno é do tipo EstoqueMedicamentos e deve ser ordenado lexicograficamente
  pelo nome do medicamento.

  Dica: Observe que o receituario lista cada remédio e os horários em que ele deve ser tomado no dia.
  Assim, a demanda de cada remédio já está latente no receituario, bastando contar a quantidade de vezes que cada remédio
  é tomado.

-}

demandaMedicamentos :: Receituario -> EstoqueMedicamentos
demandaMedicamentos [] = []
demandaMedicamentos ((m,h):ls) = (m,length h) : demandaMedicamentos ls

{-
   QUESTÃO 5  VALOR: 1,0 ponto, sendo 0,5 para cada função.

 Um receituário é válido se, e somente se, todo os medicamentos são distintos e estão ordenados lexicograficamente e,
 para cada medicamento, seus horários também estão ordenados e são distintos.

 Inversamente, um plano de medicamentos é válido se, e somente se, todos seus horários também estão ordenados e são distintos,
 e para cada horário, os medicamentos são distintos e são ordenados lexicograficamente.

 Defina as funções "receituarioValido" e "planoValido" que verifiquem as propriedades acima e cujos tipos são dados abaixo:

-}

estaOrdenado :: (Ord a) => [a] -> Bool
estaOrdenado [] = True
estaOrdenado [x] = True
estaOrdenado (x:y:ls) = (x < y) && estaOrdenado (y:ls)

possuiCopia :: (Eq a) => a -> [a] -> Bool
possuiCopia _ [] = False
possuiCopia x (y:ls)
   | x == y = True
   | otherwise = possuiCopia x ls

todosDiferentes :: (Eq a) => [a] -> Bool
todosDiferentes [] = True
todosDiferentes [x] = True
todosDiferentes (x:ls)
   | possuiCopia x ls = False
   | otherwise = todosDiferentes ls

receituarioValido :: Receituario -> Bool
receituarioValido [] = True
receituarioValido ls = estaOrdenado meds && todosDiferentes meds && all estaOrdenado hors && all todosDiferentes hors
   where meds = [med | (med,hor) <- ls]
         hors = [hor | (med,hor) <- ls]

planoValido :: PlanoMedicamento -> Bool
planoValido [] = True
planoValido ls = estaOrdenado hors && todosDiferentes hors && all estaOrdenado meds && all todosDiferentes meds
   where meds = [med | (hor,med) <- ls]
         hors = [hor | (hor,med) <- ls]

{-

   QUESTÃO 6  VALOR: 1,0 ponto,

 Um plantão é válido se, e somente se, todas as seguintes condições são satisfeitas:

 1. Os horários da lista são distintos e estão em ordem crescente;
 2. Não há, em um mesmo horário, ocorrência de compra e medicagem de um mesmo medicamento (e.g. `[Comprar m1, Medicar m1 x]`);
 3. Para cada horário, as ocorrências de Medicar estão ordenadas lexicograficamente.

 Defina a função "plantaoValido" que verifica as propriedades acima e cujo tipo é dado abaixo:

-}

-- plantaoInvalido2 =
--   [ (6, [Medicar med6, Medicar med9]),
--     (8, [Medicar med2, Medicar med4]),
--     (17, [Medicar med4, Comprar med4 30]),
--     (22, [Medicar med7])
--   ]

unicaOcorrencia :: Medicamento -> [Cuidado] -> Bool
unicaOcorrencia med [] = True
unicaOcorrencia med (Comprar m q:ls)
   | m == med = False
   | otherwise = unicaOcorrencia med ls
unicaOcorrencia med (Medicar m:ls)   
   | m == med = False
   | otherwise = unicaOcorrencia med ls

cuidadoValido :: [Cuidado] -> Bool
cuidadoValido [] = True
cuidadoValido (Comprar m q:ls)
   | unicaOcorrencia m ls = cuidadoValido ls
   | otherwise = False
cuidadoValido (Medicar m:ls)
   | unicaOcorrencia m ls = cuidadoValido ls
   | otherwise = False 

getMedicamentos :: [Cuidado] -> [Medicamento]
getMedicamentos [] = []
getMedicamentos (Comprar m q:ls) = getMedicamentos ls
getMedicamentos (Medicar m:ls) = m : getMedicamentos ls

medicamentosOrdenados :: [[Cuidado]] -> Bool
medicamentosOrdenados [] = True
medicamentosOrdenados (c:cs)
   | estaOrdenado (getMedicamentos c) = medicamentosOrdenados cs
   | otherwise = False

plantaoValido :: Plantao -> Bool
plantaoValido [] = True
plantaoValido ls = estaOrdenado hors && all cuidadoValido cuids && medicamentosOrdenados cuids
   where hors = [hor | (hor,cuid) <- ls]
         cuids = [cuid | (hor,cuid) <- ls]

{-
   QUESTÃO 7  VALOR: 1,0 ponto

  Defina a função "geraPlanoReceituario", cujo tipo é dado abaixo e que, a partir de um receituario válido,
  retorne um plano de medicamento válido.

  Dica: enquanto o receituário lista os horários que cada remédio deve ser tomado, o plano de medicamentos  é uma
  disposição ordenada por horário de todos os remédios que devem ser tomados pelo paciente em um certo horário.

-}

juntaMedicamento :: Horario -> Receituario -> [Medicamento]
juntaMedicamento _ [] = []
juntaMedicamento h ((m,hrs):ys) 
   | h `elem` hrs = m : juntaMedicamento h ys
   | otherwise = juntaMedicamento h ys

juntaPlano :: Receituario -> [Horario] -> PlanoMedicamento 
juntaPlano _ [] = []
juntaPlano receit (h:hs) = (h, juntaMedicamento h receit) : juntaPlano receit hs 

geraPlanoReceituario :: Receituario -> PlanoMedicamento
geraPlanoReceituario n = juntaPlano n (listaHorarios n)
   where listaHorarios rc = sort $ nub $ concat $ [hors | (med,hors) <- rc]

-- geraPlanoReceituario :: Receituario -> PlanoMedicamento -- PlanoMedicamento
-- geraPlanoReceituario x = juntaMedicamentos x (listaHorarios x)
--    where listaHorarios rc = sort $ nub $ concat $ [hors | (med,hors) <- rc]



{- QUESTÃO 8  VALOR: 1,0 ponto

 Defina a função "geraReceituarioPlano", cujo tipo é dado abaixo e que retorna um receituário válido a partir de um
 plano de medicamentos válido.
 Dica: Existe alguma relação de simetria entre o receituário e o plano de medicamentos? Caso exista, essa simetria permite
 compararmos a função geraReceituarioPlano com a função geraPlanoReceituario ? Em outras palavras, podemos definir
 geraReceituarioPlano com base em geraPlanoReceituario ?

-}

juntaHorario :: Medicamento -> PlanoMedicamento -> [Horario]
juntaHorario _ [] = []
juntaHorario m ((hor,med):ls) 
   | m `elem` med = hor : juntaHorario m ls
   | otherwise = juntaHorario m ls

juntaReceituario :: PlanoMedicamento -> [Medicamento] -> Receituario 
juntaReceituario _ [] = []
juntaReceituario plano (med:meds) = (med, juntaHorario med plano) : juntaReceituario plano meds 

geraReceituarioPlano :: PlanoMedicamento -> Receituario
geraReceituarioPlano n = juntaReceituario n (listaMedicamentos n)
   where listaMedicamentos rc = sort $ nub $ concat $ [meds | (hor,meds) <- rc]

{-  QUESTÃO 9 VALOR: 1,0 ponto

Defina a função "executaPlantao", cujo tipo é dado abaixo e que executa um plantão válido a partir de um estoque de medicamentos,
resultando em novo estoque. A execução consiste em desempenhar, sequencialmente, todos os cuidados para cada horário do plantão.
Caso o estoque acabe antes de terminar a execução do plantão, o resultado da função deve ser Nothing. Caso contrário, o resultado
deve ser Just v, onde v é o valor final do estoque de medicamentos

-}

executaCuidados :: [Cuidado] -> EstoqueMedicamentos -> EstoqueMedicamentos
executaCuidados [] estoque = estoque 
executaCuidados (Medicar med:ls) estoque = executaCuidados ls (fromJust(tomarMedicamento med estoque))
executaCuidados (Comprar med qtd:ls) estoque = executaCuidados ls (comprarMedicamento med qtd estoque)

cuidadoPlantaoValido :: [Cuidado] -> EstoqueMedicamentos -> Bool
cuidadoPlantaoValido [] _ = True 
cuidadoPlantaoValido (Medicar med:ls) estoque
   | consultarMedicamento med estoque == 0 = False
   | otherwise = cuidadoPlantaoValido ls estoque
cuidadoPlantaoValido (Comprar med qtd:ls) estoque = cuidadoPlantaoValido ls novoEstoque
   where novoEstoque = comprarMedicamento med qtd estoque

executaPlantao :: Plantao -> EstoqueMedicamentos -> Maybe EstoqueMedicamentos
executaPlantao [] estoque = Just estoque
executaPlantao ((hrs,cuidados):ls) estoque 
               | not(cuidadoPlantaoValido cuidados estoque) = Nothing 
               | otherwise = executaPlantao ls (executaCuidados cuidados estoque)

{-
QUESTÃO 10 VALOR: 1,0 ponto

Defina uma função "satisfaz", cujo tipo é dado abaixo e que verifica se um plantão válido satisfaz um plano
de medicamento válido para um certo estoque, ou seja, a função "satisfaz" deve verificar se a execução do plantão
implica terminar com estoque diferente de Nothing e administrar os medicamentos prescritos no plano.
Dica: fazer correspondencia entre os remédios previstos no plano e os ministrados pela execução do plantão.
Note que alguns cuidados podem ser comprar medicamento e que eles podem ocorrer sozinhos em certo horário ou
juntamente com ministrar medicamento.

-}

planoExpandido :: PlanoMedicamento -> [(Horario, Medicamento)]
planoExpandido [] = []
planoExpandido ((hor, meds):ls) = sort (zip hors meds ++ planoExpandido ls)
   where hors = replicate (length meds) hor

cuidadosMedicar :: [Cuidado] -> [Medicamento]
cuidadosMedicar [] = []
cuidadosMedicar (Medicar med:ls) = med : cuidadosMedicar ls
cuidadosMedicar (_:ls) = cuidadosMedicar ls

plantaoMedicarExpandido :: Plantao -> [(Horario, Medicamento)]
plantaoMedicarExpandido [] = []
plantaoMedicarExpandido ((hor, cuids):ls) = sort (zip hors medicar ++ plantaoMedicarExpandido ls)
   where medicar = cuidadosMedicar cuids
         hors = replicate (length medicar) hor

satisfaz :: Plantao -> PlanoMedicamento -> EstoqueMedicamentos -> Bool
satisfaz plantao plano estoque = (plantaoMedicarExpandido plantao == planoExpandido plano) && isJust(executaPlantao plantao estoque)


{-

QUESTÃO 11 VALOR: 1,0 ponto

 Defina a função "plantaoCorreto", cujo tipo é dado abaixo e que gera um plantão válido que satisfaz um plano de
 medicamentos válido e um estoque de medicamentos.
 Dica: a execução do plantão deve atender ao plano de medicamentos e ao estoque.

-}

geraCuidado :: [Medicamento] -> EstoqueMedicamentos -> [Cuidado]
geraCuidado [] _ = []
geraCuidado (med:ls) estoque
   | consultarMedicamento med estoque > 0 = Medicar med : geraCuidado ls estoque
   | consultarMedicamento med estoque <= 0 = Comprar med 1 : geraCuidado (med:ls) novoEstoque
   where novoEstoque = comprarMedicamento med 1 estoque

plantaoCorreto :: PlanoMedicamento ->  EstoqueMedicamentos  -> Plantao
plantaoCorreto [] _ = []
plantaoCorreto ((hr, meds):ls) estoque = (hr, geraCuidado meds estoque) : plantaoCorreto ls estoque
