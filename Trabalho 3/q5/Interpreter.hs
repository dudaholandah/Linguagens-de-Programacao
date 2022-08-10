module Root.Questoes.Q5.Interpreter where

import Root.Questoes.Q5.AbsLI

type RContext = [(String, Valor)]

type ErrorMessage = String

data Valor
  = ValorStr String
  | ValorInt Integer
  | ValorBool Bool

instance Eq Valor where
  (ValorInt i1) == (ValorInt i2) = i1 == i2
  (ValorStr s1) == (ValorStr s2) = s1 == s2
  (ValorBool b1) == (ValorBool b2) = b1 == b2

s :: Valor -> String
s (ValorStr str) = str

i :: Valor -> Integer
i (ValorInt vint) = vint

b :: Valor -> Bool
b (ValorBool vbool) = vbool

getStr :: Ident -> String
getStr (Ident s) = s

lookupN :: RContext -> String -> Valor
lookupN ((i,v):cs) s
   | i == s = v
   | otherwise = lookupN cs s

update :: RContext -> String -> Valor -> RContext
update [] s v = [(s, v)]
update ((i, v) : cs) s nv
  | i == s = (i, nv) : cs
  | otherwise = (i, v) : update cs s nv 


executeP :: RContext -> Program -> Either ErrorMessage RContext
executeP context (Prog stm) = execute context stm


execute :: RContext -> Stm -> Either ErrorMessage RContext
execute context x = case x of
  SAss id exp -> case eval context exp of
                        Right vexp -> Right ( update context (getStr id) vexp )
                        Left msg -> Left msg
  SBlock [] -> Right context
  SBlock (s : stms) -> case execute context s of
                        Right cont -> execute cont (SBlock stms)
                        Left msg -> Left msg
  SWhile exp stm -> case eval context exp of
                        Right ( ValorInt vexp ) -> if (vexp == 0)
                                        then Right context
                                        else case execute context stm of
                                                Right cont -> execute cont (SWhile exp stm)
                                                Left msg -> Left msg
                        Left msg -> Left msg
  SdoWhile stm exp -> case execute context stm of
                        Right cont -> execute cont (SWhile exp stm)
                        Left msg -> Left msg
  STry [] stmsC stmsF -> execute context ( SBlock stmsF )                      
  STry (s : stmsT) stmsC stmsF -> case execute context s of
                                        Right cont -> execute cont ( STry stmsT stmsC stmsF )
                                        Left _ -> case execute context ( SBlock stmsC ) of
                                                        Right nCont -> execute nCont ( SBlock stmsF )
                                                        Left msg -> Left msg


eval :: RContext -> Exp -> Either ErrorMessage Valor
eval context x = case x of
  EAdd exp0 exp -> case eval context exp0 of
                        Right ( ValorInt vexp0 ) -> case eval context exp of
                                        Right ( ValorInt vexp ) -> Right ( ValorInt (vexp0 + vexp) )
                                        Left msg -> Left msg
                        Left msg -> Left msg
  ESub exp0 exp -> case eval context exp0 of
                        Right ( ValorInt vexp0 ) -> case eval context exp of
                                        Right ( ValorInt vexp ) -> Right ( ValorInt (vexp0 - vexp) )
                                        Left msg -> Left msg
                        Left msg -> Left msg                     
  EMul exp0 exp -> case eval context exp0 of
                        Right ( ValorInt vexp0 ) -> case eval context exp of
                                        Right ( ValorInt vexp ) -> Right ( ValorInt (vexp0 * vexp) )
                                        Left msg -> Left msg
                        Left msg -> Left msg
  EDiv exp0 exp -> case eval context exp0 of
                        Right ( ValorInt vexp0 ) -> case eval context exp of
                                        Right ( ValorInt vexp ) -> if (vexp == 0)
                                                        then Left ("divisao por 0")
                                                        else Right ( ValorInt (vexp0 `div` vexp) )
                                        Left msg -> Left msg
                        Left msg -> Left msg
  ECon exp0 exp -> case eval context exp0 of
                        Right ( ValorStr vexp0 ) -> case eval context exp of
                                        Right ( ValorStr vexp ) -> Right ( ValorStr (vexp0 ++ vexp) )
                                        Left msg -> Left msg
                        Left msg -> Left msg                                        
  EInt n -> Right (ValorInt n)
  EVar id -> Right ( lookupN context (getStr id) ) 
  EStr str -> Right (ValorStr str)
  EOr exp0 exp -> case eval context exp0 of
                        Right ( ValorBool vexp0 ) -> case eval context exp of
                                        Right ( ValorBool vexp ) -> Right ( ValorBool (vexp0 || vexp) )
                                        Left msg -> Left msg
                        Left msg -> Left msg
  EAnd exp0 exp -> case eval context exp0 of
                        Right ( ValorBool vexp0 ) -> case eval context exp of
                                        Right ( ValorBool vexp ) -> Right ( ValorBool (vexp0 && vexp) )
                                        Left msg -> Left msg
                        Left msg -> Left msg
  ENot exp -> case eval context exp of
                        Right ( ValorBool vexp ) -> Right (ValorBool (not vexp))
                        Left msg -> Left msg
  ETrue -> Right (ValorBool True)  
  EFalse -> Right (ValorBool False) 
 