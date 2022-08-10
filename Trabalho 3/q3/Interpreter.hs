module Root.Questoes.Q3.Interpreter where

import Root.Questoes.Q3.AbsLI
import Prelude hiding (lookup)

type RContext = [(String, Integer)]
type ErrorMessage = String

executeP :: RContext -> Program -> Either ErrorMessage RContext
executeP context (Prog stm) = execute context stm

-- change return from RContext to Either Error Message RContext
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
                        Right vexp -> if (vexp == 0)
                                        then Right context
                                        else case execute context stm of
                                                Right cont -> execute cont (SWhile exp stm)
                                                Left msg -> Left msg
                        Left msg -> Left msg

-- change return from Integer to Either Error Message Integer
eval :: RContext -> Exp -> Either ErrorMessage Integer
eval context x = case x of
  EAdd exp0 exp -> case eval context exp0 of
                        Right vexp0 -> case eval context exp of
                                        Right vexp -> Right (vexp0 + vexp)
                                        Left msg -> Left msg
                        Left msg -> Left msg
  ESub exp0 exp -> case eval context exp0 of
                        Right vexp0 -> case eval context exp of
                                        Right vexp -> Right (vexp0 - vexp)
                                        Left msg -> Left msg
                        Left msg -> Left msg                     
  EMul exp0 exp -> case eval context exp0 of
                        Right vexp0 -> case eval context exp of
                                        Right vexp -> Right (vexp0 * vexp)
                                        Left msg -> Left msg
                        Left msg -> Left msg
  EDiv exp0 exp -> case eval context exp0 of
                        Right vexp0 -> case eval context exp of
                                        Right vexp -> if (vexp == 0)
                                                        then Left ("divisao por 0")
                                                        else Right (vexp0 `div` vexp)
                                        Left msg -> Left msg
                        Left msg -> Left msg
  EInt n -> Right n
  EVar id -> Right ( lookup context (getStr id) )

getStr :: Ident -> String
getStr (Ident s) = s

lookup :: RContext -> String -> Integer
lookup ((i, v) : cs) s
  | i == s = v
  | otherwise = lookup cs s

update :: RContext -> String -> Integer -> RContext
update [] s v = [(s, v)]
update ((i, v) : cs) s nv
  | i == s = (i, nv) : cs
  | otherwise = (i, v) : update cs s nv
