module Lintings where

import AST
import LintTypes


--------------------------------------------------------------------------------
-- AUXILIARES
--------------------------------------------------------------------------------

-- Computa la lista de variables libres de una expresión
freeVariables :: Expr -> [Name]
freeVariables = undefined

--------------------------------------------------------------------------------
-- LINTINGS
--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
-- Computación de constantes
--------------------------------------------------------------------------------

evalBool :: Lit -> Bool
evalBool (LitBool x) = x

evalInt :: Lit -> Integer
evalInt (LitInt lit) = lit

evalOp :: Op -> Lit -> Lit -> (Lit, Bool)
evalOp Add lit1 lit2 =
  let res1 = evalInt lit1
      res2 = evalInt lit2
      result = res1 + res2
  in (LitInt result, True)
evalOp Sub lit1 lit2 = 
  let res1 = evalInt lit1
      res2 = evalInt lit2
      result = res1 - res2
  in if (res1 >= res2) then (LitInt result, True) else (LitNil, False)
evalOp Mult lit1 lit2 =
  let res1 = evalInt lit1
      res2 = evalInt lit2
      result = res1*res2
  in (LitInt result, True)
evalOp Div lit1 lit2 =
  let res1 = evalInt lit1
      res2 = evalInt lit2
  in if (res2 == 0) then (LitNil, False) else (LitInt (div res1 res2), True)
evalOp And lit1 lit2 =
  let res1 = evalBool lit1
      res2 = evalBool lit2
      res = res1 && res2
  in (LitBool res, True)
evalOp Or lit1 lit2 =
  let res1 = evalBool lit1
      res2 = evalBool lit2
      res = res1 || res2
  in (LitBool res, True)

evalConstant :: Expr -> (Expr, [LintSugg])
evalConstant expr = case expr of
  Lit lit -> (Lit lit, [])
  Var variable -> (Var variable, [])
  Case expr1 expr2 (x, xs, expr3) ->
    let (result1, sugg1) = evalConstant expr1
        (result2, sugg2) = evalConstant expr2
        (result3, sugg3) = evalConstant expr3
    in (Case result1 result2 (x, xs, result3), sugg3 ++ sugg2 ++ sugg1)
  App expr1 expr2 ->
    let (result1, sugg1) = evalConstant expr1
        (result2, sugg2) = evalConstant expr2
    in (App result1 result2, sugg1 ++ sugg2)
  Lam nom expr ->
    let (result, sugg) = evalConstant expr
    in (Lam nom result, sugg)
  If cond expr1 expr2 ->
    let (result1, sugg1) = evalConstant cond
        (result2, sugg2) = evalConstant expr1
        (result3, sugg3) = evalConstant expr2
    in (If result1 result2 result3, sugg2 ++ sugg3 ++ sugg1)
  Infix op expr1 expr2 ->
    let (result1, sugg1) = evalConstant expr1
        (result2, sugg2) = evalConstant expr2
    in case result1 of 
      Lit lit1 -> case result2 of
        Lit lit2 -> 
          let (res, changed) = evalOp op lit1 lit2
              exprSugg = LintCompCst (Infix op (Lit lit1) (Lit lit2)) (Lit res)
          in case changed of
            True -> ((Lit res), sugg1 ++ sugg2 ++ exprSugg:[])
            otherwise -> (Infix op (Lit lit1) (Lit lit2), sugg1 ++ sugg2)
        otherwise -> (Infix op result1 result2, sugg1 ++ sugg2)
      otherwise -> (Infix op result1 result2, sugg1 ++ sugg2)


--------------------------------------------------------------------------------
-- Reduce expresiones aritméticas/booleanas
-- Construye sugerencias de la forma (LintCompCst e r)
lintComputeConstant :: Linting Expr
lintComputeConstant expr = evalConstant expr

--------------------------------------------------------------------------------
-- Eliminación de chequeos redundantes de booleanos
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Elimina chequeos de la forma e == True, True == e, e == False y False == e
-- Construye sugerencias de la forma (LintBool e r)
lintRedBool :: Linting Expr
lintRedBool = undefined


--------------------------------------------------------------------------------
-- Eliminación de if redundantes
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Sustitución de if con literal en la condición por la rama correspondiente
-- Construye sugerencias de la forma (LintRedIf e r)
lintRedIfCond :: Linting Expr
lintRedIfCond = undefined

--------------------------------------------------------------------------------
-- Sustitución de if por conjunción entre la condición y su rama _then_
-- Construye sugerencias de la forma (LintRedIf e r)
lintRedIfAnd :: Linting Expr
lintRedIfAnd = undefined

--------------------------------------------------------------------------------
-- Sustitución de if por disyunción entre la condición y su rama _else_
-- Construye sugerencias de la forma (LintRedIf e r)
lintRedIfOr :: Linting Expr
lintRedIfOr = undefined

--------------------------------------------------------------------------------
-- Chequeo de lista vacía
--------------------------------------------------------------------------------
-- Sugiere el uso de null para verificar si una lista es vacía
-- Construye sugerencias de la forma (LintNull e r)

lintNull :: Linting Expr
lintNull = undefined

--------------------------------------------------------------------------------
-- Eliminación de la concatenación
--------------------------------------------------------------------------------
-- se aplica en casos de la forma (e:[] ++ es), reemplazando por (e:es)
-- Construye sugerencias de la forma (LintAppend e r)

lintAppend :: Linting Expr
lintAppend = undefined

--------------------------------------------------------------------------------
-- Composición
--------------------------------------------------------------------------------
-- se aplica en casos de la forma (f (g t)), reemplazando por (f . g) t
-- Construye sugerencias de la forma (LintComp e r)

lintComp :: Linting Expr
lintComp = undefined


--------------------------------------------------------------------------------
-- Eta Redución
--------------------------------------------------------------------------------
-- se aplica en casos de la forma \x -> e x, reemplazando por e
-- Construye sugerencias de la forma (LintEta e r)

lintEta :: Linting Expr
lintEta = undefined


--------------------------------------------------------------------------------
-- Eliminación de recursión con map
--------------------------------------------------------------------------------

-- Sustituye recursión sobre listas por `map`
-- Construye sugerencias de la forma (LintMap f r)
lintMap :: Linting FunDef
lintMap = undefined


--------------------------------------------------------------------------------
-- Combinación de Lintings
--------------------------------------------------------------------------------

getExpr :: FunDef -> Expr
getExpr (FunDef n e) = e

getName :: FunDef -> Name 
getName (FunDef n e) = n

-- Dada una transformación a nivel de expresión, se construye
-- una transformación a nivel de función
liftToFunc :: Linting Expr -> Linting FunDef
liftToFunc lintExpr func = 
    let (resultExpr, suggExpr) = lintExpr (getExpr func)
        funcResult = FunDef (getName func) resultExpr
        in (funcResult, suggExpr)

-- encadenar transformaciones:
(>==>) :: Linting a -> Linting a -> Linting a
lint1 >==> lint2 = \expr ->
    let (result1, sugg1) = lint1 expr
        (resultFinal, sugg2) = lint2 result1
    in (resultFinal, sugg1 ++ sugg2)

-- aplica las transformaciones 'lints' repetidas veces y de forma incremental,
-- hasta que ya no generen más cambios en 'func'
lintRec :: Linting a -> Linting a
lintRec lints func = 
    let (resultLint, listSugg) = lints func
        in if (null listSugg) then (resultLint, listSugg)
            else 
                let (resultLintNuevo, listSugg2) = lintRec lints resultLint
                in (resultLintNuevo, listSugg ++ listSugg2)
