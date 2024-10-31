module Lintings where

import AST
import LintTypes


--------------------------------------------------------------------------------
-- AUXILIARES
--------------------------------------------------------------------------------

-- Computa la lista de variables libres de una expresión
freeVariables :: Expr -> [Name]
freeVariables (Var v) = [v]
freeVariables (Lit _) = []
freeVariables (Infix _ expr1 expr2) = freeVariables (expr1) ++ freeVariables (expr2)
freeVariables (App expr1 expr2) = freeVariables (expr1) ++ freeVariables (expr2)
freeVariables (Lam _ expr)  = freeVariables (expr)
freeVariables (Case expr1 expr2 (_, _, expr3)) = freeVariables (expr1) ++ freeVariables (expr2) ++ freeVariables (expr3)
freeVariables (If expr1 expr2 expr3) = freeVariables (expr1) ++ freeVariables (expr2) ++ freeVariables (expr3)

--------------------------------------------------------------------------------
-- LINTINGS
--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
-- Computación de constantes
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Reduce expresiones aritméticas/booleanas
-- Construye sugerencias de la forma (LintCompCst e r)
litToInt :: Lit -> Integer
litToInt (LitInt a) = a

litToBool :: Lit -> Bool
litToBool (LitBool a) = a


computeConstantOp :: Op -> Lit -> Lit -> [LintSugg]
computeConstantOp (Add) (LitInt int1) (LitInt int2)
    | (int1 + int2) >= 0 = [(LintCompCst (Infix (Add) (Lit (LitInt int1)) (Lit (LitInt int2))) (Lit (LitInt (int1 + int2))))]
    | otherwise = []
computeConstantOp (Sub) (LitInt int1) (LitInt int2)
    | (int1 - int2) >= 0 = [(LintCompCst (Infix (Add) (Lit (LitInt int1)) (Lit (LitInt int2))) (Lit (LitInt (int1 - int2))))]
    | otherwise = []
computeConstantOp (Mult) (LitInt int1) (LitInt int2)
    | (int1 * int2) >= 0 = [(LintCompCst (Infix (Add) (Lit (LitInt int1)) (Lit (LitInt int2))) (Lit (LitInt (int1 * int2))))]
    | otherwise = []
computeConstantOp (Div) (LitInt int1) (LitInt int2)
    | (int2 /= 0) && (div int1 int2) >= 0 = [(LintCompCst (Infix (Add) (Lit (LitInt int1)) (Lit (LitInt int2))) (Lit (LitInt (div int1 int2))))]
    | otherwise = []


lintComputeConstant :: Linting Expr
lintComputeConstant (Infix op (Lit lit1) (Lit lit2)) = ((Infix op (Lit lit1) (Lit lit2)), computeConstantOp(op, lit1, lit2))
lintComputeConstant (Infix op expr1 expr2) = lintComputeConstant



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


-- Dada una transformación a nivel de expresión, se construye
-- una transformación a nivel de función
liftToFunc :: Linting Expr -> Linting FunDef
liftToFunc = undefined

-- encadenar transformaciones:
(>==>) :: Linting a -> Linting a -> Linting a
lint1 >==> lint2 = undefined

-- aplica las transformaciones 'lints' repetidas veces y de forma incremental,
-- hasta que ya no generen más cambios en 'func'
lintRec :: Linting a -> Linting a
lintRec lints func = undefined
