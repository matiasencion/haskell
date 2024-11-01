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

computeConstant :: Op -> Lit -> Lit -> (Expr, [LintSugg])
computeConstant Add (LitInt n) (LitInt m)
    | (n + m) >= 0 = (Lit (LitInt (n + m)), [LintCompCst (Infix Add (Lit (LitInt n)) (Lit (LitInt m))) (Lit (LitInt (n + m)))])
    | otherwise = (Infix Add (Lit (LitInt n)) (Lit (LitInt m)), [])

computeConstant Sub (LitInt n) (LitInt m)
    | (n - m) >= 0 = (Lit (LitInt (n - m)), [LintCompCst (Infix Sub (Lit (LitInt n)) (Lit (LitInt m))) (Lit (LitInt (n - m)))])
    | otherwise = (Infix Sub (Lit (LitInt n)) (Lit (LitInt m)), [])

computeConstant Mult (LitInt n) (LitInt m)
    | (n * m) >= 0 = (Lit (LitInt (n * m)), [LintCompCst (Infix Mult (Lit (LitInt n)) (Lit (LitInt m))) (Lit (LitInt (n * m)))])
    | otherwise = (Infix Mult (Lit (LitInt n)) (Lit (LitInt m)), [])

computeConstant Div (LitInt n) (LitInt m)
    | (m /= 0) && (n `div` m) >= 0 = (Lit (LitInt (n `div` m)), [LintCompCst (Infix Div (Lit (LitInt n)) (Lit (LitInt m))) (Lit (LitInt (n `div` m)))])
    | otherwise = (Infix Div (Lit (LitInt n)) (Lit (LitInt m)), [])

computeConstant And (LitBool n) (LitBool m) = (Lit (LitBool (n && m)), [LintCompCst (Infix And (Lit (LitBool n)) (Lit (LitBool m))) (Lit (LitBool (n && m)))])

computeConstant Or (LitBool n) (LitBool m) = (Lit (LitBool (n || m)), [LintCompCst (Infix And (Lit (LitBool n)) (Lit (LitBool m))) (Lit (LitBool (n || m)))])


lintComputeConstant :: Linting Expr
lintComputeConstant (Infix op (Lit lit1) (Lit lit2)) = computeConstant op lit1 lit2
lintComputeConstant (Infix op expr1 expr2) = (Infix op newExpr1 newExpr2, sugg1 ++ sugg2)
                                            where (newExpr1, sugg1) = lintComputeConstant expr1
                                                  (newExpr2, sugg2) = lintComputeConstant expr2

lintComputeConstant (App expr1 expr2) = (App newExpr1 newExpr2, sugg1 ++ sugg2)
                                        where (newExpr1, sugg1) = lintComputeConstant expr1
                                              (newExpr2, sugg2) = lintComputeConstant expr2
                                        
lintComputeConstant (Lam n expr) = (Lam n newExpr, sugg)
                                        where (newExpr, sugg) = lintComputeConstant expr

lintComputeConstant (Case expr1 expr2 (n1, n2, expr3)) = (Case newExpr1 newExpr2 (n1, n2, newExpr3), sugg1 ++ sugg2 ++ sugg3)
                                                        where (newExpr1, sugg1) = lintComputeConstant expr1
                                                              (newExpr2, sugg2) = lintComputeConstant expr2
                                                              (newExpr3, sugg3) = lintComputeConstant expr3
                    
lintComputeConstant (If expr1 expr2 expr3) = (If newExpr1 newExpr2 newExpr3, sugg1 ++ sugg2 ++ sugg3)
                                            where (newExpr1, sugg1) = lintComputeConstant expr1
                                                  (newExpr2, sugg2) = lintComputeConstant expr2
                                                  (newExpr3, sugg3) = lintComputeConstant expr3

lintComputeConstant expr = (expre, [])

--------------------------------------------------------------------------------
-- Eliminación de chequeos redundantes de booleanos
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Elimina chequeos de la forma e == True, True == e, e == False y False == e
-- Construye sugerencias de la forma (LintBool e r)
lintRedBool :: Linting Expr
lintRedBool (Infix Eq (Lit (LitBool x)) expr)
    | x = (expr, [LintBool (Infix Eq (Lit (LitBool x)) expr) expr])
    | otherwise = (not expr, [LintBool (Infix Eq (Lit (LitBool x)) expr) (not expr)])

lintRedBool (Infix Eq expr (Lit (LitBool x)))
    | x = (expr, [LintBool expr (Infix Eq (Lit (LitBool x))) expr])
    | otherwise = (not expr, [LintBool expr (Infix Eq (Lit (LitBool x))) (not expr)])

lintRedBool (Infix op expr1 expr2) = (Infix op newExpr1 newExpr2, sugg1 ++ sugg2)
                                    where (newExpr1, sugg1) = lintRedBool expr1
                                          (newExpr2, sugg2) = lintRedBool expr2

lintRedBool expr = (expr, [])

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
