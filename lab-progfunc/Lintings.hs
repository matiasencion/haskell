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
freeVariables (Lam x expr)  = [y | y <- freeVariables (expr), y /= x]
freeVariables (Case expr1 expr2 (x, xs, expr3)) = freeVariables (expr1) ++ freeVariables (expr2) ++ [y | y <- freeVariables (expr3), y /= x && y /= x]
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

lintComputeConstant expr = (expr, [])

--------------------------------------------------------------------------------
-- Eliminación de chequeos redundantes de booleanos
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Elimina chequeos de la forma e == True, True == e, e == False y False == e
-- Construye sugerencias de la forma (LintBool e r)
lintRedBool :: Linting Expr
lintRedBool = undefined
{-
lintRedBool (Infix Eq (Lit (LitBool x)) expr)
    | x = (newExpr, sugg ++ [LintBool (Infix Eq (Lit (LitBool x)) expr) newExpr])
    | otherwise = (not newExpr, sugg ++ [LintBool (Infix Eq (Lit (LitBool x)) expr) (not newExpr)])
        where (newExpr, sugg) = lintRedBool expr

lintRedBool (Infix Eq expr (Lit (LitBool x)))
    | x = (newExpr, sugg ++ [LintBool (Infix Eq expr (Lit (LitBool x))) newExpr])
    | otherwise = (not newExpr, sugg ++ [LintBool (Infix Eq expr (Lit (LitBool x))) (not newExpr)])
        where (newExpr, sugg) = lintRedBool expr

lintRedBool (Infix op expr1 expr2) = (Infix op newExpr1 newExpr2, sugg1 ++ sugg2)
                                    where (newExpr1, sugg1) = lintRedBool expr1
                                          (newExpr2, sugg2) = lintRedBool expr2

lintRedBool (App expr1 expr2) = (App newExpr1 newExpr2, sugg1 ++ sugg2)
                                        where (newExpr1, sugg1) = lintRedBool expr1
                                              (newExpr2, sugg2) = lintRedBool expr2
                                        
lintRedBool (Lam n expr) = (Lam n newExpr, sugg)
                                        where (newExpr, sugg) = lintRedBool expr

lintRedBool (Case expr1 expr2 (n1, n2, expr3)) = (Case newExpr1 newExpr2 (n1, n2, newExpr3), sugg1 ++ sugg2 ++ sugg3)
                                                        where (newExpr1, sugg1) = lintRedBool expr1
                                                              (newExpr2, sugg2) = lintRedBool expr2
                                                              (newExpr3, sugg3) = lintRedBool expr3
                    
lintRedBool (If expr1 expr2 expr3) = (If newExpr1 newExpr2 newExpr3, sugg1 ++ sugg2 ++ sugg3)
                                            where (newExpr1, sugg1) = lintRedBool expr1
                                                  (newExpr2, sugg2) = lintRedBool expr2
                                                  (newExpr3, sugg3) = lintRedBool expr3

lintRedBool expr = (expr, [])
-}
--------------------------------------------------------------------------------
-- Eliminación de if redundantes
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Sustitución de if con literal en la condición por la rama correspondiente
-- Construye sugerencias de la forma (LintRedIf e r)

lintRedIfCond :: Linting Expr
lintRedIfCond (If (Lit (LitBool True)) expr1 expr2) = (newExpr1, sugg1 ++ [LintRedIf (If (Lit (LitBool True)) expr1 expr2) newExpr1])
                                                    where (newExpr1, sugg1) = lintRedIfCond expr1

lintRedIfCond (If (Lit (LitBool False)) expr1 expr2) = (newExpr2, sugg2 ++ [LintRedIf (If (Lit (LitBool False)) expr1 expr2) newExpr2])
                                                    where (newExpr2, sugg2) = lintRedIfCond expr2

lintRedIfCond (Infix op expr1 expr2) = (Infix op newExpr1 newExpr2, sugg1 ++ sugg2)
                                    where (newExpr1, sugg1) = lintRedIfCond expr1
                                          (newExpr2, sugg2) = lintRedIfCond expr2

lintRedIfCond (App expr1 expr2) = (App newExpr1 newExpr2, sugg1 ++ sugg2)
                                where (newExpr1, sugg1) = lintRedIfCond expr1
                                      (newExpr2, sugg2) = lintRedIfCond expr2
                                        
lintRedIfCond (Lam n expr) = (Lam n newExpr, sugg)
                            where (newExpr, sugg) = lintRedIfCond expr

lintRedIfCond (Case expr1 expr2 (n1, n2, expr3)) = (Case newExpr1 newExpr2 (n1, n2, newExpr3), sugg1 ++ sugg2 ++ sugg3)
                                                where (newExpr1, sugg1) = lintRedIfCond expr1
                                                      (newExpr2, sugg2) = lintRedIfCond expr2
                                                      (newExpr3, sugg3) = lintRedIfCond expr3

lintRedIfCond (If expr1 expr2 expr3) = (If newExpr1 newExpr2 newExpr3, sugg1 ++ sugg2 ++ sugg3)
                                    where (newExpr1, sugg1) = lintRedIfCond expr1
                                          (newExpr2, sugg2) = lintRedIfCond expr2
                                          (newExpr3, sugg3) = lintRedIfCond expr3

lintRedIfCond expr = (expr, [])
--------------------------------------------------------------------------------
-- Sustitución de if por conjunción entre la condición y su rama _then_
-- Construye sugerencias de la forma (LintRedIf e r)
lintRedIfAnd :: Linting Expr
lintRedIfAnd (If expr1 expr2 (Lit (LitBool False))) = (Infix And newExpr1 newExpr2, sugg1 ++ sugg2 ++ [LintRedIf (If expr1 expr2 (Lit (LitBool False))) (Infix And newExpr1 newExpr2)])
                                                    where (newExpr1, sugg1) = lintRedIfAnd expr1
                                                          (newExpr2, sugg2) = lintRedIfAnd expr2

lintRedIfAnd (Infix op expr1 expr2) = (Infix op newExpr1 newExpr2, sugg1 ++ sugg2)
                                    where (newExpr1, sugg1) = lintRedIfAnd expr1
                                          (newExpr2, sugg2) = lintRedIfAnd expr2

lintRedIfAnd (App expr1 expr2) = (App newExpr1 newExpr2, sugg1 ++ sugg2)
                                where (newExpr1, sugg1) = lintRedIfAnd expr1
                                      (newExpr2, sugg2) = lintRedIfAnd expr2
                                        
lintRedIfAnd (Lam n expr) = (Lam n newExpr, sugg)
                        where (newExpr, sugg) = lintRedIfAnd expr

lintRedIfAnd (Case expr1 expr2 (n1, n2, expr3)) = (Case newExpr1 newExpr2 (n1, n2, newExpr3), sugg1 ++ sugg2 ++ sugg3)
                                                where (newExpr1, sugg1) = lintRedIfAnd expr1
                                                      (newExpr2, sugg2) = lintRedIfAnd expr2
                                                      (newExpr3, sugg3) = lintRedIfAnd expr3

lintRedIfAnd (If expr1 expr2 expr3) = (If newExpr1 newExpr2 newExpr3, sugg1 ++ sugg2 ++ sugg3)
                                    where (newExpr1, sugg1) = lintRedIfAnd expr1
                                          (newExpr2, sugg2) = lintRedIfAnd expr2
                                          (newExpr3, sugg3) = lintRedIfAnd expr3

lintRedIfAnd expr = (expr, [])

--------------------------------------------------------------------------------
-- Sustitución de if por disyunción entre la condición y su rama _else_
-- Construye sugerencias de la forma (LintRedIf e r)
lintRedIfOr :: Linting Expr
lintRedIfOr (If expr1 (Lit (LitBool True)) expr2) = (Infix Or newExpr1 newExpr2, sugg1 ++ sugg2 ++ [LintRedIf (If expr1 (Lit (LitBool True)) expr2) (Infix Or newExpr1 newExpr2)])
                                                    where (newExpr1, sugg1) = lintRedIfOr expr1
                                                          (newExpr2, sugg2) = lintRedIfOr expr2

lintRedIfOr (Infix op expr1 expr2) = (Infix op newExpr1 newExpr2, sugg1 ++ sugg2)
                                    where (newExpr1, sugg1) = lintRedIfOr expr1
                                          (newExpr2, sugg2) = lintRedIfOr expr2

lintRedIfOr (App expr1 expr2) = (App newExpr1 newExpr2, sugg1 ++ sugg2)
                                where (newExpr1, sugg1) = lintRedIfOr expr1
                                      (newExpr2, sugg2) = lintRedIfOr expr2
                                        
lintRedIfOr (Lam n expr) = (Lam n newExpr, sugg)
                        where (newExpr, sugg) = lintRedIfOr expr

lintRedIfOr (Case expr1 expr2 (n1, n2, expr3)) = (Case newExpr1 newExpr2 (n1, n2, newExpr3), sugg1 ++ sugg2 ++ sugg3)
                                                where (newExpr1, sugg1) = lintRedIfOr expr1
                                                      (newExpr2, sugg2) = lintRedIfOr expr2
                                                      (newExpr3, sugg3) = lintRedIfOr expr3

lintRedIfOr (If expr1 expr2 expr3) = (If newExpr1 newExpr2 newExpr3, sugg1 ++ sugg2 ++ sugg3)
                                    where (newExpr1, sugg1) = lintRedIfOr expr1
                                          (newExpr2, sugg2) = lintRedIfOr expr2
                                          (newExpr3, sugg3) = lintRedIfOr expr3

lintRedIfOr expr = (expr, [])
--------------------------------------------------------------------------------
-- Chequeo de lista vacía
--------------------------------------------------------------------------------
-- Sugiere el uso de null para verificar si una lista es vacía
-- Construye sugerencias de la forma (LintNull e r)

lintNull :: Linting Expr
lintNull = undefined
{-lintNull (Infix Eq expr (Lit (LitNil))) = (null newExpr, sugg ++ [LintNull (Infix Eq expr (Lit (LitNil))) (null newExpr)])
                                        where (newExpr, sugg) = lintNull expr
lintNull (Infix Eq (Lit (LitNil)) expr) = (null newExpr, sugg ++ [LintNull (Infix Eq (Lit (LitNil)) expr) (null newExpr)])
                                        where (newExpr, sugg) = lintNull expr
lintNull (Infix Eq (Lam "lenght" expr) (Lit (LitInt 0))) = (null newExpr, sugg ++ [LintNull (Infix Eq (Lam "lenght" expr) (Lit (LitInt 0))) (null newExpr)])
                                                        where (newExpr, sugg) = lintNull expr
lintNull (Infix Eq (Lit (LitInt 0)) (Lam "lenght" expr)) = (null newExpr, sugg ++ [LintNull (Infix Eq (Lit (LitInt 0)) (Lam "lenght" expr)) (null newExpr)])
                                                        where (newExpr, sugg) = lintNull expr

lintNull (Infix op expr1 expr2) = (Infix op newExpr1 newExpr2, sugg1 ++ sugg2)
                                where (newExpr1, sugg1) = lintNull expr1
                                      (newExpr2, sugg2) = lintNull expr2

lintNull (App expr1 expr2) = (App newExpr1 newExpr2, sugg1 ++ sugg2)
                            where (newExpr1, sugg1) = lintNull expr1
                                  (newExpr2, sugg2) = lintNull expr2
                                        
lintNull (Lam n expr) = (Lam n newExpr, sugg)
                        where (newExpr, sugg) = lintNull expr

lintNull (Case expr1 expr2 (n1, n2, expr3)) = (Case newExpr1 newExpr2 (n1, n2, newExpr3), sugg1 ++ sugg2 ++ sugg3)
                                            where (newExpr1, sugg1) = lintNull expr1
                                                  (newExpr2, sugg2) = lintNull expr2
                                                  (newExpr3, sugg3) = lintNull expr3

lintNull (If expr1 expr2 expr3) = (If newExpr1 newExpr2 newExpr3, sugg1 ++ sugg2 ++ sugg3)
                                where (newExpr1, sugg1) = lintNull expr1
                                      (newExpr2, sugg2) = lintNull expr2
                                      (newExpr3, sugg3) = lintNull expr3

lintNull expr = (expr, [])-}
--------------------------------------------------------------------------------
-- Eliminación de la concatenación
--------------------------------------------------------------------------------
-- se aplica en casos de la forma (e:[] ++ es), reemplazando por (e:es)
-- Construye sugerencias de la forma (LintAppend e r)

lintAppend :: Linting Expr
lintAppend (Infix Cons expr1 (Infix Append (Lit (LitNil)) expr2)) = (Infix Cons newExpr1 newExpr2, sugg1 ++ sugg2 ++ [LintAppend (Infix Cons expr1 (Infix Append (Lit (LitNil)) expr2)) (Infix Cons newExpr1 newExpr2)])
                                                                    where (newExpr1, sugg1) = lintAppend expr1
                                                                          (newExpr2, sugg2) = lintAppend expr2

lintAppend (Infix op expr1 expr2) = (Infix op newExpr1 newExpr2, sugg1 ++ sugg2)
                                    where (newExpr1, sugg1) = lintAppend expr1
                                          (newExpr2, sugg2) = lintAppend expr2

lintAppend (App expr1 expr2) = (App newExpr1 newExpr2, sugg1 ++ sugg2)
                                where (newExpr1, sugg1) = lintAppend expr1
                                      (newExpr2, sugg2) = lintAppend expr2
                                        
lintAppend (Lam n expr) = (Lam n newExpr, sugg)
                        where (newExpr, sugg) = lintAppend expr

lintAppend (Case expr1 expr2 (n1, n2, expr3)) = (Case newExpr1 newExpr2 (n1, n2, newExpr3), sugg1 ++ sugg2 ++ sugg3)
                                                where (newExpr1, sugg1) = lintAppend expr1
                                                      (newExpr2, sugg2) = lintAppend expr2
                                                      (newExpr3, sugg3) = lintAppend expr3

lintAppend (If expr1 expr2 expr3) = (If newExpr1 newExpr2 newExpr3, sugg1 ++ sugg2 ++ sugg3)
                                    where (newExpr1, sugg1) = lintAppend expr1
                                          (newExpr2, sugg2) = lintAppend expr2
                                          (newExpr3, sugg3) = lintAppend expr3

lintAppend expr = (expr, [])
--------------------------------------------------------------------------------
-- Composición
--------------------------------------------------------------------------------
-- se aplica en casos de la forma (f (g t)), reemplazando por (f . g) t
-- Construye sugerencias de la forma (LintComp e r)

lintComp :: Linting Expr
lintComp (App expr1 (App expr2 expr3)) = (Infix Comp expr1 newExpr, sugg ++ [LintComp (App expr1 (App expr2 expr3)) (Infix Comp expr1 newExpr)]) 
                                          where (newExpr, sugg) = lintComp (App expr2 expr3)

lintComp (Infix op expr1 expr2) = (Infix op newExpr1 newExpr2, sugg1 ++ sugg2)
                              where (newExpr1, sugg1) = lintComp expr1
                                    (newExpr2, sugg2) = lintComp expr2

lintComp (App expr1 expr2) = (App newExpr1 newExpr2, sugg1 ++ sugg2)
                              where (newExpr1, sugg1) = lintComp expr1
                                    (newExpr2, sugg2) = lintComp expr2

lintComp (Lam x expr) = (Lam x newExpr, sugg)
                        where (newExpr, sugg) = lintComp expr

lintComp (Case expr1 expr2 (x,xs,expr3)) = (Case newExpr1 newExpr2 (x,xs,newExpr3), sugg1 ++ sugg2 ++ sugg3)
                                          where (newExpr1, sugg1) = lintComp expr1
                                                (newExpr2, sugg2) = lintComp expr2
                                                (newExpr3, sugg3) = lintComp expr3

lintComp (If expr1 expr2 expr3) = (If newExpr1 newExpr2 newExpr3, sugg1 ++ sugg2 ++ sugg3)
                              where (newExpr1, sugg1) = lintComp expr1
                                    (newExpr2, sugg2) = lintComp expr2
                                    (newExpr3, sugg3) = lintComp expr3

lintComp expr = (expr, [])

--------------------------------------------------------------------------------
-- Eta Redución
--------------------------------------------------------------------------------
-- se aplica en casos de la forma \x -> e x, reemplazando por e
-- Construye sugerencias de la forma (LintEta e r)

lintEta :: Linting Expr
lintEta (Lam x (App expr (Var y))) = if x == y && elem x (freeVariables expr) then (Lam x (App expr (Var x)), []) else (expr, [LintEta (Lam x (App expr (Var x))) expr])

lintEta (Infix op expr1 expr2) = (Infix op newExpr1 newExpr2, sugg1 ++ sugg2)
                              where (newExpr1, sugg1) = lintEta expr1
                                    (newExpr2, sugg2) = lintEta expr2

lintEta (App expr1 expr2) = (App newExpr1 newExpr2, sugg1 ++ sugg2)
                              where (newExpr1, sugg1) = lintEta expr1
                                    (newExpr2, sugg2) = lintEta expr2

lintEta (Lam x expr) = (Lam x newExpr, sugg)
                        where (newExpr, sugg) = lintEta expr

lintEta (Case expr1 expr2 (x,xs,expr3)) = (Case newExpr1 newExpr2 (x,xs,newExpr3), sugg1 ++ sugg2 ++ sugg3)
                                          where (newExpr1, sugg1) = lintEta expr1
                                                (newExpr2, sugg2) = lintEta expr2
                                                (newExpr3, sugg3) = lintEta expr3

lintEta (If expr1 expr2 expr3) = (If newExpr1 newExpr2 newExpr3, sugg1 ++ sugg2 ++ sugg3)
                              where (newExpr1, sugg1) = lintEta expr1
                                    (newExpr2, sugg2) = lintEta expr2
                                    (newExpr3, sugg3) = lintEta expr3

lintEta expr = (expr, [])

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
lint1 >==> lint2 = \x -> 
      let (res1, sugg1) = lint1 x
          (res2, sugg2) = lint2 res1
      in (res2, sugg1 ++ sugg2)

-- aplica las transformaciones 'lints' repetidas veces y de forma incremental,
-- hasta que ya no generen más cambios en 'func'
lintRec :: Linting a -> Linting a
lintRec lints func = undefined
