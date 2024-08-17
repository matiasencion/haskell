module Prac1 where
    
sumsqrs :: Integer -> Integer -> Integer -> Integer
sumsqrs x y z 
    | max x y == x && max x z == x  = if max y z == y then (x^2 + y^2) else (x^2 + z^2)
    | max x y == x  = (z^2 + x^2)--SI LA SEGUNDA CONDICION NO SE CUMPLE
    | max x z == x  = (y^2 + x^2)--SI LA PRIMERA CONDICION NO SE CUMPLE
    | otherwise = (y^2 + z^2)--SI NINGUNA DE LAS DOS CONDICIONES SE CUMPLE

analize :: Int -> Int -> Int -> Bool
analize a b c = c^2 == a^2 + b^2

myAnd :: Bool -> Bool -> Bool
myAnd x y
    | x && y    = True
    |otherwise  = False

myAnd1 :: Bool -> Bool -> Bool
myAnd1 True True = True
myAnd1 False True = False
myAnd1 True False = False
myAnd1 False False = False

myOr :: Bool -> Bool -> Bool
myOr x y
    | x || y    = True
    |otherwise  = False

myOr1 :: Bool -> Bool -> Bool
myOr1 True True = True
myOr1 False True = True
myOr1 True False = True
myOr1 False False = False

implica :: Bool -> Bool -> Bool
implica x y = not x || y

type Fecha = (Int, Int, Int)
calcularEdad :: Fecha -> Fecha -> Int
calcularEdad (x,y,z) (d,m,a)
    | x >= d && y == m || m >= y = a - z
    | otherwise = a - z - 1