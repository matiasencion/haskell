module Prac1 where
    
    sumsqrs :: Integer -> Integer -> Integer -> Integer
    sumsqrs x y z 
        | max x y == x && max x z == x  = if max y z == y then (x^2 + y^2) else (x^2 + z^2)
        | max x y == x  = (z^2 + x^2)--SI LA SEGUNDA CONDICION NO SE CUMPLE
        | max x z == x  = (y^2 + x^2)--SI LA PRIMERA CONDICION NO SE CUMPLE
        | otherwise = (y^2 + z^2)--SI NINGUNA DE LAS DOS CONDICIONES SE CUMPLE