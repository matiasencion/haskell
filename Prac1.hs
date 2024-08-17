module Prac1 where
    
    max3 :: Integer -> Integer -> Integer -> Integer
    max3 x y z = if (max(x,y) == x && max(x,z) == x) then x else if (max(y,x) == y && max(y,z) == y) then y else z

    sumsqrs :: Integer -> Integer -> Integer -> Integer
    sumsqrs x y z = if max3(x,y,z) == x then if max(y,z) == y then (x*x + y*y) else (x*x + z*z) else if max3(x,y,x) == y then if max(x,z) == x then (y*y + x*x) else (y*y + z*z) else if max(x,y) == x then (z*z + x*x) else (z*z + y*y)