import Prelude hiding (flip, map, filter, length, all, elem)

twice :: (a -> a) -> (a -> a)
twice f = f.f

duplicate :: String -> Integer -> String
duplicate str n
    | n <= 0    = ""
    | n == 1    = str
    | otherwise = str ++ duplicate str (n-1)

sumaPrimeros :: [Int] -> [Int]
sumaPrimeros (x:y:xs) = (x+y):x:y:xs
sumaPrimeros x = x

flip :: (a -> b -> c) -> b -> a -> c
flip f y x = f x y

lambdaFlip :: (a -> b -> c) -> b -> a -> c
lambdaFlip f = (\y x -> f x y)

cuentas :: Integer -> Integer
cuentas x =  ((div 2).((+ (-8)).((2*).(3+)))) x

map :: (a -> b) -> [a] -> [b]
map f xs = [f x | x <- xs]

filter:: (a -> Bool) -> [a] -> [a]
filter f xs = [x | x <- xs, f x]

squares :: [Integer] -> [Integer]
squares xs = map (\x -> x*x) xs

length :: [a] -> Integer
length xs = sum (map (\x -> 1) xs)

all :: (a -> Bool) -> [a] -> Bool
all f xs = length (filter f xs) == length xs

elem :: Eq a => a -> [a] -> Bool
elem n xs = length(filter (\x -> x == n) xs) > 0

filter' :: (a -> Bool) -> [a] -> [a]
filter' p xs = concat(map box xs)
            where box ls 
                    | p ls  = [ls]
                    | otherwise = []

data Triangulo = Equi Int | Iso Int Int | Esca Int Int Int
                deriving(Eq)

isos :: [Triangulo] -> Integer
isos xs = length [x | x <- xs, case x of Iso _ _ -> True; _ -> False] 

isos' :: [Triangulo] -> Integer
isos' xs = length(filter (\x -> case x of Iso _ _ -> True; _ -> False) xs)

type Matriz a = [[a]]

columna :: Int -> Matriz a -> [a]
columna i m = [head(drop i f) | f <- m]

transpose :: Matriz a -> Matriz a
transpose m = [columna i m | i <- [0 .. fromInteger(length (m !! 1) - 1)]]