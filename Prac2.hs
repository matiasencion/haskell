import Prelude hiding (flip, map, filter, length, all)

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