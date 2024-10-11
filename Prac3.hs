import Prelude hiding (elem)

sumSqs :: Num a => [a] -> a
sumSqs [] = 0
sumSqs (x:xs) = x*x + sumSqs xs

elem :: Eq a => a -> [a] -> Bool
elem _ [] = False
elem e (x:xs) = e == x || elem e xs

elimDups :: Eq a => [a] -> [a]
elimDups [] = []
elimDups [x] = [x]
elimDups (x:xs) = if x /= head xs then x:elimDups (xs) else elimDups (xs)

split :: [a] -> ([a], [a])
split [] = ([], [])  
split [x] = ([x], [])
split (x:y:xs) = (x:xs1, y:xs2)
  where (xs1, xs2) = split xs  