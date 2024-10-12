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

merge :: Ord a => [a] -> [a] -> [a]
merge x [] = x
merge [] x = x
merge (x:xs) (y:ys) = if x < y then x:y:merge (xs) (ys) else y:x:merge (xs) (ys)

sumSqsf :: Num a => [a] -> a
sumSqsf x = foldr (\x y -> x*x + y) 0 x

sumSqsTR :: Num a => [a] -> a
sumSqsTR x = sumSqsTRAux x 0
  where 
    sumSqsTRAux [] n = n
    sumSqsTRAux (x:xs) n = sumSqsTRAux (xs) (x*x + n)

elemTR :: Eq a => a -> [a] -> Bool
elemTR e x = elemTRAux e x False
  where
    elemTRAux _ [] n = n
    elemTRAux e (x:xs) n = elemTRAux e (xs) (e == x || n)

elimDupsTR :: Eq a => [a] -> [a]
elimDupsTR [] = []
elimDupsTR (x:xs) = elimDupsTRAux xs x [x]
  where
    elimDupsTRAux :: Eq a => [a] -> a -> [a] -> [a]
    elimDupsTRAux [] last acc = acc
    elimDupsTRAux (x:xs) last (acc) = if x /= last then elimDupsTRAux (xs) (x) (acc ++ [x]) else elimDupsTRAux (xs) (last) (acc)

splitTR :: [a] -> ([a],[a])
splitTR x = splitTRAux x True ([],[])
  where
    splitTRAux [] _ (a,b) = (a,b)
    splitTRAux (x:xs) True (a,b) = splitTRAux xs False (a ++ [x],b)
    splitTRAux (x:xs) False (a,b) = splitTRAux xs True (a,b ++ [x])

maxIndTR :: Ord a => [a] -> (a, Int)
maxIndTR (x:xs) = maxIndTRAux xs x 1 1
  where
    maxIndTRAux [] max ind _= (max, ind)
    maxIndTRAux (x:xs) max ind count = if x > max then maxIndTRAux (xs) (x) (ind + count) 1 else maxIndTRAux (xs) (max) (ind) (count+1) 

takeWhileTR :: (a -> Bool) -> [a] -> [a]
takeWhileTR f x = takeWhileTRAux f x []
  where
    takeWhileTRAux _ [] acc = acc
    takeWhileTRAux f (x:xs) acc = if f (x) then takeWhileTRAux (f) (xs) (acc ++ [x]) else acc
  
dropWhileTR :: (a -> Bool) -> [a] -> [a]
dropWhileTR f x = dropWhileTRAux f x True []
  where 
    dropWhileTRAux _ [] _ acc = acc
    dropWhileTRAux f (x:xs) False acc = dropWhileTRAux (f) (xs) False (acc ++ [x])
    dropWhileTRAux f (x:xs) True acc = if f (x) then dropWhileTRAux (f) (xs) True (acc) else dropWhileTRAux (f) (xs) False (acc ++ [x])