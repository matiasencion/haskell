module FirstScript where

size :: Integer
size = 12 + 13

square :: Integer -> Integer
square n = n * n

double :: Integer -> Integer
double n = 2 * n

example :: Integer
example = double(size - square(2 + 2))

func1 :: Integer ->Integer
func1 = square.double

func2 :: Integer -> Integer
func2 = double.square

xOr :: Bool -> Bool -> Bool
xOr x y = (x == True && y == False) || (x == False && y == True)

xOrTable :: Bool -> Bool -> Bool
xOrTable True True = False
xOrTable True False = True
xOrTable False True = True
xOrTable False False = False

nAnd :: Bool -> Bool -> Bool
nAnd x y = not (x && y)

threeDifferent :: Integer -> Integer -> Integer -> Bool
threeDifferent x y z = x /= y && x /= z && y /= z

maxThree :: Integer -> Integer -> Integer -> Integer
maxThree x y z 
    | x >= y && x >= z      = x
    | y >= z                = y
    | otherwise             = z

min' :: Integer -> Integer -> Integer
min' x y =
    if x <= y then x else y

minThree :: Integer -> Integer -> Integer -> Integer
minThree x y z 
    | x <= y && x <= z      = x
    | y <= z                = y
    | otherwise             = z

charToNum :: Char -> Int
charToNum ch 
    | fromEnum(ch) <= 57 && fromEnum(ch) >= 48      = fromEnum(ch) - fromEnum('0')
    | otherwise                                     = 0

onThreeLines :: String -> String -> String -> String
onThreeLines a b c = a ++ "\n" ++ b ++ "\n" ++ c ++ "\n"