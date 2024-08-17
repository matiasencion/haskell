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

romanDigit :: Char -> String
romanDigit ch 
    | ch == '1'   = "I"
    | ch == '2'   = "II"
    | ch == '3'   = "III"
    | ch == '4'   = "IV"
    | ch == '5'   = "V"
    | ch == '6'   = "VI"
    | ch == '7'   = "VII"
    | ch == '8'   = "VIII"
    | ch == '9'   = "IX"
    | otherwise = "No es un digito.\n"

averageThree :: Integer -> Integer -> Integer -> Float
averageThree x y z = fromIntegral(x + y + z) / 3

howManyAboveAverage :: Integer -> Integer -> Integer -> Integer
howManyAboveAverage x y z 
    | fromInteger x > averageThree x y z && fromInteger y > averageThree x y z && fromInteger z > averageThree x y z = 3
    | fromInteger x > averageThree x y z && fromInteger y > averageThree x y z || fromInteger x > averageThree x y z && fromInteger z > averageThree x y z || fromInteger y > averageThree x y z && fromInteger z > averageThree x y z    = 2
    | fromInteger x > averageThree x y z || fromInteger y > averageThree x y z || fromInteger z > averageThree x y z   = 1
    | otherwise = 0

numberNDroots :: Float -> Float -> Float -> Integer
numberNDroots a b c
    | sqrt(b^2 - 4*a*c) > 0 = 2
    | sqrt(b^2 - 4*a*c) == 0   = 1
    | otherwise = 0

maxOccurs :: Integer -> Integer -> (Integer, Integer)
maxOccurs x y
    | x == y    = (x, 2)
    | x > y     = (x, 1)
    | x < y     = (y, 1)

maxThreeOccurs :: Integer -> Integer -> Integer -> (Integer, Integer)
maxThreeOccurs x y z 
    | x == y && y == z   = (x, 3)
    | x == y && x > z   = (x, 2)
    | x == z && x > y   = (x, 2)
    | y == z && y > x   = (y, 2)
    | x > y && x > z    = (x, 1)
    | y > x && y > z    = (y, 1)
    | otherwise    = (z, 1)

middleThree :: Integer -> Integer -> Integer -> Integer
middleThree x y z
    | max x y == y && max x z == x || min x y == y && min x z == x      = x
    | max x y == x && max y z == y || min x y == x && min y z == y      = y
    | max x z == x && max y z == z || min x z == x && min y z == z      = z

orderTriple :: (Integer, Integer, Integer) -> (Integer, Integer, Integer)
orderTriple (x, y, z) = (minThree x y z, middleThree x y z, maxThree x y z)

whereCrossXAxis :: Float -> Float -> Float -- RECIBE PENDIENTE Y ORDENADA EN EL ORIGEN y = a.x + b
whereCrossXAxis a b = (-b) / a

data Shape = Circle Float |
            Rectangle Float Float |
            Triangle Float Float 
            deriving (Eq, Ord, Show)

perimeter :: Shape -> Float
perimeter (Circle r) = 2*pi*r
perimeter (Rectangle a b) = 2*(a+b)
perimeter (Triangle b h) = b +  2 * sqrt((b/2)^2 + h^2)

isRound :: Shape -> Bool
isRound (Circle _)          = True
isRound (Rectangle _ _)     = False
isRound (Triangle _ _)    = False

area :: Shape -> Int
area (Circle r) = pi*r^2
area (Rectangle a b) = a*b
area (Triangle b h) = b*h/2

isRegular :: Shape -> Bool
isRegular (Circle _)        = True
isRegular (Rectangle _ _)   = True
isRegular (Triangle b h)    = h^2 == (b/2)^2 + b^2

data Item = Name String | Price Int