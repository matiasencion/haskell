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
            Rectangle Float Float
            deriving (Eq, Ord, Show)

perimeter :: Shape -> Float
perimeter (Circle r) = 2*pi*r
perimeter (Rectangle a b) = 2*(a+b)

isRound :: Shape -> Bool
isRound (Circle _)          = True
isRound (Rectangle _ _)     = False

area :: Shape -> Float
area (Circle r) = pi*r^2
area (Rectangle a b) = a*b

isRegular :: Shape -> Bool
isRegular (Circle _)        = True
isRegular (Rectangle _ _)   = True

data Item = Name String | Price Int

isDigit :: Char -> Bool
isDigit d = fromEnum d - fromEnum '0' >= 0 && fromEnum d - fromEnum '0' <= 9

digits :: String -> String
digits st = [d | d <- st, isDigit d]

isEven :: Integer -> Bool
isEven n = (mod n 2 == 0)

allEven :: [Integer] -> Bool
allEven ls = (ls == [n | n <- ls, isEven n])

allOdd :: [Integer] -> Bool
allOdd ls = ([] == [n | n <- ls, isEven n]) 

isLetter :: Char -> Bool
isLetter ch = (fromEnum ch >= 97 && fromEnum ch <= 122 || fromEnum ch >= 65 && fromEnum ch <= 90)

toCapitalLetter :: Char -> Char
toCapitalLetter ch 
    | fromEnum(ch) >= 97 && fromEnum(ch) <= 122 = toEnum (fromEnum(ch) - 32)
    | otherwise                                 = ch

capitalize :: String -> String
capitalize str = [toCapitalLetter ch | ch <- str]

capitalizeLetters :: String -> String
capitalizeLetters str = [toCapitalLetter ch | ch <- str, isLetter ch]

divisors :: Integer -> [Integer]
divisors x = [n | n <- [1 .. x], mod x n == 0]

isPrime :: Integer -> Bool
isPrime x 
    | x /= 1    = ([1, x] == divisors x)
    | otherwise = True

matches :: Integer -> [Integer] -> [Integer]
matches x ls = [n | n <- ls, x == n]

elem' :: Integer -> [Integer] -> Bool
elem' x ls = (matches x ls /= [])

onSeparateLines :: [String] -> String
onSeparateLines [] = ""
onSeparateLines (x:xs) = x ++ "\n" ++ onSeparateLines xs

duplicate :: String -> Integer -> String
duplicate str n
    | n <= 0    = ""
    | n == 1    = str
    | otherwise = str ++ duplicate str (n-1)

fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci x = fibonacci (x-1) + fibonacci (x-2)

fibTable :: Integer -> String
fibTable x
    | x == 0    = "n\t\tfib n\n0\t\t0\n"
    | otherwise = fibTable (x-1) ++ show x ++ "\t\t" ++ show (fibonacci(x)) ++ "\n"

doubleAll :: [Integer] -> [Integer]
doubleAll ls = [2*n | n <- ls]

doubleAll' :: [Integer] -> [Integer]
doubleAll' x = map double x

doubleAll'' :: [Integer] -> [Integer]
doubleAll'' [] = []
doubleAll'' (x:xs) = 2*x : doubleAll'' xs

to1 :: a -> Integer
to1 x = 1 

myLength :: [a] -> Integer
myLength x = sum (map to1 x)

greaterZero :: Integer -> Bool
greaterZero n = n > 0

less10 :: Integer -> Bool
less10 n = n < 10

addOne :: Integer -> Integer
addOne n = n + 1

addUp :: [Integer] -> [Integer]
addUp ns = map addOne (filter greaterZero ns)

add2 :: [Integer] -> [Integer]
add2 ns = map addOne (map addOne ns)

greaterZeroAndLess10 :: [Integer] -> [Integer]
greaterZeroAndLess10 ns = filter greaterZero (filter less10 ns)

squareAll :: [Integer] -> [Integer]
squareAll ns = map square ns

sumSquares :: [Integer] -> Integer
sumSquares ns = sum (squareAll ns)

checkAllGreater0 :: [Integer] -> Bool
checkAllGreater0 ns = filter greaterZero ns == ns

minF :: (Integer -> Integer) -> Integer -> Integer
minF f n 
    | n == 0    = f 0
    | otherwise = if f n <= minF f (n - 1) then f n else minF f (n - 1)

