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

type Curso = (String, Int, Int)--NOMBRE CODIGO NOTA
type Estudiante = (String, Integer, Int, [Curso]) -- NOMBRE CI ANIO_DE_INGRESO CURSOS_APROVADOS

gal, calculo, fisica :: Curso
gal = ("GAL", 1, 8)
calculo = ("Calculo", 2, 7)
fisica = ("Fisica", 3, 6)

matias :: Estudiante
matias = ("Matias", 52639899, 2019, [gal, fisica])

nombreCI :: Estudiante -> (String, Integer)
nombreCI (nom, ci, anio, cursos) = (nom, ci) 

anioIngreso :: Estudiante -> Int
anioIngreso (nom, ci, anio, cursos) = anio

cursosConNota :: Estudiante -> Int -> [Int]
cursosConNota (nom, ci, anio, cursos) nota = [cod | (cnom, cod, cnota) <- cursos, cnota == nota]

estudiantesAnioIngreso :: [Estudiante] -> Int -> [(String, Integer)]
estudiantesAnioIngreso estudiantes anio = [nombreCI e | e <- estudiantes, anio == anioIngreso e]

data Curso' = Curso' {
    nombreCurso :: String,
    codigo :: Int,
    nota :: Int
}

data Estudiante' = Estudiante' {
    nombreEstudiante :: String,
    ci :: Integer,
    anio :: Int,
    cursos :: [Curso']
} 

gal' = Curso' "GAL" 1 8
calculo' = Curso' "Calculo" 2 7
fisica' = Curso' "Fisica" 3 6

matias' = Estudiante' "Matias" 52639899 2019 [gal', fisica']

nombreCI' :: Estudiante' -> (String, Integer)
nombreCI' (Estudiante' nom ci _ _) = (nom, ci)

anioIngreso' :: Estudiante' -> Int
anioIngreso' (Estudiante' _ _ anio _) = anio

cursosConNota' :: Estudiante' -> Int -> [Int]
cursosConNota' (Estudiante' _ _ _ cursos) nota = [cod | (Curso' _ cod notaCurso) <- cursos, notaCurso == nota]

estudiantesAnioIngreso' :: [Estudiante'] -> Int -> [(String, Integer)]
estudiantesAnioIngreso' estudiantes anio = [(nom, ci) | (Estudiante' nom ci anioE _) <- estudiantes, anioE == anio]

data ParOrdenado = ParOrdenado Double Double
                deriving(Show)

crearParOrdenado :: Double -> Double -> ParOrdenado 
crearParOrdenado x y 
    | max x y == x  = ParOrdenado y x
    | otherwise     = ParOrdenado x y

sumarParOrdenado :: ParOrdenado -> ParOrdenado -> ParOrdenado
sumarParOrdenado (ParOrdenado a b) (ParOrdenado x y) = ParOrdenado (a+x) (b+y)

multiplicarParOrdenado :: ParOrdenado -> Double -> ParOrdenado
multiplicarParOrdenado (ParOrdenado x y) c = crearParOrdenado (x*c) y

data Triangulo = Equi Int | Iso Int Int | Esca Int Int Int
                deriving(Show)

mkTriangulo :: Int -> Int -> Int -> Triangulo
mkTriangulo a b c
    | a == b && b == c  = Equi a
    | a == b && a /= c  = Iso a c
    | a == c && a /= b  = Iso a b 
    | b == c && b /= a  = Iso b a
    | otherwise         = Esca a b c 