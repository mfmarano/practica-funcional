import Data.List
import Text.Show.Functions

-- 1)

data Bolita = Azul | Rojo | Verde | Negro deriving (Show, Eq)
data Direccion = Norte | Sur | Este | Oeste deriving (Show)

data Celda = Celda {
    bolitas :: [Bolita],
    posicion :: Posicion
} deriving (Show)

data Tablero = Tablero {
    tamaño :: Tamaño,
    celdas :: [Celda],
    cabezal :: Posicion
} deriving (Show)

type Tamaño = (Int, Int)
type Posicion = (Int, Int) 

-- 2)

inicializarTablero :: Tamaño -> Tablero
inicializarTablero tamaño = Tablero tamaño (crearCeldas tamaño) (1,1)

-- ****lo que se nos ocurrió con Fede****
-- con composicion
crearCeldas :: Tamaño -> [Celda]
crearCeldas (filas,columnas) = map (Celda []) (concatMap (zip [1..filas] . replicate filas) [1..columnas])

-- sin composición
-- crearCeldas' :: Tamaño -> [Posicion]
-- crearCeldas' (filas,columnas) = concatMap (\ n -> zip [1..filas] (replicate filas n)) [1..columnas]

-- ****lo que se nos ocurrió sin Fede****
-- armarMatriz :: [Tamaño] -> [Posicion]
-- armarMatriz [(1,m)] = crearListaPosiciones [(1,m)]
-- armarMatriz [(n,m)] = crearListaPosiciones [(n,m)] ++ armarMatriz [(n-1,m)]

-- crearListaPosiciones :: [Tamaño] -> [Posicion]
-- crearListaPosiciones [(n,1)] = [(n,1)]
-- crearListaPosiciones [(n,m)] = [(n,m)] ++ crearListaPosiciones [(n,m-1)]

-- 3)

-- moverCabezal
moverCabezal :: Direccion -> Tablero -> Tablero
moverCabezal direccion tablero
    | puedeMoverseCabezal direccion tablero = mapCabezal (movermeUnaDireccion direccion) tablero
    | otherwise = error "El cabezal se cayó del tablero"

-- puedeMoverseCabezal ubicada en punto 5

mapCabezal :: (Posicion -> Posicion) -> Tablero -> Tablero
mapCabezal funcion tablero = tablero { cabezal = (funcion . cabezal) tablero }

dentroDeTablero :: Condicion
dentroDeTablero (Tablero (filas,columnas) _ (x,y)) = (x > 0 && x <= filas) && (y > 0 && y <= columnas)

movermeUnaDireccion :: Direccion -> Posicion -> Posicion
movermeUnaDireccion Norte (x,y) = movermeEnTablero (x,y) (0,1)
movermeUnaDireccion Sur (x,y) = movermeEnTablero (x,y) (0,-1)
movermeUnaDireccion Este (x,y) = movermeEnTablero (x,y) (1,0)
movermeUnaDireccion Oeste (x,y) = movermeEnTablero (x,y) (-1,0)

movermeEnTablero :: Posicion -> Posicion -> Posicion
movermeEnTablero (x,y) (a,b) = (x + a, y + b)

-- ponerBolita
ponerBolita :: Bolita -> Tablero -> Tablero
ponerBolita bolita tablero = accionEnCelda tablero (agregarBolitaEnCelda bolita)

-- sacarBolita
sacarBolita :: Bolita -> Tablero -> Tablero
sacarBolita bolita tablero = accionEnCelda tablero (sacarBolitaDeCelda bolita)

mapBolitas :: ([Bolita] -> [Bolita]) -> Celda -> Celda
mapBolitas funcion celda = celda { bolitas = (funcion . bolitas) celda }

mapCeldas :: ([Celda] -> [Celda]) -> Tablero -> Tablero
mapCeldas funcion tablero = tablero { celdas = (funcion . celdas) tablero }

accionEnCelda :: Tablero -> (Celda -> Celda) -> Tablero
accionEnCelda tablero accion = mapCeldas (map (realizarSiEsCabezal accion (cabezal tablero))) tablero

realizarSiEsCabezal :: (Celda -> Celda) -> Posicion -> Celda -> Celda
realizarSiEsCabezal funcion cabezal celda
    | esCeldaDelCabezal cabezal (posicion celda) = funcion celda
    | otherwise = celda

agregarBolitaEnCelda :: Bolita -> Celda -> Celda
agregarBolitaEnCelda bolita celda = mapBolitas (bolita:) celda

sacarBolitaDeCelda :: Bolita -> Celda -> Celda
sacarBolitaDeCelda bolita celda
    | hayBolitaEnCelda bolita celda = mapBolitas (delete bolita) celda
    | otherwise = error ("No hay bolitas del color" ++ colorBolita bolita ++ "para sacar de la celda actual")

hayBolitaEnCelda :: Bolita -> Celda -> Bool
hayBolitaEnCelda bolita celda = elem bolita (bolitas celda)

esCeldaDelCabezal :: Posicion -> Posicion -> Bool
esCeldaDelCabezal (x,y) (a,b) =  a == x && b == y

colorBolita :: Bolita -> String
colorBolita Azul = " azul "
colorBolita Rojo = " rojo "
colorBolita Verde = " verde "
colorBolita Negro = " negro "

-- 4)

type Sentencia = Tablero -> Tablero
type Condicion = Tablero -> Bool

-- foldl :: (Tablero -> Sentencia -> Tablero) -> Tablero -> [Sentencia] -> Tablero
-- ($) :: (Tablero -> Tablero) -> Tablero -> Tablero
-- flip ($) :: Tablero -> (Tablero -> Tablero) -> Tablero
realizarSentencias :: [Sentencia] -> Tablero -> Tablero
realizarSentencias sentencias tablero = foldl (flip ($)) tablero sentencias

repetir :: [Sentencia] -> Int -> Tablero -> Tablero
repetir sentencias veces tablero  = realizarSentencias (multiplicarSentencias sentencias veces) tablero

multiplicarSentencias :: [Sentencia] -> Int -> [Sentencia]
multiplicarSentencias sentencias veces = concat (replicate veces sentencias)

-- repetir' :: [Sentencia] -> Tablero -> Int -> Tablero
-- repetir' sentencias tablero 0 = tablero
-- repetir' sentencias tablero veces = repetir' sentencias (realizarSentencias sentencias tablero) (veces-1)

alternativa :: Condicion -> [Sentencia] -> [Sentencia] -> Tablero -> Tablero
alternativa condicion sentenciasTrue sentenciasFalse tablero
    | condicion tablero = realizarSentencias sentenciasTrue tablero
    | otherwise = realizarSentencias sentenciasFalse tablero

si :: Condicion -> [Sentencia] -> Tablero -> Tablero
si condicion sentencias tablero = alternativa condicion sentencias [] tablero

siNo :: Condicion -> [Sentencia] -> Tablero -> Tablero
siNo condicion sentencias tablero = alternativa condicion [] sentencias tablero

mientras :: Condicion -> [Sentencia] -> Tablero -> Tablero
mientras condicion sentencias tablero 
    | condicion tablero = mientras condicion sentencias (realizarSentencias sentencias tablero)
    | otherwise =  tablero

irAlBorde :: Direccion -> Tablero -> Tablero
irAlBorde direccion tablero = mientras (puedeMoverseCabezal direccion) [moverCabezal direccion] tablero

-- 5)

puedeMoverseCabezal :: Direccion -> Condicion
puedeMoverseCabezal direccion tablero = dentroDeTablero tablero { cabezal = movermeUnaDireccion direccion (cabezal tablero) }

hayBolita :: Bolita -> Condicion
hayBolita bolita tablero = hayBolitaEnCelda bolita (obtenerCeldaCabezal tablero)

cantidadDeBolitas :: Bolita -> Tablero -> Int
cantidadDeBolitas bolita tablero = (length . (filter (==bolita))) (bolitas (obtenerCeldaCabezal tablero))

obtenerCeldaCabezal :: Tablero -> Celda
obtenerCeldaCabezal tablero = head (filter ((esCeldaDelCabezal (cabezal tablero)).posicion) (celdas tablero))

-- 6)

programa :: Tablero -> [Sentencia] -> Tablero
programa tablero sentencias = realizarSentencias sentencias tablero

-- 7)

punto7 = programa (inicializarTablero (3,3)) [
    moverCabezal Norte,
    ponerBolita Negro,
    ponerBolita Negro,
    ponerBolita Azul,
    moverCabezal Norte,
    repetir [ponerBolita Rojo, ponerBolita Azul] 15,
    alternativa (hayBolita Verde) [moverCabezal Este, ponerBolita Negro] [moverCabezal Sur, moverCabezal Este, ponerBolita Azul],
    moverCabezal Este,
    mientras ((<= 9).(cantidadDeBolitas Verde)) [ponerBolita Verde],
    ponerBolita Azul]