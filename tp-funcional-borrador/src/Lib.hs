import Data.List

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

-- ejemplos para testear
tablero1 = inicializarTablero (2,2)
tablero2 = Tablero (2,2) (crearCeldas (2,2)) (1,1)
tablero3 = Tablero (1,2) [Celda [Rojo, Verde] (1,1), Celda [Azul, Verde] (1,2)] (1,2)
celda = Celda [Rojo, Verde, Negro] (1,1)

-- 2)

inicializarTablero :: Tamaño -> Tablero
inicializarTablero tamaño = Tablero tamaño (crearCeldas tamaño) (1,1)

-- lo que se nos ocurrió con Fede
-- con composicion
crearCeldas :: Tamaño -> [Celda]
crearCeldas (filas,columnas) = map (Celda []) (concatMap (zip [1..filas] . replicate filas) [1..columnas])

--crearListaCeldas :: [Posicion] -> [Celda]
--crearListaCeldas posiciones = map (Celda []) posiciones

--crearCeldaVacia :: Posicion -> Celda
--crearCeldaVacia posicion = Celda [] posicion

-- sin composición
crearListaPosiciones' :: Tamaño -> [Posicion]
crearListaPosiciones' (filas,columnas) = concatMap (\ n -> zip [1..filas] (replicate filas n)) [1..columnas]

-- lo que se nos ocurrió sin Fede
armarMatriz :: [Tamaño] -> [Posicion]
armarMatriz [(1,m)] = crearListaPosiciones'' [(1,m)]
armarMatriz [(n,m)] = crearListaPosiciones'' [(n,m)] ++ armarMatriz [(n-1,m)]

crearListaPosiciones'' :: [Tamaño] -> [Posicion]
crearListaPosiciones'' [(n,1)] = [(n,1)]
crearListaPosiciones'' [(n,m)] = [(n,m)] ++ crearListaPosiciones'' [(n,m-1)]

-- tablero de (3,3)
-- zip [1,1,1] [1,2,3] ++ zip [2,2,2] [1,2,3] ++ zip [3,3,3] [1,2,3]
-- [(1,1), (1,2), (1,3), (2,1), (2,2), (2,3), (3,1), (3,2), (3,3)]

-- tablero de (2,4)
-- zip [1,1,1,1] [1,2,3,4] ++ zip [2,2,2,2] [1,2,3,4]
-- [(1,1), (1,2), (1,3), (1,4), (2,1), (2,2), (2,3), (2,4)]

-- zip
-- lista1: (replicate (snd tamaño) n)
-- lista2: [1..(snd tamaño)]

-- intento de recursividad
--crearListaPosiciones :: Tamaño -> [Posicion]
--crearListaPosiciones (1,m) = zip (replicate m 1) [1..m]
--crearListaPosiciones (n,m) = zip (replicate m n-1) [1..m]

-- 3)

-- moverCabezal
moverCabezal :: Tablero -> Direccion -> Tablero
moverCabezal tablero direccion
    | dentroDeTablero (movermeUnaDireccion direccion (cabezal tablero)) (tamaño tablero) = tablero { cabezal = movermeUnaDireccion direccion (cabezal tablero) }
    | otherwise = error "El cabezal se cayó del tablero"

dentroDeTablero :: Posicion -> Tamaño -> Bool
dentroDeTablero cabezal tamaño = (fst cabezal > 0 && fst cabezal <= fst tamaño) && (snd cabezal > 0 && snd cabezal <= snd tamaño)

movermeUnaDireccion :: Direccion -> Posicion -> Posicion
movermeUnaDireccion Norte (x,y) = movermeEnTablero (x,y) (0,1)
movermeUnaDireccion Sur (x,y) = movermeEnTablero (x,y) (0,-1)
movermeUnaDireccion Este (x,y) = movermeEnTablero (x,y) (1,0)
movermeUnaDireccion Oeste (x,y) = movermeEnTablero (x,y) (-1,0)

movermeEnTablero :: Posicion -> Posicion -> Posicion
movermeEnTablero (x,y) (a,b) = (x + a, y + b)

-- ponerBolita
ponerBolita :: Bolita -> Tablero -> Tablero 
ponerBolita bolita tablero = tablero { celdas = map (accionar agregarBolitaEnCelda bolita (cabezal tablero)) (celdas tablero) }

agregarBolitaEnCelda :: Bolita -> Celda -> Celda
agregarBolitaEnCelda bolita celda = celda { bolitas = bolita : (bolitas celda) }

-- sacarBolita
sacarBolita :: Bolita -> Tablero -> Tablero
sacarBolita bolita tablero = tablero { celdas = map (accionar sacarBolitaDeCelda bolita (cabezal tablero)) (celdas tablero) }

sacarBolitaDeCelda :: Bolita -> Celda -> Celda
sacarBolitaDeCelda bolita celda
    | any (==bolita) (bolitas celda) = celda { bolitas = delete bolita (bolitas celda) }
    | otherwise = error ("No hay bolitas del color" ++ colorBolita bolita ++ "para sacar de la celda actual")

colorBolita :: Bolita -> String
colorBolita Azul = " azul "
colorBolita Rojo = " rojo "
colorBolita Verde = " verde "
colorBolita Negro = " negro "

accionar :: (Bolita -> Celda -> Celda) -> Bolita -> Posicion -> Celda -> Celda 
accionar funcion bolita cabezal celda
    | esCeldaDelCabezal celda cabezal = funcion bolita celda
    | otherwise = celda

esCeldaDelCabezal :: Celda -> Posicion -> Bool
esCeldaDelCabezal celda (x,y) =  fst (posicion celda) == x && snd (posicion celda) == y