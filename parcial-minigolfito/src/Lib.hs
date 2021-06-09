-- Modelo inicial
data Jugador = Jugador {
    nombre :: String,
    padre :: String,
    habilidad :: Habilidad 
    } deriving (Eq, Show)

data Habilidad = Habilidad {
    fuerzaJugador :: Int,
    precisionJugador :: Int
    } deriving (Eq, Show)

-- Jugadores de ejemplo
bart = Jugador "Bart" "Homero" (Habilidad 25 60)
todd = Jugador "Todd" "Ned" (Habilidad 15 80)
rafa = Jugador "Rafa" "Gorgory" (Habilidad 10 1)

data Tiro = Tiro {
    velocidad :: Int,
    precision :: Int,
    altura :: Int
    } deriving (Eq, Show)

type Puntos = Int

-- Funciones útiles
between n m x = elem x [n .. m]

maximoSegun f = foldl1 (mayorSegun f)

mayorSegun f a b
    | f a > f b = a
    | otherwise = b

-- 1)

    -- a)

type Palo = Habilidad -> Tiro

putter :: Palo
putter (Habilidad _ precision) = Tiro 10 (2*precision) 0

madera :: Palo
madera (Habilidad _ precision) = Tiro 100 (div precision 2) 5

hierro :: Int -> Palo
hierro n (Habilidad fuerza precision) = Tiro (fuerza*n) (div precision n) (max (n-3) 0)

    -- b)

palos :: [Palo]
palos = [putter, madera] ++ map hierro [1..10]

-- 2)