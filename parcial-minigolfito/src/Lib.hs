import Data.List

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

maximoSegun :: Ord b => (a -> b) -> [a] -> a
maximoSegun f = foldl1 (mayorSegun f)

mayorSegun :: Ord x => (t -> x) -> (t -> t -> t)
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

golpe :: Jugador -> Palo -> Tiro
golpe jugador palo = (palo.habilidad) jugador

-- 3)

type Condicion = Tiro -> Bool
type Efecto = Tiro -> Tiro

data Obstaculo = Obstaculo {
    puedeSuperar :: Condicion,
    efectoLuegoDeSuperar :: Efecto
}

tiroDetenido = Tiro 0 0 0

tunelConRampita :: Obstaculo
tunelConRampita = Obstaculo superaTunelConRampita efectoTunelConRampita
superaTunelConRampita :: Condicion
superaTunelConRampita tiro = precision tiro > 90 && alRasDelSuelo tiro
alRasDelSuelo :: Condicion
alRasDelSuelo = (== 0).altura
efectoTunelConRampita :: Efecto
efectoTunelConRampita tiro = Tiro (velocidad tiro * 2) 100 0

laguna :: Int -> Obstaculo
laguna largo = Obstaculo superaLaguna (efectoLaguna largo)
superaLaguna :: Condicion
superaLaguna tiro = velocidad tiro > 80 && (between 1 5.altura) tiro
efectoLaguna :: Int -> Efecto
efectoLaguna largo tiro = tiro { altura = div (altura tiro) largo }

hoyo :: Obstaculo
hoyo = Obstaculo superaHoyo efectoHoyo
superaHoyo :: Condicion
superaHoyo tiro = (between 5 20.velocidad) tiro && alRasDelSuelo tiro && ((>95).precision) tiro
efectoHoyo :: Efecto
efectoHoyo _ = tiroDetenido

intentarSuperarObstaculo :: Obstaculo -> Tiro -> Tiro
intentarSuperarObstaculo obstaculo tiro
    | puedeSuperar obstaculo tiro = efectoLuegoDeSuperar obstaculo tiro
    | otherwise = tiroDetenido

-- 4)

    -- a)

palosUtiles :: Jugador -> Obstaculo -> [Palo]
palosUtiles jugador obstaculo = filter (leSirveParaSuperar jugador obstaculo) palos

leSirveParaSuperar :: Jugador -> Obstaculo -> Palo -> Bool
leSirveParaSuperar jugador obstaculo palo = puedeSuperar obstaculo (golpe jugador palo)

    -- b)

cuantosSePuedenSuperar :: Tiro -> [Obstaculo] -> Int
cuantosSePuedenSuperar tiro [] = 0
cuantosSePuedenSuperar tiro (obstaculo : obstaculos)
    | puedeSuperar obstaculo tiro = 1 + cuantosSePuedenSuperar (efectoLuegoDeSuperar obstaculo tiro) obstaculos
    | otherwise = 0

    -- c)

paloMasUtil :: Jugador -> [Obstaculo] -> Palo
paloMasUtil jugador obstaculos = maximoSegun (flip (cuantosSePuedenSuperar) obstaculos . golpe jugador) palos

-- 5)

padresQuePierden :: [(Jugador, Puntos)] -> [String]
padresQuePierden puntos = map (padre.fst) (niñosPerdedores puntos)

niñosPerdedores :: [(Jugador, Puntos)] -> [(Jugador, Puntos)]
niñosPerdedores puntos = (delete (ganador puntos) puntos)

ganador :: [(Jugador, Puntos)] -> (Jugador, Puntos)
ganador puntos = maximoSegun (snd) puntos

puntos :: [(Jugador, Puntos)]
puntos = [(bart, 10), (todd, 20), (rafa, 15)]

-- otra manera

padresQuePierden' :: [(Jugador, Puntos)] -> [String]
padresQuePierden' puntos = (map (padre.fst) . filter (not . gano puntos)) puntos

gano :: [(Jugador, Puntos)] -> (Jugador, Puntos) -> Bool
gano puntos puntosJugador = (all ((< snd puntosJugador) . snd) . filter (/= puntosJugador)) puntos