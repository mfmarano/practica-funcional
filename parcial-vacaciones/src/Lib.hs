import Text.Show.Functions

-- Auxiliares
mapCansancio :: Int -> Turista -> Turista
mapCansancio valor turista = turista { cansancio = cansancio turista + valor }

mapStress :: Int -> Turista -> Turista
mapStress valor turista = turista { stress = stress turista + valor }
cambiarStressPorcentual porcentaje turista = mapStress (calcularPorcentaje porcentaje turista) turista

calcularPorcentaje porcentaje turista = div (porcentaje * stress turista) 100

mapIdiomas :: ([String] -> [String]) -> Turista -> Turista
mapIdiomas funcion turista = turista { idiomas = (funcion . idiomas) turista }
aprenderIdioma :: String -> Turista -> Turista
aprenderIdioma idioma = mapIdiomas (idioma:)

mapViajaSolo :: (Bool -> Bool) -> Turista -> Turista
mapViajaSolo funcion turista = turista { viajaSolo = (funcion . viajaSolo) turista }
acompañarTurista :: Turista -> Turista
acompañarTurista = mapViajaSolo (const True)
-- Auxiliares

-------------
-- Punto 1 --
-------------

data Turista = Turista {
    cansancio :: Int,
    stress :: Int,
    viajaSolo :: Bool,
    idiomas :: [String]
} deriving (Show)

ana = Turista 0 21 False ["español"]
beto = Turista 15 15 True ["aleman"]
cathi = Turista 15 15 True ["aleman", "catalan"]

-------------
-- Punto 2 --
-------------

type Excursion = Turista -> Turista

irALaPlaya :: Excursion
irALaPlaya turista 
    | viajaSolo turista = mapCansancio (-5) turista
    | otherwise = mapStress (-1) turista

apreciarElementoDelPaisaje :: String -> Excursion
apreciarElementoDelPaisaje elemento turista = mapStress (- length elemento) turista

salirAHablarUnIdioma :: String -> Excursion
salirAHablarUnIdioma idioma turista = (acompañarTurista . aprenderIdioma idioma) turista

caminar :: Int -> Excursion
caminar minutos turista = (mapCansancio (intensidad minutos) . mapStress (- intensidad minutos)) turista

intensidad minutos = div minutos 4

data Marea = Fuerte | Moderada | Tranquila

paseoEnBarco :: Marea -> Excursion
paseoEnBarco Fuerte turista = (mapStress 6 . mapCansancio 10) turista
paseoEnBarco Moderada turista = turista
paseoEnBarco Tranquila turista = (caminar 10 . apreciarElementoDelPaisaje "mar" . salirAHablarUnIdioma "aleman") turista

-------
-- a --
-------

hacerUnaExcursion :: Excursion -> Turista -> Turista
hacerUnaExcursion excursion turista = (cambiarStressPorcentual (-10) . excursion) turista

-------
-- b --
-------

deltaSegun :: (a -> Int) -> a -> a -> Int
deltaSegun f algo1 algo2 = f algo1 - f algo2

type Indice = Turista -> Int

deltaExcursionSegun :: Indice -> Excursion -> Turista -> Int
deltaExcursionSegun indice excursion turista = deltaSegun indice (hacerUnaExcursion excursion turista) turista

-------
-- c --
-------

-- i

excursionEsEducativa :: Excursion -> Turista -> Bool
excursionEsEducativa excursion turista = deltaExcursionSegun (length.idiomas) excursion turista > 0

-- ii

excursionesDesestresantes :: Turista -> [Excursion] -> [Excursion]
excursionesDesestresantes turista excursiones =  filter ((<=(-3)) . flip (deltaExcursionSegun stress) turista) excursiones

-------------
-- Punto 3 --
-------------

type Tour = [Excursion]

completo :: Tour
completo = [caminar 20, apreciarElementoDelPaisaje "cascada", caminar 40, irALaPlaya, salirAHablarUnIdioma "melmacquiano"]

ladoB :: Excursion -> Tour
ladoB excursion = [paseoEnBarco Tranquila, excursion, caminar 120]

islaVecina :: Marea -> Tour
islaVecina marea = [paseoEnBarco marea, excursionIslaVecina marea, paseoEnBarco marea]

excursionIslaVecina Fuerte = apreciarElementoDelPaisaje "lago"
excursionIslaVecina _ = irALaPlaya

-------
-- a --
-------

hacerUnTour :: Turista -> Tour -> Turista
hacerUnTour turista tour = foldl (flip hacerUnaExcursion) turista (mapStress (length tour) : tour)

-------
-- b --
-------

esConvincente :: [Tour] -> Turista -> Bool
esConvincente tours turista = any (tourConvincente turista) tours

tourConvincente :: Turista -> Tour -> Bool
tourConvincente turista tour = (algunaDejaAcompañado turista . excursionesDesestresantes turista) tour

algunaDejaAcompañado :: Turista -> Tour -> Bool
algunaDejaAcompañado turista tour = any (dejaAcompañado turista) tour

dejaAcompañado :: Turista -> Excursion -> Bool
dejaAcompañado turista excursion = (not . viajaSolo . hacerUnaExcursion excursion) turista

-------
-- c --
-------

efectividad :: Tour -> [Turista] -> Int
efectividad tour turistas = (sum . map (espiritualidadObtenida tour) . (turistasConvencidos tour)) turistas

espiritualidadObtenida :: Tour -> Turista -> Int
espiritualidadObtenida tour turista = ((* (-1)) . deltaEspiritualidad tour) turista

deltaEspiritualidad :: Tour -> Turista -> Int
deltaEspiritualidad tour turista = deltaSegun espiritualidad (hacerUnTour turista tour) turista

espiritualidad :: Turista -> Int
espiritualidad (Turista cansancio stress _ _) = cansancio + stress

turistasConvencidos :: Tour -> [Turista] -> [Turista]
turistasConvencidos tour turistas = filter (flip tourConvincente tour) turistas

-------------
-- Punto 4 --
-------------

-------
-- a --
-------

tourInfinitasPlayas = repeat irALaPlaya

-------
-- b --
-------

-- Para Ana sí se puede saber si es convincente, ya que viaja acomapañada y la primera excursión ya es desestresante.
-- Para Beto no, porque no cumple ninguna de las dos condiciones (siempre viaja solo y su stress no será <= 3) y la función se cuelga

-------
-- c --
-------

-- No se podra saber la efectividad de ese tour excepto para una cantidad nula de turistas, que daría 0.