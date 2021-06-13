import Text.Show.Functions

-- Auxiliares
cambiarHabitantes :: ([Personaje] -> [Personaje]) -> Universo -> Universo
cambiarHabitantes funcion universo = universo { habitantes = (funcion . habitantes) universo }

cambiarEnergia :: Int -> Personaje -> Personaje
cambiarEnergia valor personaje = personaje { energia = energia personaje + valor }

cambiarEdad :: (Int -> Int) -> Personaje -> Personaje
cambiarEdad funcion personaje = personaje { edad = (funcion . edad) personaje }

cambiarHabilidades :: ([String] -> [String]) -> Personaje -> Personaje
cambiarHabilidades funcion personaje = personaje { habilidades = (funcion . habilidades) personaje }

cambiarPlaneta :: (String -> String) -> Personaje -> Personaje
cambiarPlaneta funcion personaje = personaje { planeta = (funcion . planeta) personaje }
-- Auxiliares


-------------
-- Punto 1 --
-------------

data Personaje = Personaje {
    edad :: Int,
    energia :: Int,
    habilidades :: [String],
    nombre :: String,
    planeta :: String
} deriving (Show, Eq)

data Guantelete = Guantelete {
    material :: String,
    gemas :: [Gema]
} deriving (Show)

type Gema = Personaje -> Personaje

--data Gema = Alma | Tiempo | Espacio | Mente | Realidad | Poder deriving Show

data Universo = Universo {
    habitantes :: [Personaje]
} deriving (Show)

ironMan = Personaje { edad = 56, energia = 100, habilidades = ["tener un traje metalico", "ser multimillonario"], nombre = "Tony Stark", planeta = "Tierra" }
blackWidow = Personaje { edad = 30, energia = 150, habilidades = ["artes marciales"], nombre = "Natasha Romanoff", planeta = "Tierra" }

chasquido :: Guantelete -> Universo -> Universo
chasquido guantelete universo
    | estaCompleto guantelete = efectoChasquido universo
    | otherwise = universo

estaCompleto :: Guantelete -> Bool
estaCompleto guantelete = ((==6).length.gemas $ guantelete) && ((=="uru").material $ guantelete)

efectoChasquido :: Universo -> Universo
efectoChasquido universo = cambiarHabitantes (take (mitadDeHabitantes universo)) universo

mitadDeHabitantes :: Universo -> Int
mitadDeHabitantes universo = (div (length.habitantes $ universo) 2)

-------------
-- Punto 2 --
-------------

informacionSobreHabitantes :: ([Personaje] -> t) -> Universo -> t
informacionSobreHabitantes funcion universo = funcion (habitantes universo)

aptoParaPendex :: Universo -> Bool
aptoParaPendex universo = informacionSobreHabitantes (any ((<45).edad)) universo

energiaTotal :: Universo -> Int
energiaTotal universo = informacionSobreHabitantes (sum . map energia . filter ((>1).length.habilidades)) universo

-------------
-- Punto 3 --
-------------

-- gema de la Mente
mente :: Int -> Gema
mente valor = cambiarEnergia (-valor)


-- gema del Alma
alma :: String -> Gema
alma habilidad = cambiarEnergia (-10) . eliminarHabilidadSiLaPosee habilidad

eliminarHabilidadSiLaPosee :: String -> Personaje -> Personaje
eliminarHabilidadSiLaPosee habilidad personaje
    | elem habilidad (habilidades personaje) = cambiarHabilidades (filter (/= habilidad)) personaje
    | otherwise = personaje

-- gema del Espacio
espacio :: String -> Gema
espacio planeta = cambiarEnergia (-20) . cambiarPlaneta (const planeta)

-- gema del Poder
poder :: Gema
poder personaje = (vaciarHabilidadesGemaPoder . cambiarEnergia (-(energia personaje))) personaje

vaciarHabilidadesGemaPoder :: Personaje -> Personaje
vaciarHabilidadesGemaPoder personaje 
    | ((<=2).length.habilidades) personaje = cambiarHabilidades (const []) personaje
    | otherwise = personaje 

-- gema del Tiempo
tiempo :: Gema
tiempo = cambiarEnergia (-50) . reducirEdadALaMitad

reducirEdadALaMitad :: Personaje -> Personaje
reducirEdadALaMitad personaje = cambiarEdad (max 18 . subtract (div (edad personaje) 2)) personaje

-- la gema loca
gemaLoca :: Gema -> Gema
gemaLoca gema = gema.gema

-------------
-- Punto 4 --
-------------

guantelete = Guantelete { material = "goma", gemas = [tiempo, alma "usar Mjolnir", gemaLoca (alma "programacion en Haskell")]}

-------------
-- Punto 5 --
-------------

{-
Indicar cómo se produce el “efecto de lado” sobre la víctima. (???)
-}

utilizar :: [Gema] -> Personaje -> Personaje
utilizar gemas personaje = foldl (flip ($)) personaje gemas

{-
    el "efecto de lado" que recibe el personaje seria que se le aplica la primera gema,
    la cual devolvería un nuevo personaje con atributos diferentes.
    asi sucesivamente con las demas gemas que van recibiendo estos nuevos personajes 
    (esto es posible ya que una gema es una función del tipo Personaje -> Personaje)
-}

-------------
-- Punto 6 --
-------------

{-
Resolver utilizando recursividad. Definir la función
gemaMasPoderosa que dado un guantelete y una persona obtiene la gema del infinito que
produce la pérdida más grande de energía sobre la víctima.
-}

gemaMasPoderosa :: Guantelete -> Personaje -> Gema
gemaMasPoderosa guantelete personaje = gemaDeMayorPoder personaje (gemas guantelete)

gemaDeMayorPoder :: Personaje -> [Gema] -> Gema
gemaDeMayorPoder _ [gema] = gema
gemaDeMayorPoder personaje (gema1 : gema2 : gemas)
    | (energia.gema1) personaje > (energia.gema2) personaje = gemaDeMayorPoder personaje (gema2:gemas)
    | otherwise = gemaDeMayorPoder personaje (gema1:gemas)

-------------
-- Punto 7 --
-------------

{-
Dada la función generadora de gemas y un guantelete de locos:
-}
infinitasGemas :: Gema -> [Gema]
infinitasGemas gema = gema:(infinitasGemas gema)

guanteleteDeLocos :: Guantelete
guanteleteDeLocos = Guantelete "vesconite" (infinitasGemas tiempo)

--Y la función

usoLasTresPrimerasGemas :: Guantelete -> Personaje -> Personaje
usoLasTresPrimerasGemas guantelete = (utilizar . take 3. gemas) guantelete

{-
Justifique si se puede ejecutar, relacionándolo con conceptos vistos en la cursada:
-}

-- gemaMasPoderosa guanteleteDeLocos ironMan 

-- la función gemaDeMayorPoder no converge a un valor porque tiene una lista infinita a la cual va a evaluar dentro de la recursividad.
-- entonces no va a poder realizar lo que se pide: "mostrar la gema más poderosa dentro de esta lista infinita de gemas"

-- usoLasTresPrimerasGemas guanteleteDeLocos ironMan

-- sí se puede ejecutar, ya que por evaluación diferida en la función usoLasTresPrimerasGemas 
-- al evaluar el take 3 no necesita de todas las demás infinitas gemas, sino únicamente las primeras 3.