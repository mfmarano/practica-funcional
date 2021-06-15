-- Auxiliares
cambiarDeuda funcion pais = pais { deudaConFMI = (funcion . deudaConFMI) pais }
cambiarSectorPublico funcion pais = pais { poblacionActivaSectorPublico = (funcion . poblacionActivaSectorPublico) pais }
cambiarIngresoPerCapita funcion pais = pais { ingresoPerCapita = (funcion . ingresoPerCapita) pais }
cambiarRecursosNaturales funcion pais = pais { recursosNaturales = (funcion . recursosNaturales) pais }
aplicarPorcentaje total porcentaje = div (porcentaje * total) 100
pbi pais = (ingresoPerCapita pais) * (poblacionActivaSectorPrivado pais + poblacionActivaSectorPublico pais)
-- Auxiliares

-------------
-- Punto 1 --
-------------

-- a)

data Pais = Pais {
    ingresoPerCapita :: Int,
    poblacionActivaSectorPublico :: Int,
    poblacionActivaSectorPrivado :: Int,
    recursosNaturales :: [String],
    deudaConFMI :: Int
} deriving Show

-- b)

namibia = Pais 4140 400000 650000 ["mineria", "ecoturismo"] 50000000

-------------
-- Punto 2 --
-------------

type Receta = [Estrategia]
type Estrategia = Pais -> Pais

prestar :: Int -> Estrategia
prestar n pais = cambiarDeuda (+ (aplicarPorcentaje n 150)) pais

reducirPuestosSectorPublico :: Int -> Estrategia
reducirPuestosSectorPublico puestos = disminuirIngresoPerCapita puestos . cambiarSectorPublico (subtract puestos)

disminuirIngresoPerCapita puestos pais
    | puestos > 100 = cambiarIngresoPerCapita (subtract (aplicarPorcentaje (ingresoPerCapita pais) 20)) pais
    | otherwise = cambiarIngresoPerCapita (subtract (aplicarPorcentaje (ingresoPerCapita pais) 15)) pais

darleRecursoAEmpresa :: String -> Estrategia
darleRecursoAEmpresa recurso = cambiarRecursosNaturales (quitarRecurso recurso) . cambiarDeuda (subtract 2000000)

quitarRecurso recurso = filter (/= recurso)

establecerBlindaje :: Estrategia
establecerBlindaje pais = prestar (div (pbi pais) 2) . reducirPuestosSectorPublico 500 $ pais

-------------
-- Punto 3 --
-------------

-- a)

receta = [prestar 200000000, darleRecursoAEmpresa "mineria"]

-- b)


aplicarReceta :: Pais -> Receta -> Pais
aplicarReceta pais receta = foldl (flip ($)) pais receta

aplicarRecetaANamibia pais receta = aplicarReceta namibia receta

-------------
-- Punto 4 --
-------------

-- a)
puedenZafar :: [Pais] -> [Pais]
puedenZafar = filter (elem "Petroleo" . recursosNaturales)
-- b)
totalDeDeuda :: [Pais] -> Int
totalDeDeuda = sum . map deudaConFMI

-- o tambien

totalDeDeuda' :: [Pais] -> Int
totalDeDeuda' = foldr ((+) . deudaConFMI) 0

-------------
-- Punto 5 --
-------------

estaOrdenadaDePeorAMejor :: Pais -> [Receta] -> Bool
estaOrdenadaDePeorAMejor _ [receta] = True
estaOrdenadaDePeorAMejor pais (receta1 : receta2 : recetas)
    | pbi (aplicarReceta pais receta1) < pbi (aplicarReceta pais receta2) = estaOrdenadaDePeorAMejor pais (receta2 : recetas)
    | otherwise = False

-------------
-- Punto 6 --
-------------
{-
Si un país tiene infinitos recursos naturales, modelado con la función recursosNaturalesInfinitos
a. ¿qué sucede evaluamos la función 4a con ese país?

si evaluamos la funcion puedenZafar con un país con una lista infinita de recursos,
no puede devolver un resultado porque elem "Petroleo" recorreria toda la lista hasta 
encontrar algun elemento que sea "Petroleo", y al ser una lista infinita de "Energia",
la ejecución nunca terminaría

b. ¿y con la 4b?

con la totalDeDeuda funcionaría perfectamente ya que por evaluación diferida nunca se necesita
en esta función evaluar recursosNaturales. por lo que la lista infinita no afectaría en nada

Justifique ambos puntos relacionándolos con algún concepto.
-}

recursosNaturalesInfinitos :: [String]
recursosNaturalesInfinitos = "Energia" : recursosNaturalesInfinitos