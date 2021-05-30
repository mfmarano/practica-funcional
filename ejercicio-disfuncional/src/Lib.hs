import Text.Show.Functions

tieneNombreLargo mascota = length (fst mascota) > 9

--sumarEnergia (Persona nombre energia edad listaDeIntereses) = Persona nombre (energia + 5) edad listaDeIntereses
--sumarEnergia unaPersona = unaPersona { energia = energia unaPersona + 5 }

triplicarLosPares :: [Int] -> [Int]
triplicarLosPares numeros = map (*3) . filter even $ numeros

esMamifero _ = True
sonTodosMamiferos animales = all esMamifero animales

{-
abrirVentanas :: Casa -> Casa
abrirVentanas = id
prenderEstufa :: Casa -> Casa
type Casa = String
mudarseA :: String -> Casa -> Casa
mudarseA unaDireccion unaCasa = unaCasa
encenderElAireA :: Int -> Casa -> Casa
encenderElAireA unaTemperatura unaCasa = ...

miCasaInteligente = Casa
   { direccion = "Medrano 951",
     temperatura = 26,
     reguladoresDeTemperatura = [
       abrirVentanas,
       prenderEstufa,
       mudarseA "Medrano 952",
       flip encenderElAireA 24
     ]
   }

flip :: (a -> b -> c) -> (b -> a -> c)
-}
esBeatle "Ringo"  = True
esBeatle "John"   = True
esBeatle "George" = True
esBeatle "Paul"   = True
esBeatle _        = False

--sumaDeLasEdadesRecursiva []              = 0
--sumaDeLasEdadesRecursiva (cabeza : cola) =
--   edad cabeza + sumaDeLasEdadesRecursiva cola
--
lista = [1,2,3]

--abrirVentanas casa = casa {
--  temperatura = temperatura casa - 2
--}
--
--abrirVentanas (Casa direccion temperatura reguladores) = Casa direccion (temperatura - 2) reguladoresDeTemperatura

--agregarValor valor indice lista =
--   take (indice - 1) lista ++ valor : drop indice lista
--
--poneleUnNombre numeros = sum (map (*3) (filter even numeros)) < 100
--poneleUnNombre numeros = ((< 100) . sum . map (* 3) . filter even) numeros
--
--poneleUnNombre :: ([Int] -> Bool)
--poneleUnNombre numero = ((< 100) . sum . map (* 3) . filter even) numero

-- Paréntesis
-- Aplicación prefija
-- Composición
-- * /
-- + -
-- && ==
-- $
--