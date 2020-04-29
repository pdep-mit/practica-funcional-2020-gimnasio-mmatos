module Library where

---------------------------
-- Precalentamiento
---------------------------

-- Declarar el tipo de dato Gimnasta
data Gimnasta = Gimnasta {
  edad :: Int,
  peso :: Int,
  tonificacion :: Int
} deriving (Show, Eq)

type Ejercicio = Int -> Gimnasta -> Gimnasta

-- Explicitar el tipo de esta función en base al uso esperado:
relax :: Ejercicio
relax minutos gimnasta = gimnasta

-- Declarar la constante gimnastaDePrueba de tipo Gimnasta
-- para usarlo desde las pruebas (Spec.hs) y/o desde la consola
gimnastaDePrueba :: Gimnasta
gimnastaDePrueba = Gimnasta {tonificacion = 3, edad = 30, peso = 75}
gimnastaDePrueba' = Gimnasta 30 75 3

-------------------------------------
-- Punto 1: Gimnastas saludables
-------------------------------------

esObeso :: Gimnasta -> Bool
esObeso gimnasta = peso gimnasta > 100
esObeso' = (> 100).peso
esObeso'' (Gimnasta _ peso _) = peso > 100

estaTonificado :: Gimnasta -> Bool
estaTonificado = (>5).tonificacion

estaSaludable :: Gimnasta -> Bool
estaSaludable gimnasta = not (esObeso gimnasta) && estaTonificado gimnasta
estaSaludable' gimnasta = (not.esObeso) gimnasta && estaTonificado gimnasta

---------------------------
-- Punto 2: Quemar calorías
---------------------------

quemarCalorias :: Int -> Gimnasta -> Gimnasta
quemarCalorias calorias gimnasta
  | esObeso gimnasta = bajarKilos (calorias `div` 150) gimnasta
  | (not.esObeso) gimnasta && edad gimnasta > 30 && calorias > 200 = bajarKilos 1 gimnasta
  | otherwise = bajarKilos (calorias `div` (peso gimnasta * edad gimnasta)) gimnasta

bajarKilos :: Int -> Gimnasta -> Gimnasta
bajarKilos kilosABajar gimnasta = Gimnasta {
  tonificacion = tonificacion gimnasta,
  edad = edad gimnasta,
  peso = peso gimnasta - kilosABajar
  }

bajarKilos' kilosABajar (Gimnasta edadInicial pesoInicial tonificacionInicial)
  = Gimnasta edadInicial (pesoInicial-kilosABajar) tonificacionInicial

bajarKilos'' :: Int -> Gimnasta -> Gimnasta
bajarKilos'' kilosABajar gimnastaOriginal
  = gimnastaOriginal {peso = peso gimnastaOriginal - kilosABajar}

---------------------------
-- Punto 3: Ejercicios
---------------------------
-- type Ejercicio = Int -> Gimnasta -> Gimnasta

type Kilos = Int
pesas :: Kilos -> Ejercicio
pesas kilos minutos gimnasta
  | minutos > 10 = tonificar (kilos `div` 10) gimnasta
  | otherwise = gimnasta

tonificar :: Int -> Gimnasta -> Gimnasta
tonificar tonificacionGanada gimnastaOriginal
  = gimnastaOriginal {tonificacion = tonificacion gimnastaOriginal + tonificacionGanada}

----------
type Inclinacion = Int
colina :: Inclinacion -> Ejercicio
colina inclinacion minutos = quemarCalorias (2*minutos*inclinacion)

montania :: Inclinacion -> Ejercicio
montania inclinacionInicial minutosTotales
  = tonificar 1
      . colina (inclinacionInicial + 3) (minutosTotales `div` 2)
      . colina inclinacionInicial (minutosTotales `div` 2)

-----------

ejercicioEnCinta :: Int -> Ejercicio
ejercicioEnCinta velocidadPromedio minutos = quemarCalorias (velocidadPromedio*minutos)

caminata :: Ejercicio
caminata = ejercicioEnCinta 5

entrenamientoEnCinta :: Ejercicio
entrenamientoEnCinta minutos gimnasta = ejercicioEnCinta velociadPromedio minutos gimnasta
  where
    velocidadInicial = 6
    velocidadFinal = velocidadInicial + minutos `div` 5
    velociadPromedio = (velocidadInicial + velocidadFinal) `div` 2