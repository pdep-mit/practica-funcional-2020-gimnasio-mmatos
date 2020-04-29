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

estaTonificado = (>5).tonificacion

estaSaludable gimnasta = not (esObeso gimnasta) && estaTonificado gimnasta
estaSaludable' gimnasta = (not.esObeso) gimnasta && estaTonificado gimnasta

---------------------------
-- Punto 2: Quemar calorías
---------------------------


---------------------------
-- Punto 3: Ejercicios
---------------------------